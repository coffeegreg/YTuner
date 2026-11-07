#!/bin/sh
# End-to-end test for country presets: fetch, validate, cache, merge, and the
# fallback when the preset repository is unreachable or serving rubbish.
#
#   ./script/test-presets.sh [path-to-retuner]
#
# A local HTTP server stands in for the preset repository, so this needs no
# network access and no privileged ports.
set -eu

# shellcheck disable=SC1007  # CDPATH= is a deliberate empty assignment for cd
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BIN=${1:-}
if [ -z "$BIN" ]; then
  # shellcheck disable=SC2012  # one glob under bin/, named by the target triple
  BIN=$(ls "$ROOT"/bin/*/viritin 2>/dev/null | head -1) \
    || { echo "error: no binary found; run script/build.sh first" >&2; exit 1; }
fi
[ -x "$BIN" ] || { echo "error: $BIN is not executable" >&2; exit 1; }

PORT=${PORT:-18120}
REPO_PORT=${REPO_PORT:-18121}
WORK=$(mktemp -d)
PID=""
REPO_PID=""
cleanup() {
  if [ -n "$PID" ]; then
    kill "$PID" 2>/dev/null || true
    wait "$PID" 2>/dev/null || true
  fi
  if [ -n "$REPO_PID" ]; then
    kill "$REPO_PID" 2>/dev/null || true
    wait "$REPO_PID" 2>/dev/null || true
  fi
  rm -rf "$WORK"
}
trap cleanup EXIT INT TERM

fail=0
say_ok()   { echo "  ok   $1"; }
say_fail() { echo "  FAIL $1"; fail=1; }

mkdir -p "$WORK/repo"
cp "$BIN" "$WORK/viritin"

# The stand-in repository. One of its categories collides with a category in
# the user own file, so the merge is actually exercised.
cat > "$WORK/repo/fi.ini" <<'INI'
[National]
Test National One=http://127.0.0.1:1/one.mp3|http://127.0.0.1:1/one.png
Test National Two=http://127.0.0.1:1/two.mp3

[Shared Category]
Preset Side=http://127.0.0.1:1/preset.mp3
INI
cp "$WORK/repo/fi.ini" "$WORK/repo/fi.ini.good"

start_repo() {
  # localserver.py rather than -m http.server: same static file service, minus
  # the reverse DNS lookup HTTPServer does at bind time, which costs 35 seconds
  # per bind on a macOS runner.
  ( cd "$WORK/repo" && exec python3 "$ROOT/script/testdata/localserver.py" "$REPO_PORT" >/dev/null 2>&1 ) &
  REPO_PID=$!
  i=0
  while [ "$i" -lt 50 ]; do
    if curl -fsS --noproxy '*' -o /dev/null "http://127.0.0.1:$REPO_PORT/fi.ini" 2>/dev/null; then
      return 0
    fi
    i=$((i + 1)); sleep 0.2
  done
  echo "error: stand-in repository did not start" >&2; exit 1
}

stop_repo() {
  if [ -n "$REPO_PID" ]; then
    kill "$REPO_PID" 2>/dev/null || true
    wait "$REPO_PID" 2>/dev/null || true
    REPO_PID=""
  fi
}

cat > "$WORK/viritin.ini" <<INI
[Configuration]
INIVersion=1.2.2
MessageInfoLevel=4
IPAddress=127.0.0.1
[WebServer]
WebServerIPAddress=127.0.0.1
WebServerPort=$PORT
[DNSServer]
Enable=0
[RadioBrowser]
Enable=0
[MyStations]
Enable=1
MyStationsFile=stations.ini
[Presets]
Enable=1
PresetsCountries=fi
PresetsURL=http://127.0.0.1:$REPO_PORT
[Bookmark]
Enable=0
INI

# The user own file. "Shared Category" also exists in the preset; "Mine Only"
# does not.
cat > "$WORK/stations.ini" <<'INI'
[Mine Only]
My Own Station=http://127.0.0.1:1/mine.mp3

[Shared Category]
User Side=http://127.0.0.1:1/user.mp3
INI

start_server() {
  ( cd "$WORK" && exec ./viritin -p "$PORT" > "$1" 2>&1 ) &
  PID=$!
  i=0
  while [ "$i" -lt 50 ]; do
    if curl -fsS --noproxy '*' -o /dev/null \
         "http://127.0.0.1:$PORT/setupapp/x/loginxml.asp?token=0" 2>/dev/null; then
      return 0
    fi
    kill -0 "$PID" 2>/dev/null || { echo "error: server exited during startup" >&2; cat "$1" >&2; exit 1; }
    i=$((i + 1)); sleep 0.2
  done
  echo "error: server did not start listening" >&2; cat "$1" >&2; exit 1
}

stop_server() {
  if [ -n "$PID" ]; then
    kill "$PID" 2>/dev/null || true
    wait "$PID" 2>/dev/null || true
    PID=""
  fi
}

body() { curl -fsS --noproxy '*' "http://127.0.0.1:$PORT$1" 2>/dev/null || true; }

has() {
  if printf '%s' "$2" | grep -q "$3"; then
    say_ok "$1"
  else
    say_fail "$1 (expected '$3')"
    printf '       got: %.300s\n' "$2"
  fi
}

hasnt() {
  if printf '%s' "$2" | grep -q "$3"; then
    say_fail "$1 (did not expect '$3')"
    printf '       got: %.300s\n' "$2"
  else
    say_ok "$1"
  fi
}

echo "Testing presets with $BIN"

echo "- a reachable repository"
start_repo
start_server "$WORK/run1.log"
CATS=$(body "/viritin/mystations?mac=aabbccddee")
has "the preset category is served"          "$CATS" "National"
has "the user own category survives"         "$CATS" "Mine Only"
has "a category present in both appears"     "$CATS" "Shared Category"
if [ "$(printf '%s' "$CATS" | grep -c 'Shared Category')" = "1" ]; then
  say_ok "the shared category is not duplicated"
else
  say_fail "the shared category is not duplicated"
fi
NAT=$(body "/viritin/mystations/National?mac=aabbccddee")
has "a preset station is listed"             "$NAT" "Test National One"
SHARED=$(body "/viritin/mystations/Shared%20Category?mac=aabbccddee")
has "the merged category keeps the user one" "$SHARED" "User Side"
has "the merged category gains the preset"   "$SHARED" "Preset Side"
if [ -f "$WORK/config/presets/fi.ini" ]; then
  say_ok "the fetched list is cached on disk"
else
  say_fail "the fetched list is cached on disk"
fi
stop_server

echo "- an unreachable repository"
stop_repo
start_server "$WORK/run2.log"
CATS=$(body "/viritin/mystations?mac=aabbccddee")
has "the cached list is still served"        "$CATS" "National"
has "the failure names the URL"              "$(cat "$WORK/run2.log")" "127.0.0.1:$REPO_PORT/fi.ini"
stop_server

echo "- a repository serving something that is not a station list"
printf '<!doctype html><html><body>404 Not Found</body></html>' > "$WORK/repo/fi.ini"
start_repo
start_server "$WORK/run3.log"
CATS=$(body "/viritin/mystations?mac=aabbccddee")
has  "the good cached list is kept"          "$CATS" "National"
has  "the rejection is logged"               "$(cat "$WORK/run3.log")" "not a station list"
hasnt "the markup is not served"             "$CATS" "doctype"
stop_server
stop_repo

echo "- a country code that is not one"
cp "$WORK/repo/fi.ini.good" "$WORK/repo/fi.ini"
# -i.bak rather than -i: BSD sed reads the argument after -i as the backup
# suffix, so the bare form consumes the script and then tries to run the file
# name as one. That is one of the reasons this suite has never passed on macOS.
sed -i.bak 's|^PresetsCountries=fi$|PresetsCountries=../../etc,fi|' "$WORK/viritin.ini"
start_server "$WORK/run4.log"
has "the bad code is refused by name"        "$(cat "$WORK/run4.log")" "two-letter code"
if [ -e "$WORK/config/presets/../../etc.ini" ] || [ -e "$WORK/etc.ini" ]; then
  say_fail "a path-traversing code wrote outside the cache"
else
  say_ok "a path-traversing code wrote nothing"
fi
stop_server

if [ "$fail" -ne 0 ]; then
  echo "--- last server log ---" >&2
  cat "$WORK"/run*.log >&2
  exit 1
fi
echo "All preset checks passed."
