#!/bin/sh
# Start a freshly built Viritin against a throwaway config and check that it
# actually serves the vTuner endpoints an AVR hits first.
#
#   ./script/smoke-test.sh [path-to-viritin]
#
# Radio-browser and the DNS server are switched off so the test stays offline
# and needs no privileged ports.
set -eu

# shellcheck disable=SC1007  # CDPATH= is a deliberate empty assignment for cd
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BIN=${1:-}
if [ -z "$BIN" ]; then
  # shellcheck disable=SC2012  # one glob under bin/, named by the target triple
  BIN=$(ls "$ROOT"/bin/*/viritin "$ROOT"/bin/*/retuner 2>/dev/null | head -1) \
    || { echo "error: no binary found; run script/build.sh first" >&2; exit 1; }
fi
[ -x "$BIN" ] || { echo "error: $BIN is not executable" >&2; exit 1; }

PORT=${PORT:-18100}
WORK=$(mktemp -d)
PID=""
cleanup() {
  if [ -n "$PID" ]; then
    kill "$PID" 2>/dev/null || true
    wait "$PID" 2>/dev/null || true
  fi
  rm -rf "$WORK"
}
trap cleanup EXIT INT TERM

cp "$BIN" "$WORK/viritin"
cat > "$WORK/viritin.ini" <<EOF
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
Enable=0
[Bookmark]
Enable=1
EOF

# exec, so that $! is the server and not the subshell around it.
( cd "$WORK" && exec ./viritin -p "$PORT" > server.log 2>&1 ) &
PID=$!

# Wait for the listener rather than sleeping a fixed amount.
ready=""
i=0
while [ "$i" -lt 50 ]; do
  if curl -fsS --noproxy '*' -o /dev/null \
       "http://127.0.0.1:$PORT/setupapp/x/loginxml.asp?token=0" 2>/dev/null; then
    ready=1; break
  fi
  kill -0 "$PID" 2>/dev/null || { echo "error: server exited during startup" >&2; cat "$WORK/server.log" >&2; exit 1; }
  i=$((i + 1))
  sleep 0.2
done
[ -n "$ready" ] || { echo "error: server did not start listening" >&2; cat "$WORK/server.log" >&2; exit 1; }

fail=0
check() {
  name=$1; url=$2; expect=$3
  body=$(curl -fsS --noproxy '*' "http://127.0.0.1:$PORT$url" 2>/dev/null || true)
  if printf '%s' "$body" | grep -q "$expect"; then
    echo "  ok   $name"
  else
    echo "  FAIL $name (expected '$expect')"
    printf '       got: %.200s\n' "$body"
    fail=1
  fi
}

echo "Smoke testing $BIN on port $PORT"
check "login token handshake" "/setupapp/x/loginxml.asp?token=0"        "EncryptedToken"
check "main menu"             "/setupapp/x/loginxml.asp?mac=aabbccddee" "<ListOfItems>"
check "about page"            "/viritin/about?mac=aabbccddee"            "Welcome to Viritin"
check "empty folder message"  "/viritin/empty?mac=aabbccddee"            "<ItemType>Display</ItemType>"

# Frontier Silicon radios (Hama, Medion, Technisat, Roberts, Pure, Sangean,
# Karcher and the rest) speak the same vTuner protocol as the AVRs, but ask for
# it two path segments deeper: /setupapp/<vendor>/asp/BrowseXML/loginXML.asp
# rather than /setupapp/<vendor>/loginxml.asp. The route wildcard happens to
# span both, which is the only reason those radios work at all -- so it is
# asserted here rather than left to chance. A router change that narrowed the
# wildcard to a single segment would otherwise break every Frontier device
# without breaking a single test.
# An AVR that saved a bookmark before the rename replays the absolute URL it
# stored, which begins /retuner/ or /ytuner/. Those routes are still registered
# for exactly that reason, so the guarantee is asserted rather than assumed.
echo "Bookmarks saved before the renames"
check "the old retuner root path still serves" \
  "/retuner/about?mac=aabbccddee"                                  "Welcome to Viritin"
check "and so do its sub-paths" \
  "/retuner/empty?mac=aabbccddee"                                  "<ItemType>Display</ItemType>"
check "the old ytuner root path still serves" \
  "/ytuner/about?mac=aabbccddee"                                  "Welcome to Viritin"
check "and so do its sub-paths" \
  "/ytuner/empty?mac=aabbccddee"                                  "<ItemType>Display</ItemType>"

echo "Frontier Silicon path shape"
check "deep path token handshake" \
  "/setupapp/karcher/asp/BrowseXML/loginXML.asp?token=0"        "EncryptedToken"
check "deep path main menu" \
  "/setupapp/karcher/asp/BrowseXML/loginXML.asp?mac=aabbccddee" "<ListOfItems>"
check "deep path station lookup" \
  "/setupapp/karcher/asp/BrowseXML/statxml.asp?mac=aabbccddee&id=x" "<ListOfItems>"

# The maintenance service defaulted to 8080 for years, which is also where the
# add-on puts the stations editor: two of this project's own services claiming
# one port, and 8080 is the first port anything else on a home server takes.
# The defaults the binary writes into a fresh ini are checked here against every
# other port this project ships a default for, the add-on's options included.
echo "Default ports"
port_check() {
  if [ "$2" = ok ]; then
    echo "  ok   $1"
  else
    echo "  FAIL $1 ($2)"
    fail=1
  fi
}

maint=$(sed -n 's/^MaintenanceServerPort=\([0-9]*\).*/\1/p' "$WORK/viritin.ini")
if [ -z "$maint" ]; then
  port_check "maintenance default is written to a fresh ini" "no MaintenanceServerPort line"
else
  # Every other default in the generated ini, plus the add-on's own port
  # options - the schema entries below them read "port", not a number, so they
  # do not match.
  other_ports=$(
    grep -E '^[A-Za-z]+Port=[0-9]+' "$WORK/viritin.ini" \
      | grep -v '^MaintenanceServerPort=' | cut -d= -f2
    sed -n 's/^  [a-z_]*port: *\([0-9][0-9]*\) *$/\1/p' "$ROOT/viritin/config.yaml"
  )
  clash=""
  for p in $other_ports; do
    if [ "$p" = "$maint" ]; then clash="$p"; fi
  done
  if [ -n "$clash" ]; then
    port_check "maintenance default is not another service's port" \
      "$maint is already a default elsewhere"
  else
    port_check "maintenance default is not another service's port" ok
  fi

  if [ "$maint" = 8080 ]; then
    port_check "maintenance default avoids 8080" "still 8080"
  else
    port_check "maintenance default avoids 8080" ok
  fi
fi

# --- a station with no logo of its own ----------------------------------------
# A receiver asks for a logo with HEAD before it will GET one, so a 404 ends it
# there: no art, and nothing in the log that looks like a fault. Most
# radio-browser stations carry no favicon, so that was the ordinary case - a
# real Denon issued one icon request in a whole session, took the 404, and never
# asked again. Both verbs are checked because HEAD is the one that decides.
echo "A station with no logo still gets an image"
icon_check() { # $1 = what it is, $2 = curl args
  # shellcheck disable=SC2086  # $2 is a deliberate argument list
  out=$(curl -s --noproxy '*' -o /dev/null -D - $2 \
    "http://127.0.0.1:$PORT/viritin/icon?id=MS_AAAAAAAAAAAA" 2>/dev/null || true)
  code=$(printf '%s' "$out" | sed -n 's|^HTTP/[0-9.]* \([0-9]*\).*|\1|p' | tail -1)
  ctype=$(printf '%s' "$out" | tr -d '\r' | sed -n 's/^[Cc]ontent-[Tt]ype: *//p' | tail -1)
  clen=$(printf '%s' "$out" | tr -d '\r' | sed -n 's/^[Cc]ontent-[Ll]ength: *//p' | tail -1)
  if [ "$code" = 200 ] && [ "$ctype" = "image/jpeg" ] && [ "${clen:-0}" -gt 0 ]; then
    echo "  ok   $1"
  else
    echo "  FAIL $1 (code=$code type=$ctype length=$clen)"
    fail=1
  fi
}
icon_check "GET returns an image, not a 404" ""
icon_check "HEAD answers with a type and a length" "-X HEAD"

if [ "$fail" -ne 0 ]; then
  echo "--- server log ---" >&2
  cat "$WORK/server.log" >&2
  exit 1
fi
echo "All smoke checks passed."
