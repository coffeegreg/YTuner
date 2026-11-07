#!/bin/sh
# Regression tests for the Radio Browser filtering and error handling that the
# audit fixed. Each check here corresponds to a bug that was shipped once:
# without them, nothing stops any of it coming back.
#
#   ./script/test-radiobrowser.sh [path-to-retuner]
#
# A local HTTP server stands in for radio-browser.info, so this needs no
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

PORT=${PORT:-18210}
RB_PORT=${RB_PORT:-18211}
MOCK="$ROOT/script/testdata/mock-radiobrowser.py"
WORK=$(mktemp -d)
PID=""
RB_PID=""
# Every mock this run started, not only the last: a phase that forgets to stop
# one would otherwise leave it for the next run to adopt.
RB_PIDS=""
cleanup() {
  if [ -n "$PID" ]; then kill "$PID" 2>/dev/null || true; wait "$PID" 2>/dev/null || true; fi
  for p in $RB_PIDS; do
    kill "$p" 2>/dev/null || true
    wait "$p" 2>/dev/null || true
  done
  rm -rf "$WORK"
}
trap cleanup EXIT INT TERM PIPE

fail=0
ok()   { echo "  ok   $1"; }
bad()  { echo "  FAIL $1"; fail=1; }

cp "$BIN" "$WORK/viritin"

start_rb() {
  # Per-phase request log. Sharing one file across phases would let an earlier
  # phase's query satisfy a later assertion -- the same class of bug as sharing
  # ports, which has bitten this suite twice.
  RB_LOG="$WORK/rb-requests-$RB_PORT.log"
  export RB_LOG
  : > "$RB_LOG"
  # The readiness probe below cannot tell our mock from one an earlier run
  # abandoned on this port, and next_ports hands out the same sequence every
  # run -- so a leaked mock is inherited by the next run of this suite, serving
  # whatever it was started with. One phase used to leak exactly that way.
  if curl -fsS --noproxy '*' -o /dev/null -m 2 \
       "http://127.0.0.1:$RB_PORT/json/countries" 2>/dev/null; then
    echo "error: something is already serving on port $RB_PORT" >&2
    exit 1
  fi
  python3 "$MOCK" "$RB_PORT" "$1" >/dev/null 2>&1 &
  RB_PID=$!
  RB_PIDS="$RB_PIDS $RB_PID"
  i=0
  while [ "$i" -lt 50 ]; do
    if curl -fsS --noproxy '*' -o /dev/null "http://127.0.0.1:$RB_PORT/json/countries" 2>/dev/null; then
      return 0
    fi
    i=$((i + 1)); sleep 0.2
  done
  echo "error: mock radio-browser did not start on $RB_PORT" >&2; exit 1
}

# Each phase gets its own pair of ports. Sharing one pair means a mock that
# cannot bind leaves the previous phase's server answering, and the run reports
# a confident result for a process that is serving last phase's data.
next_ports() {
  PORT=$((PORT + 2))
  RB_PORT=$((PORT + 1))
}
stop_rb() {
  if [ -n "$RB_PID" ]; then
    kill "$RB_PID" 2>/dev/null || true
    wait "$RB_PID" 2>/dev/null || true
    RB_PID=""
  fi
}

# $1 = BitrateMax, $2 = NotAllowedInName, $3 = NotAllowedInURL
#
# Both files get the same filters, because which one is live depends on
# CommonAVRini and these phases are not about that. The phase at the end of
# this file is: under the shipped default it is avr.ini, and config/<mac>.ini
# is never written or read at all.
MAC=aabbccddee
write_config() {
  mkdir -p "$WORK/config"
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
[MyStations]
Enable=0
[Bookmark]
Enable=0
[RadioBrowser]
Enable=1
RBAPIURL=http://127.0.0.1:$RB_PORT
RBCacheType=catMemStr
RBCacheTTL=0
RBUUIDsCacheTTL=0
INI
  for f in avr "$MAC"; do
    write_avr_ini "$WORK/config/$f.ini" "$1" "$2" "$3"
  done
}

# $1 = path, $2 = BitrateMax, $3 = NotAllowedInName, $4 = NotAllowedInURL
write_avr_ini() {
  cat > "$1" <<INI
[Configuration]
INIVersion=1.0.2
Protocol=
[MainMenu Items]
IdentifiersList=radiobrowser
LabelsList=Radio Browser
[RadioBrowser Filtering]
AllowedCodecs=
NotAllowedCodecs=
BitrateMax=${2:-}
NotAllowedInName=${3:-}
NotAllowedInURL=${4:-}
[RadioBrowser Sorting]
INI
}

start_server() {
  ( cd "$WORK" && exec ./viritin -p "$PORT" > "$1" 2>&1 ) &
  PID=$!
  i=0
  while [ "$i" -lt 60 ]; do
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

stations() {
  curl -fsS --noproxy '*' \
    "http://127.0.0.1:$PORT/viritin/radiobrowser/country/Testland?mac=$MAC" 2>/dev/null || true
}

has() {
  if printf '%s' "$2" | grep -q "$3"; then ok "$1"; else
    bad "$1 (expected '$3')"; printf '       got: %.400s\n' "$2"
  fi
}
hasnt() {
  if printf '%s' "$2" | grep -q "$3"; then
    bad "$1 (did not expect '$3')"; printf '       got: %.400s\n' "$2"
  else ok "$1"; fi
}

echo "Testing Radio Browser handling with $BIN"

# --- the configuration we actually ship ---------------------------------------
# Every phase below writes its own avr.ini, which is exactly how the shipped one
# went unexamined for so long. cfg/avr.ini carried a demonstration filter list -
# AllowedCountries=Poland;Germany;*Britain*;Spain among them - while its own
# comments said the default for each was blank. A new install anywhere outside
# those four countries got "No station(s) found" for its own country and no hint
# why, because the server was doing exactly what it had been told. This phase
# runs the file as shipped, against a station list from a country not on any
# list.
echo "- the configuration we ship does not filter everything out"
next_ports; start_rb ok; write_config "" "" ""
cp "$ROOT/cfg/avr.ini" "$WORK/config/avr.ini"
cp "$ROOT/cfg/avr.ini" "$WORK/config/$MAC.ini"
start_server "$WORK/run-shipped.log"
S=$(stations)
has "a station from a country nobody listed still reaches the AVR" "$S" "<StationName>"
stop_server; stop_rb

# --- the bitrate filter -------------------------------------------------------
# Comparing a JSON number against '' raised "Invalid variant type cast", and the
# handler then freed the JSON array twice on the way out. A station list
# containing a number, a missing field and a string all at once is what the
# filter has to survive.
echo "- BitrateMax filtering"
next_ports; start_rb ok; write_config 192 "" ""
start_server "$WORK/run1.log"
S=$(stations)
has   "the server survives filtering at all"     "$S" "<ListOfItems>"
has   "a station under the limit is kept"        "$S" "Low Bitrate FM"
has   "a station exactly at the limit is kept"   "$S" "Exactly At Limit"
hasnt "a station over the limit is dropped"      "$S" "Over The Limit"
has   "a station with no bitrate field is kept"  "$S" "No Bitrate Field"
has   "a station with a string bitrate is kept"  "$S" "String Bitrate"
hasnt "no variant cast error is logged"          "$(cat "$WORK/run1.log")" "variant"
stop_server
stop_rb

# --- name filtering -----------------------------------------------------------
# NotAllowedInName used to test the URL, so name blocklists did nothing at all
# and URL blocklists silently ran twice. The two stations below are mirror
# images of each other: only a filter reading the right field separates them.
echo "- NotAllowedInName reads the name, not the URL"
next_ports; start_rb ok; write_config "" "aac" ""
start_server "$WORK/run2.log"
S=$(stations)
hasnt "a blocked word in the NAME excludes"      "$S" "Some AAC Station"
has   "the same word in the URL does not"        "$S" "Perfectly Fine"
stop_server
stop_rb

echo "- NotAllowedInURL still reads the URL"
next_ports; start_rb ok; write_config "" "" ".aac"
start_server "$WORK/run3.log"
S=$(stations)
hasnt "a blocked word in the URL excludes"       "$S" "Perfectly Fine"
has   "the same word in the name does not"       "$S" "Some AAC Station"
stop_server
stop_rb

# --- awkward upstream responses ----------------------------------------------
echo "- an empty directory response"
next_ports; start_rb empty; write_config "" "" ""
start_server "$WORK/run4.log"
S=$(stations)
has   "the server still answers"                 "$S" "<ListOfItems>"
hasnt "and does not crash"                       "$(cat "$WORK/run4.log")" "Runtime error"
stop_server
stop_rb

echo "- a malformed directory response"
next_ports; start_rb malformed; write_config "" "" ""
start_server "$WORK/run5.log"
S=$(stations)
has   "the server still answers"                 "$S" "<ListOfItems>"
hasnt "and does not crash"                       "$(cat "$WORK/run5.log")" "Runtime error"
stop_server
stop_rb

echo "- an unreachable directory"
next_ports; write_config "" "" ""
start_server "$WORK/run6.log"
S=$(stations)
has   "the server still answers"                 "$S" "<ListOfItems>"
L=$(cat "$WORK/run6.log")
has   "the error names the URL it requested"     "$L" "127.0.0.1:$RB_PORT/json/"
has   "the error names the cause"                "$L" "Connect to 127.0.0.1"
stop_server

# --- which config file a request actually reads ------------------------------
# CommonAVRini ships as 1, and is True in the code too, so every receiver reads
# config/avr.ini directly and no per-MAC file is ever created. CLAUDE.md
# asserted the opposite unconditionally, which is how a real Denon connecting
# for the first time was expected to leave a config/0005CD350400.ini behind. It
# cannot: nothing writes one in this mode.
#
# Every phase above writes both files, so none of them can tell the two apart.
# These two write one each, in opposite directions, which is the whole point.
echo "- with CommonAVRini on, avr.ini is the file that counts"
next_ports; start_rb ok; write_config "" "" ""
rm -f "$WORK/config/$MAC.ini"
write_avr_ini "$WORK/config/avr.ini" 192
start_server "$WORK/run7.log"
S=$(stations)
hasnt "a filter in avr.ini alone is applied"     "$S" "Over The Limit"
has   "a station under the limit still comes through" "$S" "Low Bitrate FM"
if [ -e "$WORK/config/$MAC.ini" ]; then
  bad "no per-MAC file is created for a served AVR"
else
  ok "no per-MAC file is created for a served AVR"
fi
stop_server; stop_rb

echo "- with CommonAVRini off, config/<mac>.ini is"
next_ports; start_rb ok; write_config "" "" ""
awk '/^\[Configuration\]/ { print; print "CommonAVRini=0"; next } { print }' \
  "$WORK/viritin.ini" > "$WORK/viritin.ini.new"
mv "$WORK/viritin.ini.new" "$WORK/viritin.ini"
write_avr_ini "$WORK/config/$MAC.ini" 192
start_server "$WORK/run8.log"
S=$(stations)
hasnt "a filter in the per-MAC file is applied"  "$S" "Over The Limit"
has   "a station under the limit still comes through" "$S" "Low Bitrate FM"
stop_server; stop_rb

# --- what search asks upstream for -------------------------------------------
# radio-browser matches `name=` as a substring, so a short query matches far
# more than it looks like it should: searching "yle" returns Hardstyle,
# Freestyle and Mylene Farmer. Which of those the AVR shows first is decided
# entirely by the order= in our own query. Alphabetical put "101.ru Mylene
# Farmer" on line one and Yle Radio Suomi somewhere inside 125 pages, which on
# a remote control is indistinguishable from search being broken.
#
# The ordering is invisible in the response, so this asserts on the request the
# mock recorded. Popular already asks for votes descending; search is the
# odd one out.
echo "- search asks upstream for the most-voted stations first"
next_ports; start_rb ok; write_config "" "" ""
start_server "$WORK/run-searchorder.log"
curl -fsS --noproxy '*' -o /dev/null \
  "http://127.0.0.1:$PORT/viritin/search?search=yle&mac=$MAC" 2>/dev/null || true
Q=$(grep '/json/stations/search' "$RB_LOG" 2>/dev/null || true)
has   "search reaches radio-browser at all"      "$Q" "name=yle"
has   "ordered by votes, not alphabetically"     "$Q" "order=votes"
has   "most-voted first"                         "$Q" "reverse=true"
hasnt "no alphabetical ordering is requested"    "$Q" "order=name"
stop_server; stop_rb

if [ "$fail" -ne 0 ]; then
  echo "--- last server log ---" >&2
  cat "$WORK"/run*.log >&2
  exit 1
fi
echo "All Radio Browser checks passed."
