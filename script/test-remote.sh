#!/bin/sh
# Tests for the AVR web remote served by the Web GUI.
#
#   ./script/test-remote.sh [path-to-retuner]
#
# A local HTTP server stands in for the receiver, so this needs no network and
# no AVR. What is being tested is the proxy in between: the browser cannot talk
# to the receiver directly (different origin, and the receiver sends no CORS
# headers), so every command goes through Retuner, and Retuner is therefore the
# thing that has to refuse the ones that should not be sent.
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

PORT=${PORT:-18410}
MOCK="$ROOT/script/testdata/mock-denon.py"
WORK=$(mktemp -d)
PID=""
DENON_PID=""
PASSWORD='s3cret'

cleanup() {
  if [ -n "$PID" ]; then kill "$PID" 2>/dev/null || true; fi
  if [ -n "$DENON_PID" ]; then kill "$DENON_PID" 2>/dev/null || true; wait "$DENON_PID" 2>/dev/null || true; fi
  rm -rf "$WORK"
}
trap cleanup EXIT INT TERM PIPE

fail=0
ok()   { echo "  ok   $1"; }
bad()  { echo "  FAIL $1"; fail=1; }

has() {
  if printf '%s' "$2" | grep -q "$3"; then ok "$1"; else
    bad "$1 (expected '$3')"; printf '       got: %.400s\n' "$2"
  fi
}
is() {
  if [ "$2" = "$3" ]; then echo "  ok   $1"
  else echo "  FAIL $1 (want '$3', got '$2')"; fail=1; fi
}
hasnt() {
  if printf '%s' "$2" | grep -q "$3"; then
    bad "$1 (did not expect '$3')"; printf '       got: %.400s\n' "$2"
  else ok "$1"; fi
}

cp "$BIN" "$WORK/viritin"

# Each phase gets its own ports, for the reason recorded in test-radiobrowser.sh:
# a shared pair lets a server that failed to start leave the previous phase
# answering, and the run then reports a confident result about the wrong process.
next_ports() {
  PORT=$((PORT + 3))
  GUI_PORT=$((PORT + 1))
  DENON_PORT=$((PORT + 2))
  DENON_LOG="$WORK/denon-$DENON_PORT.log"
  export DENON_LOG
  : > "$DENON_LOG"
}

# $1 = which screen the receiver is showing: menu (default), search, playing
start_denon() {
  # A receiver left over from an interrupted run would answer the readiness
  # probe below and be mistaken for this phase's for the rest of the run.
  if curl -fsS --noproxy '*' -o /dev/null -m 2 \
       "http://127.0.0.1:$DENON_PORT/goform/formNetAudio_StatusXml.xml" 2>/dev/null; then
    echo "error: something is already serving on port $DENON_PORT" >&2
    exit 1
  fi
  python3 "$MOCK" "$DENON_PORT" ${1:+"$1"} >/dev/null 2>&1 &
  DENON_PID=$!
  i=0
  while [ "$i" -lt 50 ]; do
    if curl -fsS --noproxy '*' -o /dev/null \
      "http://127.0.0.1:$DENON_PORT/goform/formNetAudio_StatusXml.xml" 2>/dev/null; then
      return 0
    fi
    i=$((i + 1)); sleep 0.2
  done
  echo "error: stand-in receiver did not start on $DENON_PORT" >&2; exit 1
}
stop_denon() {
  if [ -n "$DENON_PID" ]; then
    kill "$DENON_PID" 2>/dev/null || true
    wait "$DENON_PID" 2>/dev/null || true
    DENON_PID=""
  fi
}

# $1 = RemoteAVRAddress value
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
Enable=0
[WebGUI]
Enable=1
WebGUIIPAddress=127.0.0.1
WebGUIPort=$GUI_PORT
WebGUIUser=admin
WebGUIPassword=$PASSWORD
RemoteAVRAddress=$1
INI
}

start_server() {
  ( cd "$WORK" && exec ./viritin -p "$PORT" >"$1" 2>&1 ) &
  PID=$!
  i=0
  while [ "$i" -lt 50 ]; do
    if curl -fsS --noproxy '*' -u "admin:$PASSWORD" -o /dev/null \
      "http://127.0.0.1:$GUI_PORT/api/status" 2>/dev/null; then
      return 0
    fi
    i=$((i + 1)); sleep 0.2
  done
  echo "error: viritin web gui did not start on $GUI_PORT" >&2
  cat "$1" >&2
  exit 1
}
stop_server() {
  if [ -n "$PID" ]; then
    kill "$PID" 2>/dev/null || true
    wait "$PID" 2>/dev/null || true
    PID=""
  fi
}

gui()      { curl -fsS --noproxy '*' -u "admin:$PASSWORD" "$@" 2>/dev/null || true; }
gui_code() { curl -sS  --noproxy '*' -o /dev/null -w '%{http_code}' "$@" 2>/dev/null || true; }
post()     {
  curl -sS --noproxy '*' -u "admin:$PASSWORD" -H 'Content-Type: application/json' \
    -X POST --data "$2" "http://127.0.0.1:$GUI_PORT$1" 2>/dev/null || true
}

echo "Testing the AVR web remote with $BIN"

# --- the page and the status proxy --------------------------------------------
echo "- the remote is served and reports what the receiver is showing"
next_ports; write_config "127.0.0.1:$DENON_PORT"; start_denon
start_server "$WORK/run-remote.log"

C=$(gui_code "http://127.0.0.1:$GUI_PORT/remote")
has "an unauthenticated request is refused" "$C" "401"

P=$(gui "http://127.0.0.1:$GUI_PORT/remote")
has "the page is served to an authenticated client" "$P" "viritin-remote"

S=$(gui "http://127.0.0.1:$GUI_PORT/api/remote/status")
has "the list the receiver is showing comes through" "$S" "Radio Browser"
has "the cursor flag is reported"                    "$S" '"flag"'
# szLine is always ten entries and one of them is a page counter, not an item.
# A remote that treats "every non-empty line" as the list offers "[ 1/7 ]" as a
# station you can select, which does nothing and looks broken.
hasnt "the page counter is not offered as a list item" "$S" '1/7 *\\?",'
has  "the page counter is reported separately"        "$S" '"page"'
hasnt "empty padding lines are dropped"               "$S" '"text":""'

# --- commands ------------------------------------------------------------------
echo "- cursor commands reach the receiver"
: > "$DENON_LOG"
R=$(post "/api/remote/cmd" '{"cmd":"CurDown"}')
has "the command is accepted"            "$R" '"ok":true'
has "and arrives at the receiver"        "$(cat "$DENON_LOG")" "PutNetAudioCommand%2FCurDown"
has "posted to the right endpoint"       "$(cat "$DENON_LOG")" "/NetAudio/index.put.asp"

# --- what the proxy must refuse ------------------------------------------------
# The command arrives from a browser, so it is input like any other. Without an
# allowlist the remote is a general-purpose way to POST anything at all to the
# receiver: PutZone_InputFunction switches the whole amplifier's source, and a
# Z2 source command powers on zone 2, which is a pair of speakers outdoors.
echo "- the proxy refuses commands that are not on the list"
: > "$DENON_LOG"
R=$(post "/api/remote/cmd" '{"cmd":"PutZone_InputFunction/NET"}')
hasnt "a zone command is not accepted"        "$R" '"ok":true'
hasnt "and never reaches the receiver"        "$(cat "$DENON_LOG")" "PutZone_InputFunction"

: > "$DENON_LOG"
R=$(post "/api/remote/cmd" '{"cmd":"CurDown&cmd1=PutZone_InputFunction/NET"}')
hasnt "a smuggled second command is not accepted" "$R" '"ok":true'
hasnt "and never reaches the receiver"            "$(cat "$DENON_LOG")" "PutZone_InputFunction"

# --- search --------------------------------------------------------------------
echo "- keyword search reaches the receiver"
: > "$DENON_LOG"
R=$(post "/api/remote/search" '{"q":"yle"}')
has "the search is accepted"        "$R" '"ok":true'
has "and arrives as an iRadio search" "$(cat "$DENON_LOG")" "PutNetFuncSearchiRadio%2Fyle"

: > "$DENON_LOG"
R=$(post "/api/remote/search" '{"q":"rock & roll"}')
has "a keyword with punctuation is encoded, not passed raw" "$(cat "$DENON_LOG")" "rock%20%26%20roll"

stop_server; stop_denon

# --- the receiver being unreachable --------------------------------------------
echo "- an unreachable receiver"
next_ports; write_config "127.0.0.1:$DENON_PORT"   # nothing listening there
start_server "$WORK/run-unreachable.log"
S=$(gui "http://127.0.0.1:$GUI_PORT/api/remote/status")
hasnt "status does not claim success"  "$S" '"ok":true'
has   "the failure is reported"        "$S" '"error"'
C=$(gui_code -u "admin:$PASSWORD" "http://127.0.0.1:$GUI_PORT/remote")
has   "the page is still served"       "$C" "200"
stop_server

# --- not configured -------------------------------------------------------------
echo "- no receiver address configured"
next_ports; write_config ""
start_server "$WORK/run-unset.log"
S=$(gui "http://127.0.0.1:$GUI_PORT/api/remote/status")
has "status says the address is not set" "$S" '"error"'
stop_server

# --- which line the cursor is on ---------------------------------------------
# chFlag is a bitmask. A real Denon showing search results sends 0 for the
# heading, 1 for each selectable station and 9 for the one under the cursor, so
# a remote testing chFlag = 8 highlights nothing on any list whose items are
# selectable. The remote worked and looked like it did not.
echo "- the cursor is found in a flag that carries other bits too"
next_ports; start_denon search; write_config "127.0.0.1:$DENON_PORT"
start_server "$WORK/run-cursor.log"
S=$(gui "http://127.0.0.1:$GUI_PORT/api/remote/status")
has   "the station under the cursor is marked"  "$S" '"text" : "Radio Dei Helsinki", "flag" : 9, "cursor" : true'
has   "a station beside it is not"              "$S" '"text" : "Radio Helsinki", "flag" : 1, "cursor" : false'
has   "and is still reported as selectable"     "$S" '"flag" : 1, "cursor" : false, "selectable" : true'
hasnt "the heading is not selectable"           "$S" '"text" : "Search by Keyword", "flag" : 0, "cursor" : false, "selectable" : true'
stop_server; stop_denon

# --- clicking a name ----------------------------------------------------------
# The distance from the cursor to the line you clicked, in cursor keys, then
# Enter. The mock moves its own cursor when it is sent one, so this asks where
# the cursor ended up rather than counting presses and assuming they landed.
echo "- clicking a line below the cursor walks down to it and enters"
next_ports; start_denon search; write_config "127.0.0.1:$DENON_PORT"
start_server "$WORK/run-select.log"
R=$(post "/api/remote/select" '{"index":4}')
has  "the click is accepted"                  "$R" '"ok":true'
S=$(gui "http://127.0.0.1:$GUI_PORT/api/remote/status")
has  "the cursor is on the line that was clicked" "$S" '"text" : "Radio Helsinki 92,6 MHz (160 kbps)", "flag" : 9'
is   "it took exactly three steps down" \
     "$(grep -c 'PutNetAudioCommand%2FCurDown' "$DENON_LOG")" "3"
is   "and none up"  "$(grep -c 'PutNetAudioCommand%2FCurUp' "$DENON_LOG")" "0"
is   "then one Enter" "$(grep -c 'PutNetAudioCommand%2FCurEnter' "$DENON_LOG")" "1"
stop_server; stop_denon

echo "- clicking a line above the cursor walks up to it"
next_ports; start_denon search; write_config "127.0.0.1:$DENON_PORT"
start_server "$WORK/run-selectup.log"
post "/api/remote/select" '{"index":5}' >/dev/null
post "/api/remote/select" '{"index":2}' >/dev/null
S=$(gui "http://127.0.0.1:$GUI_PORT/api/remote/status")
has  "the cursor came back up"  "$S" '"index" : 2, "text" : "Radio Helsinki", "flag" : 9'
is   "three up presses"  "$(grep -c 'PutNetAudioCommand%2FCurUp' "$DENON_LOG")" "3"
stop_server; stop_denon

echo "- clicking the line the cursor is already on only presses Enter"
next_ports; start_denon search; write_config "127.0.0.1:$DENON_PORT"
start_server "$WORK/run-selectsame.log"
R=$(post "/api/remote/select" '{"index":1}')
has  "accepted"                    "$R" '"ok":true'
is   "nothing moved"               "$(grep -c 'CurDown\|CurUp' "$DENON_LOG")" "0"
is   "and it was entered"          "$(grep -c 'PutNetAudioCommand%2FCurEnter' "$DENON_LOG")" "1"
stop_server; stop_denon

# --- when the cursor cannot be found -----------------------------------------
# Now Playing reports every flag as 0. Stepping from a position we do not know
# would move the selection somewhere nobody asked for, on a screen where Enter
# might start playing whatever it lands on.
echo "- a screen with no cursor is left alone"
next_ports; start_denon playing; write_config "127.0.0.1:$DENON_PORT"
start_server "$WORK/run-nocursor.log"
R=$(post "/api/remote/select" '{"index":2}')
hasnt "the click is refused"       "$R" '"ok":true'
has   "and says why"               "$R" "not showing a list"
is    "nothing was sent at all"    "$(grep -c 'PutNetAudioCommand' "$DENON_LOG")" "0"
stop_server; stop_denon

echo "- a line that is not on the screen is refused"
next_ports; start_denon search; write_config "127.0.0.1:$DENON_PORT"
start_server "$WORK/run-range.log"
R=$(post "/api/remote/select" '{"index":99}')
hasnt "out of range is refused"    "$R" '"ok":true'
R=$(post "/api/remote/select" '{"index":-1}')
hasnt "so is a negative index"     "$R" '"ok":true'
R=$(post "/api/remote/select" '{"index":"4"}')
hasnt "and so is an index that is not a number" "$R" '"ok":true'
is    "none of them reached the receiver" "$(grep -c 'PutNetAudioCommand' "$DENON_LOG")" "0"
stop_server; stop_denon

if [ "$fail" -ne 0 ]; then
  echo "--- last server log ---" >&2
  cat "$WORK"/run*.log >&2
  exit 1
fi
echo "All remote checks passed."
