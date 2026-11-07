#!/bin/sh
# The stations editor is the one service that writes configuration files from a
# browser, so who may reach it and what a wrong password costs are worth
# testing rather than assuming.
#
#   ./script/test-webgui.sh [path-to-retuner]
#
# Needs no network: every request is to a server this script started.
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

PORT=${PORT:-18700}
GUI_PORT=$((PORT + 1))
WORK=$(mktemp -d)
PID=""
cleanup() {
  [ -z "$PID" ] || kill "$PID" 2>/dev/null || true
  rm -rf "$WORK"
}
trap cleanup EXIT INT TERM

fail=0
ok()  { echo "  ok   $1"; }
bad() { echo "  FAIL $1"; fail=1; }

cp "$BIN" "$WORK/viritin"
mkdir -p "$WORK/config"
printf '[Test]\nA Station=http://example.com/a.mp3\n' > "$WORK/config/stations.ini"

# $1 = WebGUIIPAddress, $2 = WebGUIPassword
write_config() {
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
[WebGUI]
Enable=1
WebGUIIPAddress=$1
WebGUIPort=$GUI_PORT
WebGUIUser=admin
WebGUIPassword=$2
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
    kill -0 "$PID" 2>/dev/null || return 1
    i=$((i + 1)); sleep 0.2
  done
  return 1
}
stop_server() {
  [ -z "$PID" ] || kill "$PID" 2>/dev/null || true
  [ -z "$PID" ] || wait "$PID" 2>/dev/null || true
  PID=""
}

# Status code only, so an assertion cannot be fooled by a body. curl writes
# 000 itself when the connection fails, so there is nothing to add on failure -
# an "|| echo 000" here prints it twice and every comparison then misses.
code() { # $1 = url, rest = extra curl args
  url=$1; shift
  curl -s --noproxy '*' -o /dev/null -w '%{http_code}' --max-time 20 "$@" "$url" 2>/dev/null || true
}

# $1 = what it is, $2 = expected code, rest = args to code()
is() {
  what=$1; want=$2; shift 2
  got=$(code "$@")
  if [ "$got" = "$want" ]; then ok "$what"; else bad "$what (wanted $want, got $got)"; fi
}

echo "Testing the stations editor with $BIN"

# --- it will not start without a password ------------------------------------
# The editor writes config files, so an unset password must stop it rather than
# leave it open.
echo "- no password set"
write_config 127.0.0.1 ""
start_server "$WORK/run0.log" || true
sleep 1
if grep -q "no password set" "$WORK/run0.log"; then
  ok "the editor refuses to start and says why"
else
  bad "nothing said the editor was left unstarted"
fi
is "nothing is listening on the editor's port" 000 "http://127.0.0.1:$GUI_PORT/"
stop_server

# --- credentials -------------------------------------------------------------
echo "- credentials"
write_config 127.0.0.1 "correct horse battery"
start_server "$WORK/run1.log" || { echo "server did not start" >&2; cat "$WORK/run1.log" >&2; exit 1; }
is "no credentials is refused" 401 "http://127.0.0.1:$GUI_PORT/"
is "a wrong password is refused" 401 "http://127.0.0.1:$GUI_PORT/" -u 'admin:wrong'
is "a wrong user name is refused" 401 "http://127.0.0.1:$GUI_PORT/" -u 'wrong:correct horse battery'
is "the right credentials are accepted" 200 "http://127.0.0.1:$GUI_PORT/" -u 'admin:correct horse battery'

# A wrong guess has to cost something, or Basic auth is limited only by how
# fast an attacker opens connections.
t0=$(date +%s)
code "http://127.0.0.1:$GUI_PORT/" -u 'admin:wrong' >/dev/null
t1=$(date +%s)
if [ "$((t1 - t0))" -ge 1 ]; then
  ok "a wrong password costs at least a second"
else
  bad "a wrong password was answered immediately"
fi
stop_server

# --- who may reach it --------------------------------------------------------
# The setting matched one exact address before, so a phone on the LAN meant
# opening the editor to every client. A range is the whole point.
echo "- which clients are answered"
write_config "10.99.99.99" "correct horse battery"
start_server "$WORK/run2.log" || { echo "server did not start" >&2; exit 1; }
is "a client outside the setting is refused" 503 "http://127.0.0.1:$GUI_PORT/" -u 'admin:correct horse battery'
stop_server

write_config "127.0.0.0/24" "correct horse battery"
start_server "$WORK/run3.log" || { echo "server did not start" >&2; exit 1; }
is "a client inside a range is answered" 200 "http://127.0.0.1:$GUI_PORT/" -u 'admin:correct horse battery'
stop_server

write_config "10.99.99.0/24" "correct horse battery"
start_server "$WORK/run4.log" || { echo "server did not start" >&2; exit 1; }
is "a client outside a range is refused" 503 "http://127.0.0.1:$GUI_PORT/" -u 'admin:correct horse battery'
stop_server

write_config "10.99.99.99, 127.0.0.1 ,10.0.0.1" "correct horse battery"
start_server "$WORK/run5.log" || { echo "server did not start" >&2; exit 1; }
is "a list is accepted, spaces and all" 200 "http://127.0.0.1:$GUI_PORT/" -u 'admin:correct horse battery'
stop_server

# The old single-address form has to keep working: it is in every existing
# viritin.ini, including the shipped default.
write_config "127.0.0.1" "correct horse battery"
start_server "$WORK/run6.log" || { echo "server did not start" >&2; exit 1; }
is "the single-address form still works" 200 "http://127.0.0.1:$GUI_PORT/" -u 'admin:correct horse battery'
stop_server

write_config "0.0.0.0" "correct horse battery"
start_server "$WORK/run7.log" || { echo "server did not start" >&2; exit 1; }
is "0.0.0.0 still means any client" 200 "http://127.0.0.1:$GUI_PORT/" -u 'admin:correct horse battery'
stop_server

if [ "$fail" -ne 0 ]; then
  echo "--- last server log ---" >&2
  cat "$WORK"/run*.log >&2
  exit 1
fi
echo "All stations editor checks passed."
