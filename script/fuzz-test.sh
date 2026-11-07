#!/bin/sh
# Fires malformed input at a running Retuner and checks it is still there
# afterwards.
#
#   CHECKED=1 ./script/build.sh && ./script/fuzz-test.sh
#
# Two surfaces take bytes from outside and hand them to code that parses:
#
#   * the DNS server, whose query name is walked by hand with no bound from the
#     length of the packet, and
#   * GetIcon, which downloads a station logo from wherever the directory says
#     and decodes it in process.
#
# Neither is reachable only from a friendly LAN. A station logo URL is chosen by
# whoever submitted the station to radio-browser, and the DNS port is open to
# anything that can send a datagram.
#
# Run it against a CHECKED=1 build: -Cr turns a read past the end of a buffer
# into a runtime error the process reports, instead of a value that quietly
# means nothing. Against a release build this still passes or fails, it just
# sees less.
#
# Each phase gets its own directory, its own ports, and its own binary, passed
# in as parameters - see the note in script/test-leaks.sh for what sharing them
# cost the last time.
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

PORT=${PORT:-19000}
WORK=$(mktemp -d)
SERVER_PID=""
MOCK_PID=""
fail=0

# Every server this run started, not just the current one. A phase that ends
# early - an exit, an interrupt, a SIGPIPE from a pipeline reading the output -
# used to leave its server alive holding a port, and the next run of the suite
# then talked to it instead of its own.
STARTED_PIDS=""
# kill and then wait: kill only asks. Without the wait the script exits while
# the children are still going, and a CI runner then reports them as orphans it
# had to terminate -- which is the same shape as the stray server above, one
# step later.
cleanup() {
  for p in $STARTED_PIDS; do kill "$p" 2>/dev/null || true; done
  [ -z "$MOCK_PID" ] || kill "$MOCK_PID" 2>/dev/null || true
  for p in $STARTED_PIDS; do wait "$p" 2>/dev/null || true; done
  [ -z "$MOCK_PID" ] || wait "$MOCK_PID" 2>/dev/null || true
  rm -rf "$WORK"
}
trap cleanup EXIT INT TERM

ok()   { echo "  ok   $1"; }
bad()  { echo "  FAIL $1"; fail=1; }

# $1 = directory, $2 = web port. Leaves the pid in SERVER_PID.
start_server() {
  # Nothing may already be answering here. A stray server from an interrupted
  # run holding this port has cost two debugging sessions: it answers every
  # request the readiness check makes, so the suite adopts it and reports on a
  # process it did not start. The symptom surfaced phases later as "the process
  # died decoding a logo".
  #
  # Checking before the launch rather than after is what makes this reliable.
  # Retuner logs "Web Service: listening on" *before* the bind succeeds - with
  # the port taken you get that line and then "Binding of socket failed" - so
  # neither the log nor the socket can tell you whose server answered, and
  # polling either one races the squatter.
  if curl -fsS --noproxy '*' -o /dev/null -m 2 \
       "http://127.0.0.1:$2/setupapp/x/loginxml.asp?token=0" 2>/dev/null; then
    echo "error: something is already serving on port $2" >&2
    echo "       probably a server left behind by an interrupted run:" >&2
    echo "       pkill -f 'viritin' and try again" >&2
    exit 1
  fi

  cp "$BIN" "$1/viritin"
  ( cd "$1" && exec ./viritin -p "$PORT" > server.log 2>&1 ) &
  SERVER_PID=$!
  STARTED_PIDS="$STARTED_PIDS $SERVER_PID"
  i=0
  while [ "$i" -lt 60 ]; do
    # Backstop, named with the port so another service failing to bind its own
    # is not mistaken for this one.
    if grep -q "Binding of socket failed: $2" "$1/server.log" 2>/dev/null; then
      echo "error: our server could not bind port $2" >&2
      cat "$1/server.log" >&2
      exit 1
    fi
    if curl -fsS --noproxy '*' -o /dev/null \
         "http://127.0.0.1:$2/setupapp/x/loginxml.asp?token=0" 2>/dev/null; then
      return 0
    fi
    kill -0 "$SERVER_PID" 2>/dev/null \
      || { echo "error: server exited during startup" >&2; cat "$1/server.log" >&2; exit 1; }
    i=$((i + 1))
    sleep 0.2
  done
  echo "error: server did not start listening" >&2
  cat "$1/server.log" >&2
  exit 1
}

# Peak resident size in kB, which is what a decoder that believes a header
# leaves behind even after the allocation is released.
peak_rss() {
  awk '/^VmHWM:/ { print $2 }' "/proc/$1/status" 2>/dev/null || echo 0
}

# $1 = web port, $2 = DNS port
#
# DNSServers is deliberately empty. Every one of the random names below is a
# name nothing intercepts, and with a resolver configured each one is forwarded
# upstream - on the same thread that answers queries, with Indy's WaitingTime of
# 5000 ms per entry in RootDNS_NET. Two entries that do not answer is ten
# seconds in which the DNS service answers nothing at all, intercepted names
# included. That is how this went red in CI on a change that touched no Pascal:
# 8.8.8.8 declined to keep up with a burst of nonsense lookups, the backlog was
# still draining when the intercept check began, and two names in the middle of
# the list timed out. An empty list means Count=0, and forwarding returns at
# once - which is also what makes this phase offline, as the suite promises.
fuzz_dns() {
  run=$WORK/dns
  mkdir -p "$run"
  cat > "$run/viritin.ini" <<INI
[Configuration]
INIVersion=1.2.2
MessageInfoLevel=4
IPAddress=127.0.0.1
[WebServer]
WebServerIPAddress=127.0.0.1
WebServerPort=$1
[DNSServer]
Enable=1
DNSServerIPAddress=127.0.0.1
DNSServerPort=$2
DNSAdvertiseIP=127.0.0.1
DNSServers=
[RadioBrowser]
Enable=0
[MyStations]
Enable=0
INI
  start_server "$run" "$1"
  echo "Malformed DNS packets on port $2"
  if python3 "$ROOT/script/testdata/fuzz-dns.py" "$2"; then
    ok "the DNS server answers a real query after every malformed one"
  else
    bad "a malformed packet stopped the DNS server answering"
  fi
  if kill -0 "$SERVER_PID" 2>/dev/null; then
    ok "the process is still running"
  else
    bad "the process died"
    sed -n '$p' "$run/server.log" >&2
  fi

  # Not fuzzing, but this is the one phase with a DNS server running: every
  # name the shipped configuration claims to intercept has to actually be
  # answered here, as a subdomain and as the apex, and a name nobody intercepts
  # must not be.
  echo "The intercept list does what it says"
  patterns=$(sed -n 's/^InterceptDNs=//p' "$ROOT/cfg/viritin.ini" | head -1)
  if [ -z "$patterns" ]; then
    bad "no InterceptDNs line found in cfg/viritin.ini"
  elif python3 "$ROOT/script/testdata/dns-intercepts.py" "$2" 127.0.0.1 "$patterns"; then
    :
  else
    fail=1
  fi
  # The appliance guide asks the reader to write the same set into dnsmasq, and
  # a domain added to one list and not the other is a manufacturer that quietly
  # stops working with nothing anywhere to say why.
  grep -o '^address=/[^/]*/' "$ROOT/doc/APPLIANCE.md" \
    | sed 's|address=/||; s|/$||' | sort > "$run/doc-domains"
  printf '%s' "$patterns" | tr ',' '\n' | sed 's/^\*\.//' | sort > "$run/ini-domains"
  if cmp -s "$run/doc-domains" "$run/ini-domains"; then
    ok "doc/APPLIANCE.md lists exactly the shipped intercept set"
  else
    bad "doc/APPLIANCE.md and cfg/viritin.ini disagree about what to intercept"
    diff "$run/ini-domains" "$run/doc-domains" | sed 's/^/       /' || true
  fi

  # Every intercepted query is logged with the name as the hand-written walk
  # made of it, so the awkward names appearing there is the evidence that they
  # got that far. Without this a green run could just mean Indy dropped
  # everything before any of this project's code saw it - which is exactly what
  # happens to the malformed group, and worth knowing rather than assuming.
  reached=$(grep -a 'DNS Query intercept' "$run/server.log" \
    | grep -avc 'radio\.vtuner\.com' || true)
  if [ "${reached:-0}" -ge 8 ]; then
    ok "awkward names reached the name walk itself ($reached of them)"
  else
    bad "only $reached awkward names reached the name walk - this proved little"
  fi
  if curl -fsS --noproxy '*' -o /dev/null \
       "http://127.0.0.1:$1/setupapp/x/loginxml.asp?mac=aabbccddee" 2>/dev/null; then
    ok "the web service still serves the AVR"
  else
    bad "the web service stopped answering"
  fi
  kill "$SERVER_PID" 2>/dev/null || true
  wait "$SERVER_PID" 2>/dev/null || true
  SERVER_PID=""
}

# $1 = web port, $2 = mock image server port
fuzz_images() {
  run=$WORK/images
  mkdir -p "$run/cache"
  python3 "$ROOT/script/testdata/mock-images.py" "$2" &
  MOCK_PID=$!
  i=0
  while [ "$i" -lt 50 ]; do
    curl -fsS --noproxy '*' -o "$WORK/catalogue" \
      "http://127.0.0.1:$2/_catalogue" 2>/dev/null && break
    i=$((i + 1))
    sleep 0.2
  done
  [ -s "$WORK/catalogue" ] || { echo "error: mock image server did not start" >&2; exit 1; }

  {
    echo "[Fuzz]"
    while read -r path; do
      echo "${path#/}=http://127.0.0.1:$2/stream.mp3|http://127.0.0.1:$2$path"
    done < "$WORK/catalogue"
    echo "loop.png=http://127.0.0.1:$2/stream.mp3|http://127.0.0.1:$2/loop.png"
  } > "$run/stations.ini"

  cat > "$run/viritin.ini" <<INI
[Configuration]
INIVersion=1.2.2
MessageInfoLevel=1
IPAddress=127.0.0.1
IconCache=1
[WebServer]
WebServerIPAddress=127.0.0.1
WebServerPort=$1
[DNSServer]
Enable=0
[RadioBrowser]
Enable=0
[MyStations]
Enable=1
MyStationsFile=stations.ini
INI
  start_server "$run" "$1"
  echo "Hostile station logos on port $1"

  curl -fsS --noproxy '*' -o "$WORK/stations.xml" \
    "http://127.0.0.1:$1/viritin/mystations/Fuzz?mac=aabbccddee" 2>/dev/null || true
  tr '<' '\n' < "$WORK/stations.xml" \
    | sed -n 's/.*[?&]id=\(MS_[0-9A-F]*\).*/\1/p' | sort -u > "$WORK/icon-ids"
  count=$(wc -l < "$WORK/icon-ids" | tr -d ' ')
  wanted=$(($(wc -l < "$WORK/catalogue") + 1))
  if [ "$count" -eq "$wanted" ]; then
    ok "every hostile logo is reachable as an icon ($count of them)"
  else
    bad "expected $wanted station ids, found $count"
  fi

  while read -r id; do
    curl -s --noproxy '*' --max-time 20 -o /dev/null \
      "http://127.0.0.1:$1/viritin/icon?id=$id" || true
  done < "$WORK/icon-ids"

  if kill -0 "$SERVER_PID" 2>/dev/null; then
    ok "the process survived every one of them"
  else
    bad "the process died decoding a logo"
    tail -3 "$run/server.log" >&2
  fi

  # A header is a claim, not a measurement. 300 MB of resident memory is far
  # more than any station logo needs and far less than one crafted header can
  # ask for.
  rss=$(peak_rss "$SERVER_PID")
  if ! kill -0 "$SERVER_PID" 2>/dev/null; then
    bad "peak memory could not be read - the process is gone"
  elif [ "${rss:-0}" -lt 307200 ]; then
    ok "peak memory stayed under 300 MB (${rss} kB)"
  else
    bad "peak memory reached ${rss} kB - a declared image size was believed"
  fi

  # The control: if this one stops working, the phase above proves nothing.
  valid_id=$(tr '<' '\n' < "$WORK/stations.xml" \
    | grep -A10 'StationName>valid.png' \
    | sed -n 's/.*[?&]id=\(MS_[0-9A-F]*\).*/\1/p' | head -1)
  if [ -n "$valid_id" ] \
    && curl -fsS --noproxy '*' -o "$WORK/valid.out" \
         "http://127.0.0.1:$1/viritin/icon?id=$valid_id" 2>/dev/null \
    && [ -s "$WORK/valid.out" ]; then
    ok "a well-formed logo is still converted and served"
  else
    bad "the one valid logo did not come back"
  fi

  kill "$MOCK_PID" 2>/dev/null || true
  MOCK_PID=""
}

# $1 = web port. The icon id is used as a file name under the cache directory,
# so it decides what is read and what is written.
hostile_ids() {
  run=$WORK/keys
  mkdir -p "$run/cache"
  printf 'SECRET-MARKER\n' > "$run/secret.txt"
  printf 'SECRET-MARKER\n' > "$run/cache/MS_AAAAAAAAAAAA"
  cat > "$run/viritin.ini" <<INI
[Configuration]
INIVersion=1.2.2
MessageInfoLevel=1
IPAddress=127.0.0.1
IconCache=1
[WebServer]
WebServerIPAddress=127.0.0.1
WebServerPort=$1
[DNSServer]
Enable=0
[RadioBrowser]
Enable=0
[MyStations]
Enable=0
INI
  start_server "$run" "$1"
  echo "Icon ids that are not ids"
  for id in '../secret.txt' '..%2Fsecret.txt' '....//secret.txt' \
            '/etc/hostname' '%2Fetc%2Fhostname' '../../../../etc/hostname' \
            'MS_AAAAAAAAAAAA/../../secret.txt' '.%2E/secret.txt'; do
    body=$(curl -s --noproxy '*' --max-time 10 \
      "http://127.0.0.1:$1/viritin/icon?id=$id" 2>/dev/null || true)
    case $body in
      *SECRET-MARKER*|*localhost*)
        bad "id '$id' read a file outside the cache" ;;
      *)
        ok "id '$id' read nothing" ;;
    esac
  done
  # The cache itself is still allowed to answer: the fix must not turn the
  # cache off, only keep the key inside it.
  body=$(curl -s --noproxy '*' "http://127.0.0.1:$1/viritin/icon?id=MS_AAAAAAAAAAAA" 2>/dev/null || true)
  case $body in
    *SECRET-MARKER*) ok "an ordinary id still reads its own cache entry" ;;
    *) bad "the cache stopped answering for an ordinary name" ;;
  esac
  kill "$SERVER_PID" 2>/dev/null || true
  wait "$SERVER_PID" 2>/dev/null || true
  SERVER_PID=""
}

# $1 = web port. With CommonAVRini=0 the mac query parameter becomes a file
# name under the config directory - it is stat-ed, opened as an ini file, and
# written to when a key is missing. It arrives from the query string, so it is
# an untrusted string being used as a path, which is the same shape as the icon
# id above.
#
# A real Denon sends 32 hex characters, not a 12-character MAC, so this is not
# a field anyone should assume is well formed just because the name says "mac".
hostile_macs() {
  run=$WORK/macs
  mkdir -p "$run/config"
  cat > "$run/viritin.ini" <<INI
[Configuration]
INIVersion=1.2.2
MessageInfoLevel=1
IPAddress=127.0.0.1
CommonAVRini=0
[WebServer]
WebServerIPAddress=127.0.0.1
WebServerPort=$1
[DNSServer]
Enable=0
[RadioBrowser]
Enable=0
[MyStations]
Enable=0
INI
  cat > "$run/config/avr.ini" <<INI
[Configuration]
INIVersion=1.0.2
Protocol=
[MainMenu Items]
IdentifiersList=radiobrowser
LabelsList=Radio Browser
[RadioBrowser Filtering]
[RadioBrowser Sorting]
INI
  start_server "$run" "$1"
  echo "MACs that are not MACs"
  for mac in '../escaped' '..%2Fescaped' '....//escaped' '../../escaped' \
             '/tmp/viritin-escaped' '%2Ftmp%2Fviritin-escaped'; do
    curl -fsS --noproxy '*' --max-time 10 -o /dev/null \
      "http://127.0.0.1:$1/setupapp/x/loginxml.asp?mac=$mac" 2>/dev/null || true
  done
  # Anything written outside config/ is the failure. The config directory is
  # allowed to fill up with junk; escaping it is not.
  escaped=$(find "$run" -maxdepth 1 -name '*.ini' -not -name 'viritin.ini' 2>/dev/null | wc -l)
  escaped=$((escaped + $(find /tmp -maxdepth 1 -name 'viritin-escaped.ini' 2>/dev/null | wc -l)))
  if [ "$escaped" -eq 0 ]; then
    ok "no config file was written outside the config directory"
  else
    bad "$escaped file(s) were written outside the config directory"
    find "$run" -maxdepth 1 -name '*.ini' -not -name 'viritin.ini' | sed 's/^/       /'
    find /tmp -maxdepth 1 -name 'viritin-escaped.ini' | sed 's/^/       /'
    rm -f /tmp/viritin-escaped.ini
  fi

  # The mode still has to work: a receiver with a plausible identifier gets its
  # own file. A fix that simply stopped writing them would pass the check above.
  real=9392EAA878093E288048CF90D02D37EC
  curl -fsS --noproxy '*' --max-time 10 -o /dev/null \
    "http://127.0.0.1:$1/setupapp/x/loginxml.asp?mac=$real" 2>/dev/null || true
  if [ -f "$run/config/$real.ini" ]; then
    ok "a receiver's own identifier still gets its own config file"
  else
    bad "the per-AVR config file was not created for a valid identifier"
  fi
  kill "$SERVER_PID" 2>/dev/null || true
  wait "$SERVER_PID" 2>/dev/null || true
  SERVER_PID=""
}

echo "Fuzzing $BIN"
fuzz_dns "$PORT" "$((PORT + 1))"
fuzz_images "$((PORT + 2))" "$((PORT + 3))"
hostile_ids "$((PORT + 4))"
hostile_macs "$((PORT + 5))"

if [ "$fail" -ne 0 ]; then
  exit 1
fi
echo "All fuzz checks passed."
