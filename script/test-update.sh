#!/bin/sh
# Tests for script/viritin-update.sh, which runs unattended on machines nobody
# is sitting at. The rollback is the reason it exists, so the rollback is
# tested; an updater whose recovery path has never run is an updater that has
# only ever been tried on the happy day.
#
#   ./script/test-update.sh [path-to-retuner]
#
# A local HTTP server stands in for GitHub, so this needs no network.
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

WORK=$(mktemp -d)
MOCK_PID=""
cleanup() {
  if [ -n "$MOCK_PID" ]; then kill "$MOCK_PID" 2>/dev/null || true; fi
  stop_service
  rm -rf "$WORK"
}
trap cleanup EXIT INT TERM

fail=0
ok()  { echo "  ok   $1"; }
bad() { echo "  FAIL $1"; fail=1; }

# The same triple the release assets and the update script both compute.
case "$(uname -s)" in Darwin) OS=darwin ;; *) OS=linux ;; esac
case "$(uname -m)" in arm64|aarch64) CPU=aarch64 ;; *) CPU=x86_64 ;; esac
TARGET="$CPU-$OS"

PORT=18901
MOCK_PORT=18902

# A stand-in for launchd or systemd: the update script drives this through
# RESTART_CMD, so the health check it does afterwards is a real one against a
# real process running the binary that was just installed.
SERVICE_PID_FILE="$WORK/service.pid"
stop_service() {
  if [ -f "$SERVICE_PID_FILE" ]; then
    kill "$(cat "$SERVICE_PID_FILE")" 2>/dev/null || true
    wait "$(cat "$SERVICE_PID_FILE")" 2>/dev/null || true
    rm -f "$SERVICE_PID_FILE"
  fi
}
# With $WORK/refuse present it starts the service only when the installed
# binary is the one recorded in $WORK/good.cksum. That is how a release which
# starts by itself and then will not run as the service is simulated, without a
# stand-in binary that behaves unlike the real one.
cat > "$WORK/restart.sh" <<RESTART
#!/bin/sh
if [ -f "$SERVICE_PID_FILE" ]; then
  kill "\$(cat "$SERVICE_PID_FILE")" 2>/dev/null || true
  sleep 0.5
fi
if [ -f "$WORK/refuse" ] \\
   && [ "\$(cksum < "$WORK/install/viritin")" != "\$(cat "$WORK/good.cksum")" ]; then
  exit 0
fi
( cd "$WORK/install" && exec ./viritin -p "$PORT" > "$WORK/install/service.log" 2>&1 ) &
echo \$! > "$SERVICE_PID_FILE"
RESTART
chmod +x "$WORK/restart.sh"

# A release archive shaped exactly like the ones CI publishes: a directory
# named for the version and target, the binary inside it, and copies of the
# shipped config that an update must not put over the user's.
make_release() { # $1 = version, $2 = binary to package
  rm -rf "$WORK/assets"; mkdir -p "$WORK/assets"
  stage="$WORK/stage/viritin-$1-$TARGET"
  rm -rf "$WORK/stage"; mkdir -p "$stage/config"
  cp "$2" "$stage/retuner"; chmod +x "$stage/retuner"
  cp "$ROOT/cfg/viritin.ini" "$stage/"
  cp "$ROOT/cfg/avr.ini" "$stage/config/"
  ( cd "$WORK/stage" && tar -czf "$WORK/assets/viritin-$1-$TARGET.tar.gz" \
      "viritin-$1-$TARGET" )
}

start_mock() { # $1 = version, $2 = digest mode
  if [ -n "$MOCK_PID" ]; then kill "$MOCK_PID" 2>/dev/null || true; MOCK_PID=""; fi
  python3 "$ROOT/script/testdata/mock-release.py" "$MOCK_PORT" "$1" \
    "$WORK/assets" "$2" >/dev/null 2>&1 &
  MOCK_PID=$!
  i=0
  while [ "$i" -lt 50 ]; do
    if curl -fsS --noproxy '*' -o /dev/null \
         "http://127.0.0.1:$MOCK_PORT/repos/x/y/releases/latest" 2>/dev/null; then
      return 0
    fi
    i=$((i + 1)); sleep 0.2
  done
  echo "error: mock release server did not start" >&2; exit 1
}

# A fresh install of $1, with settings in it that no update may disturb.
install_version() { # $1 = version
  stop_service
  rm -rf "$WORK/install"; mkdir -p "$WORK/install/config"
  cp "$BIN" "$WORK/install/viritin"
  printf '%s\n' "$1" > "$WORK/install/.version"
  cat > "$WORK/install/viritin.ini" <<INI
[Configuration]
INIVersion=1.2.2
MessageInfoLevel=4
IPAddress=127.0.0.1
MyOwnSetting=keep-me
[WebServer]
WebServerIPAddress=127.0.0.1
WebServerPort=$PORT
[DNSServer]
Enable=0
[RadioBrowser]
Enable=0
[MyStations]
Enable=0
INI
  printf '[RadioBrowser Filtering]\nBitrateMax=333\n' > "$WORK/install/config/avr.ini"
}

update() { # runs the updater against the mock, returns its exit status
  PREFIX="$WORK/install" \
  REPO=o/r \
  DOWNLOAD="http://127.0.0.1:$MOCK_PORT" \
  API="http://127.0.0.1:$MOCK_PORT" \
  RESTART_CMD="$WORK/restart.sh" \
  LOGFILE="${LOGFILE:-$WORK/install/service.log}" \
  PROBE_PORT=18903 \
    sh "$ROOT/script/viritin-update.sh" "$@" > "$WORK/update.log" 2>&1
}

echo "Testing the updater with $BIN"

# --- nothing to do -----------------------------------------------------------
echo "- an install that is already current"
install_version 9.9.9.9
make_release 9.9.9.9 "$BIN"
start_mock 9.9.9.9 good
before=$(cksum < "$WORK/install/viritin")
if update; then ok "the updater succeeds"; else bad "the updater failed"; fi
if grep -q "already current" "$WORK/update.log"; then
  ok "it says so and stops"
else bad "it did not report being current"; fi
if [ "$(cksum < "$WORK/install/viritin")" = "$before" ]; then
  ok "the binary is untouched"
else bad "the binary changed with nothing to update"; fi

# --- the ordinary upgrade ----------------------------------------------------
# The archive carries viritin.ini and config/avr.ini. Copying those over an
# install is how a routine update would silently revert somebody's filters,
# stations and podcasts to the shipped defaults - so this checks the user's
# files by content, not just that the binary moved.
echo "- an upgrade to a newer release"
install_version 1.0.0.0
make_release 2.0.0.0 "$BIN"
start_mock 2.0.0.0 good
if update; then ok "the updater succeeds"; else
  bad "the updater failed"; sed -n '1,20p' "$WORK/update.log"; fi
if [ "$(cat "$WORK/install/.version")" = "2.0.0.0" ]; then
  ok "the recorded version moves"
else bad "the recorded version is $(cat "$WORK/install/.version")"; fi
if grep -q "MyOwnSetting=keep-me" "$WORK/install/viritin.ini"; then
  ok "viritin.ini is not overwritten by the archive's copy"
else bad "viritin.ini was replaced"; fi
if grep -q "BitrateMax=333" "$WORK/install/config/avr.ini"; then
  ok "config/avr.ini is not overwritten by the archive's copy"
else bad "config/avr.ini was replaced"; fi
if [ -f "$WORK/install/viritin.previous" ]; then
  ok "the previous binary is kept for rollback"
else bad "no previous binary was kept"; fi
# The first version of this check read a digest out of the REST API, where
# there is none, so it silently reported "no digest published" on every real
# release and never once compared anything. A check that cannot be seen
# succeeding is a check nobody knows is broken.
if grep -q "sha256 verified" "$WORK/update.log"; then
  ok "the checksum was actually compared, not skipped"
else bad "the update ran without verifying the checksum"; fi
if curl -fsS --noproxy '*' -o /dev/null \
     "http://127.0.0.1:$PORT/setupapp/x/loginxml.asp?token=0" 2>/dev/null; then
  ok "the service is serving afterwards"
else bad "the service is not answering after the update"; fi

# --- a release that cannot start ---------------------------------------------
# Caught by the probe run, before it is ever installed.
echo "- a release whose binary does not serve"
install_version 1.0.0.0
printf '#!/bin/sh\nexit 3\n' > "$WORK/broken"; chmod +x "$WORK/broken"
make_release 3.0.0.0 "$WORK/broken"
start_mock 3.0.0.0 good
before=$(cksum < "$WORK/install/viritin")
if update; then bad "the updater reported success on a broken release"; else
  ok "the updater fails"; fi
if grep -q "did not serve" "$WORK/update.log"; then
  ok "it says the download did not serve"
else bad "it did not explain why"; fi
if [ "$(cksum < "$WORK/install/viritin")" = "$before" ]; then
  ok "the installed binary is untouched"
else bad "a binary that cannot serve was installed anyway"; fi
if [ "$(cat "$WORK/install/.version")" = "1.0.0.0" ]; then
  ok "the recorded version does not move"
else bad "the recorded version moved on a failed update"; fi

# --- the address the service actually binds -----------------------------------
# WebServerIPAddress=default is the shipped value and does not mean loopback:
# Retuner resolves it to the machine's LAN address and binds that one
# specifically. A health check that assumed 127.0.0.1 therefore found nothing,
# rolled back an update that had worked, and then reported the rollback broken
# as well - on a real install, on the first release this updater was ever asked
# to fetch. Both probes were asking an address the service had never been on.
echo "- an install whose service does not listen on loopback"
install_version 1.0.0.0
# Both keys, and that matters: "default" for WebServerIPAddress is resolved
# against IPAddress, so leaving IPAddress pinned to 127.0.0.1 makes the service
# bind loopback after all and the phase proves nothing. The first version of
# this rewrote one key and passed against the very bug it exists for.
awk '/^IPAddress=/ { print "IPAddress=default"; next }
     /^WebServerIPAddress=/ { print "WebServerIPAddress=default"; next }
     { print }' "$WORK/install/viritin.ini" > "$WORK/r.new"
mv "$WORK/r.new" "$WORK/install/viritin.ini"
make_release 8.0.0.0 "$BIN"
start_mock 8.0.0.0 good
if LOGFILE="$WORK/install/service.log" update; then
  ok "the updater succeeds"
else
  bad "a service not on loopback was treated as a failed update"
  sed -n '1,12p' "$WORK/update.log"
fi
if grep -q "answering on" "$WORK/update.log"; then
  ok "it names the address it found the service on"
else bad "it did not say where it found the service"; fi
if grep -q "rolling back" "$WORK/update.log"; then
  bad "it rolled back a healthy update"
else ok "it does not roll back a healthy update"; fi
if [ "$(cat "$WORK/install/.version")" = "8.0.0.0" ]; then
  ok "the recorded version moves"
else bad "the recorded version is $(cat "$WORK/install/.version")"; fi
stop_server 2>/dev/null || true
stop_service

# --- the rollback ------------------------------------------------------------
# The probe cannot catch everything: a build can start perfectly against a
# throwaway config and still fail against the real one - a setting it no longer
# accepts, a port it can no longer take. That is the case the rollback exists
# for, and it is the one path here that must not be assumed to work. This
# stand-in below is a real, working binary, so the probe passes; the restart
# stand-in is what declines to bring it up as the service.
echo "- a release that passes the probe and then fails as the service"
install_version 1.0.0.0
before=$(cksum < "$WORK/install/viritin")
cksum < "$WORK/install/viritin" > "$WORK/good.cksum"
: > "$WORK/refuse"
# A working binary that is byte-different from the installed one, so the
# restart stand-in can tell them apart. Trailing bytes trouble neither an ELF
# nor a Mach-O, and this still runs - which is the point: the probe must pass,
# or this phase would be testing the probe again instead of the rollback.
cp "$BIN" "$WORK/newer"; printf '\n' >> "$WORK/newer"; chmod +x "$WORK/newer"
make_release 7.0.0.0 "$WORK/newer"
start_mock 7.0.0.0 good
if update; then bad "the updater reported success on a release that never served"; else
  ok "the updater fails"; fi
if grep -q "rolling back" "$WORK/update.log"; then
  ok "it rolls back"
else bad "it did not roll back"; fi
if grep -q "rolled back to 1.0.0.0$" "$WORK/update.log"; then
  ok "the rollback is confirmed healthy"
else bad "the rollback did not come up"; fi
if [ "$(cksum < "$WORK/install/viritin")" = "$before" ]; then
  ok "the original binary is back in place"
else bad "the installed binary is not the one we started with"; fi
if [ "$(cat "$WORK/install/.version")" = "1.0.0.0" ]; then
  ok "the recorded version does not move"
else bad "the recorded version moved despite the rollback"; fi
if curl -fsS --noproxy '*' -o /dev/null \
     "http://127.0.0.1:$PORT/setupapp/x/loginxml.asp?token=0" 2>/dev/null; then
  ok "the receiver is being served again"
else bad "nothing is serving after the rollback"; fi
rm -f "$WORK/refuse"

# --- a wrong checksum --------------------------------------------------------
echo "- published checksums that do not match"
install_version 1.0.0.0
make_release 4.0.0.0 "$BIN"
start_mock 4.0.0.0 bad
before=$(cksum < "$WORK/install/viritin")
if update; then bad "the updater installed an archive failing its own digest"; else
  ok "the updater fails"; fi
if grep -q "sha256 mismatch" "$WORK/update.log"; then
  ok "it names the mismatch"
else bad "it did not name the mismatch"; fi
if [ "$(cksum < "$WORK/install/viritin")" = "$before" ]; then
  ok "the installed binary is untouched"
else bad "the binary was replaced despite the mismatch"; fi

# --- no digest published -----------------------------------------------------
# Releases made before SHA256SUMS existed have none, and that must not become a
# reason those machines can never update again.
echo "- a release with no SHA256SUMS published"
install_version 1.0.0.0
make_release 5.0.0.0 "$BIN"
start_mock 5.0.0.0 none
if update; then ok "the updater still succeeds"; else
  bad "a missing digest blocked the update"; sed -n '1,20p' "$WORK/update.log"; fi
if grep -q "no SHA256SUMS published" "$WORK/update.log"; then
  ok "it says it could not check one"
else bad "it did not mention the missing checksums"; fi

# --- --check changes nothing -------------------------------------------------
echo "- --check reports without acting"
install_version 1.0.0.0
make_release 6.0.0.0 "$BIN"
start_mock 6.0.0.0 good
before=$(cksum < "$WORK/install/viritin")
if update --check; then ok "the updater succeeds"; else bad "--check failed"; fi
if grep -q "would update 1.0.0.0 -> 6.0.0.0" "$WORK/update.log"; then
  ok "it names both versions"
else bad "it did not report what it would do"; fi
if [ "$(cksum < "$WORK/install/viritin")" = "$before" ]; then
  ok "the binary is untouched"
else bad "--check installed something"; fi

if [ "$fail" -ne 0 ]; then
  echo "--- last update log ---" >&2
  cat "$WORK/update.log" >&2
  exit 1
fi
echo "All updater checks passed."
