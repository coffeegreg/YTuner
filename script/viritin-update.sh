#!/bin/sh
# Update an installed Retuner to the latest release, and put the old one back
# if the new one does not come up.
#
#   sudo ./viritin-update.sh            # update if there is a newer release
#   sudo ./viritin-update.sh --check    # say what would happen, change nothing
#
# Environment: PREFIX (default /usr/local/viritin), REPO, SERVICE, RESTART_CMD.
#
# This is meant to run unattended, on a machine nobody is sitting at - which is
# the whole reason the rollback exists. A release that does not start would
# otherwise leave the receiver with no station list until somebody visits.
set -eu

REPO=${REPO:-ollisulopuisto/viritin}
PREFIX=${PREFIX:-/usr/local/viritin}
SERVICE=${SERVICE:-io.github.ollisulopuisto.viritin}
API=${API:-https://api.github.com}
DOWNLOAD=${DOWNLOAD:-https://github.com}
CHECK_ONLY=0
[ "${1:-}" = "--check" ] && CHECK_ONLY=1

say() { echo "$(date '+%Y-%m-%d %H:%M:%S') viritin-update: $*"; }
die() { say "$*" >&2; exit 1; }

# Which archive this machine wants. The target triple is what the build script
# and the release assets both use, not what uname says.
case "$(uname -s)" in
  Darwin) os=darwin ;;
  Linux)  os=linux ;;
  *) die "unsupported system $(uname -s)" ;;
esac
case "$(uname -m)" in
  arm64|aarch64) cpu=aarch64 ;;
  x86_64|amd64)  cpu=x86_64 ;;
  *) die "unsupported architecture $(uname -m)" ;;
esac
TARGET="$cpu-$os"

[ -x "$PREFIX/viritin" ] || die "no installed binary at $PREFIX/viritin"

# The installed version. Written here on every successful update; absent on an
# install that predates this script, which is treated as "unknown, so update".
INSTALLED=$(cat "$PREFIX/.version" 2>/dev/null || echo unknown)

# The latest version, without parsing any JSON: /releases/latest redirects to
# the tag, and the tag is the version with a 'v' on the front.
latest_url=$(curl -fsSL -o /dev/null -w '%{url_effective}' "$DOWNLOAD/$REPO/releases/latest") \
  || die "could not reach $DOWNLOAD/$REPO/releases/latest"
LATEST=${latest_url##*/tag/}
LATEST=${LATEST#v}
case "$LATEST" in
  ''|*/*) die "could not read a version out of $latest_url" ;;
esac

say "installed $INSTALLED, latest $LATEST, target $TARGET"
if [ "$INSTALLED" = "$LATEST" ]; then
  say "already current"
  exit 0
fi
if [ "$CHECK_ONLY" = 1 ]; then
  say "would update $INSTALLED -> $LATEST"
  exit 0
fi

ASSET="viritin-$LATEST-$TARGET.tar.gz"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT INT TERM

say "downloading $ASSET"
curl -fsSL -o "$WORK/$ASSET" "$DOWNLOAD/$REPO/releases/download/v$LATEST/$ASSET" \
  || die "download failed"

# Best effort integrity check, against the SHA256SUMS the release job publishes
# beside the archives. TLS to GitHub already says the bytes came from GitHub, so
# this guards against a truncated or half-uploaded asset rather than against an
# attacker - and releases made before SHA256SUMS existed have none, which is not
# a reason to refuse to update forever.
#
# It reads the published file and not the REST API on purpose: the API has no
# per-asset digest. Some clients present an enriched response that does, which
# is exactly how the first version of this check came to be written against a
# field that is not there, and to report "no digest published" every time.
# -L, and it is the whole fix: a release download URL answers 302 to
# release-assets.githubusercontent.com, and -f does not treat a 302 as an
# error, so without it curl exits 0 having written nothing and the check
# reports there are no checksums. The archive download above always had -L,
# which is why that half worked.
digest=$(curl -fsSL "$DOWNLOAD/$REPO/releases/download/v$LATEST/SHA256SUMS" 2>/dev/null \
  | awk -v a="$ASSET" '$2 == a { print $1; exit }') || digest=
if [ -n "$digest" ]; then
  if command -v shasum >/dev/null 2>&1; then
    got=$(shasum -a 256 "$WORK/$ASSET" | cut -d' ' -f1)
  else
    got=$(sha256sum "$WORK/$ASSET" | cut -d' ' -f1)
  fi
  [ "$got" = "$digest" ] || die "sha256 mismatch: got $got, expected $digest"
  say "sha256 verified"
else
  say "no SHA256SUMS published for v$LATEST; continuing on the TLS transport alone"
fi

tar -xzf "$WORK/$ASSET" -C "$WORK" || die "archive did not extract"
NEW="$WORK/viritin-$LATEST-$TARGET/viritin"
[ -x "$NEW" ] || die "no binary inside the archive at $NEW"

# Prove the new binary serves before it becomes the installed one. A build that
# cannot start is the failure this whole script exists to survive, and finding
# out here costs nothing.
PROBE_PORT=${PROBE_PORT:-18999}
mkdir -p "$WORK/probe/config"
cp "$NEW" "$WORK/probe/viritin"
cat > "$WORK/probe/viritin.ini" <<INI
[Configuration]
INIVersion=1.2.2
MessageInfoLevel=4
IPAddress=127.0.0.1
[WebServer]
WebServerIPAddress=127.0.0.1
WebServerPort=$PROBE_PORT
[DNSServer]
Enable=0
[RadioBrowser]
Enable=0
[MyStations]
Enable=0
INI
# exec: $! must be the probe itself, or the kill below leaves it running and
# holding PROBE_PORT, and the next update run finds the port busy. bash on
# macOS does not fold this into an exec on its own.
( cd "$WORK/probe" && exec ./viritin -p "$PORT" > probe.log 2>&1 ) &
probe_pid=$!
probe_ok=0
i=0
while [ "$i" -lt 100 ]; do
  if curl -fsS --noproxy '*' -o /dev/null \
       "http://127.0.0.1:$PROBE_PORT/setupapp/x/loginxml.asp?token=0" 2>/dev/null; then
    probe_ok=1; break
  fi
  kill -0 "$probe_pid" 2>/dev/null || break
  i=$((i + 1)); sleep 0.2
done
kill "$probe_pid" 2>/dev/null || true
wait "$probe_pid" 2>/dev/null || true
if [ "$probe_ok" != 1 ]; then
  say "the downloaded binary did not serve; keeping $INSTALLED"
  sed -n '$p' "$WORK/probe/probe.log" >&2 2>/dev/null || true
  exit 1
fi
say "the new binary serves"

# Where the service answers, so the health check below asks the right socket.
#
# "default" does NOT mean loopback, and reading it that way rolled back a good
# update on a real install: Retuner resolves default to the machine's LAN
# address and binds that one specifically (Application.Address), so nothing is
# listening on 127.0.0.1 at all. The check then failed, the update was undone,
# and the rollback "failed" too - because both probes asked an address the
# service had never been on.
#
# So ask every address it could be on and accept any of them. The log is the
# ground truth: it prints the address it actually bound.
port=$(sed -n 's/^WebServerPort=//p' "$PREFIX/viritin.ini" 2>/dev/null | head -1)
[ -n "$port" ] || port=80
cfg_host=$(sed -n 's/^WebServerIPAddress=//p' "$PREFIX/viritin.ini" 2>/dev/null | head -1)
LOGFILE=${LOGFILE:-$PREFIX/viritin.log}

candidate_hosts() {
  # Newest first: what the running service last said it bound.
  tail -n 500 "$LOGFILE" 2>/dev/null \
    | sed -n 's/.*Web Service: listening on: \([0-9][0-9.]*\):.*/\1/p' | tail -1
  case "$cfg_host" in
    ''|default|0.0.0.0) ;;
    *) printf '%s\n' "$cfg_host" ;;
  esac
  printf '127.0.0.1\n'
}

# Overridable so an install that is not launchd-with-this-label - a different
# unit name, a container, the test suite - can say how to restart itself. A
# failing restart is reported by the health check below, not by set -e here.
if [ -z "${RESTART_CMD:-}" ]; then
  if [ "$os" = darwin ]; then
    RESTART_CMD="launchctl kickstart -k system/$SERVICE"
  else
    RESTART_CMD="systemctl restart $SERVICE"
  fi
fi
restart() {
  say "restarting: $RESTART_CMD"
  # shellcheck disable=SC2086  # deliberately word-split: this is a command line
  sh -c "$RESTART_CMD" >/dev/null 2>&1 || say "the restart command reported a failure"
}
healthy() {
  i=0
  while [ "$i" -lt 150 ]; do
    for h in $(candidate_hosts); do
      if curl -fsS --noproxy '*' -o /dev/null --max-time 5 \
           "http://$h:$port/setupapp/x/loginxml.asp?token=0" 2>/dev/null; then
        say "answering on $h:$port"
        return 0
      fi
    done
    i=$((i + 1)); sleep 0.2
  done
  say "no answer on any of: $(candidate_hosts | tr '\n' ' ')port $port"
  return 1
}

# Only the binary is replaced. viritin.ini and config/ are the user's, and the
# archive carries its own copies of both - copying those over an install is how
# somebody's filters, stations and podcasts would silently revert to the
# shipped defaults on a routine update.
say "installing $LATEST"
cp "$PREFIX/viritin" "$PREFIX/viritin.previous"
cp "$NEW" "$PREFIX/viritin.new"
chmod +x "$PREFIX/viritin.new"
mv "$PREFIX/viritin.new" "$PREFIX/viritin"
restart

if healthy; then
  printf '%s\n' "$LATEST" > "$PREFIX/.version"
  say "updated $INSTALLED -> $LATEST"
  exit 0
fi

say "the service did not answer after the update; rolling back"
cp "$PREFIX/viritin.previous" "$PREFIX/viritin"
restart
if healthy; then
  say "rolled back to $INSTALLED"
else
  say "rolled back to $INSTALLED and it is STILL not answering - look at this by hand"
fi
exit 1
