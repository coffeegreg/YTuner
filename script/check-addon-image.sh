#!/bin/sh
# Runs *inside* the built add-on image, mounted in by CI. It lives in a file
# rather than inline in the workflow because an inline `sh -c '...'` payload is
# one apostrophe away from silently ending early: a comment reading "s6's first
# wave" once closed the quote, and the assertions after it ran on the CI runner
# instead of in the container -- which fails with no output at all.
#
# The binary is a server and would never exit, so it is not started here. That
# it is present, executable and has every shared library it needs is what this
# can prove; script/smoke-test.sh covers it running.

echo "--- /opt/viritin"
# shellcheck disable=SC2012  # a listing for a human to read, on a fixed path
ls -la /opt/viritin /opt/viritin/cfg 2>&1 | head -40
echo "--- /etc/s6-overlay/s6-rc.d"
# shellcheck disable=SC2012  # ditto
ls -la /etc/s6-overlay/s6-rc.d 2>&1 | head -40
find /etc/s6-overlay/s6-rc.d -maxdepth 3 \
     \( -path "*viritin*" -o -name contents.d \) -exec ls -ld {} + 2>&1

echo "--- checks"
rc=0
check() {
  if eval "$2" >/dev/null 2>&1; then
    echo "ok   $1"
  else
    echo "FAIL $1"
    rc=1
  fi
}

check "the binary is present and executable" \
  "test -x /opt/viritin/viritin"
check "the bundled station list came across" \
  "test -f /opt/viritin/cfg/stations.ini"
check "the s6 run script is executable" \
  "test -x /etc/s6-overlay/s6-rc.d/viritin/run"
check "the service is declared longrun" \
  "grep -qx longrun /etc/s6-overlay/s6-rc.d/viritin/type"
# ordering, not just presence: without this the service starts in s6's first
# wave, before the container is wired up
check "the service is ordered after the base bundle" \
  "test -f /etc/s6-overlay/s6-rc.d/viritin/dependencies.d/base"
check "the service is registered under user/contents.d" \
  "test -f /etc/s6-overlay/s6-rc.d/user/contents.d/viritin"
check "the ini merge script came across" \
  "test -f /usr/bin/merge-ini.awk"
check "awk is installed" "command -v awk"

echo "--- ldd"
ldd /opt/viritin/viritin || true
check "no shared library is missing" \
  '! ldd /opt/viritin/viritin | grep -q "not found"'

if [ "$rc" -ne 0 ]; then
  echo "add-on image is not right; see FAIL above"
  exit 1
fi
echo "add-on image ok"
