# Working on Viritin

Viritin is a fork of [YTuner](https://github.com/coffeegreg/YTuner) by Greg P.,
MIT licensed, and nearly all of this code is his. Attribution is not decoration
here: the banner, the README, `LICENSE.txt` and the program header all name him,
and none of that gets swept up in a rename or tidied away.

Free Pascal 3.2.2 — what Debian, Ubuntu and Raspberry Pi OS actually ship, not
trunk. If something needs a newer compiler it does not go in.

## House rules

**Red-green.** Write the failing test first, run it, watch it fail *for the
reason you expect*, then implement. A test that has never been red has not been
tested. When a bug is found before its test, write the test and then reintroduce
the bug to confirm the test catches it — that is what the mutation checks in
`test-radiobrowser.sh`'s history were for.

**CalVer.** `APP_VERSION` is `YY.MM.DD.N`. Three constants that look similar and
are not:

| constant | means | moves when |
|---|---|---|
| `APP_VERSION` | this build | every release |
| `UPSTREAM_VERSION` | the YTuner release forked from | ~never |
| `INI_VERSION` | config-format compatibility | the format changes |

`INI_VERSION` is compared against `INIVersion=` in every user's config file.
Bump it casually and every install is told its file is outdated. The add-on's
`version:` in `viritin/config.yaml` tracks `APP_VERSION`.

**Lint everything, at the strictest setting available.** Shell scripts pass
`shellcheck --severity=style`. Note that CI's shellcheck is older than a typical
local one, so "clean here" is not "clean there" — lint at `--severity=style`
before pushing. `CHECKED=1 ./script/build.sh` adds `-Cr -Co -CR`, and CI runs
every suite against that binary.

`CHECKED=1` also links heaptrc, and leaves `cmem` out so that it works. `cmem`
replaces the memory manager with C `malloc` in its initialization, and `-gh`
installs heaptrc *first* — so cmem replaced the manager underneath and heaptrc
saw 6 allocations for a run that makes 43,000. Loading heaptrc after cmem
instead makes it wrap cmem, and segfaults mid-run. Dropping cmem from the
diagnostic build is what actually works; shipped builds are unchanged.

**Comments say why, not what.** Where a line exists because of a specific bug,
name the bug. `// Lines without '=' reserved a slot they never filled, so they
surfaced as blank entries on the AVR` is worth writing; `// loop over stations`
is not.

## Testing

No unit test framework. The suites below start a real binary and speak HTTP to
it:

    ./script/smoke-test.sh          vTuner protocol, both path shapes, legacy routes
    ./script/test-presets.sh        fetch, validate, cache, merge, fallbacks
    ./script/test-radiobrowser.sh   filtering, and awkward upstream responses
    ./script/test-leaks.sh          whether the heap grows with load (needs CHECKED=1)
    ./script/fuzz-test.sh           malformed DNS packets, hostile logos, ids that are not ids
    ./script/test-update.sh         the self-updater: what it replaces, and the rollback
    ./script/test-webgui.sh         the stations editor: who it answers, and what a wrong password costs
    ./script/test-remote.sh         the browser remote: what it forwards to the receiver, and what it refuses
    ./script/test-preset-strikes.sh  the preset generator: how many failures it takes to drop a station
    ./script/test-preset-icons.sh    where a station logo comes from, and what is refused as one

Each takes a binary path as `$1` and needs no network — mocks stand in for
radio-browser and the preset repository. That makes them an implementation-
independent spec: they would run against a rewrite in another language as-is.

`fuzz-test.sh` is worth running against a `CHECKED=1` build, where `-Cr` turns a
read past the end of a buffer into a reported error instead of a plausible
number. Two of its assertions exist because the thing they check was not true:
an icon id of `../secret.txt` returned that file, and a 3000x2 logo scaled to a
200-pixel icon rounded its height to zero and took the process down.

**Give every test phase its own ports, and pass them in as arguments.** Sharing
them lets a mock that failed to bind leave the previous phase's server
answering, which produces confident and completely false results. This has
happened twice. The second time, the ports were meant to differ — a helper
incremented a global — but each call site was a command substitution, so the
increment never left the subshell. A knowingly leaky binary then reported an
identical figure at both loads and the check passed it. Take the ports as
parameters and the whole class of bug goes away.

## Things that look safe and are not

**Anything a station directory hands you is attacker input.** A logo URL is
chosen by whoever submitted the station, and it is fetched and decoded in this
process. Headers are claims: cap what a decoder may allocate before it allocates
it, in `SetSize`, which is the one point every format's reader goes through. And
an id in a query string is not a file name - `GetIcon` used it as one.

**Strings written into files outlive the code.** `/ytuner/` URLs live in
bookmarks an AVR saved years ago and replays verbatim; `ytunerhost` is the
placeholder inside those files. Both are still served and read for that reason.
Anything persisted needs a migration, and the migration needs a test.

**Which AVR config file is live depends on `CommonAVRini`, and the default is
`avr.ini`.** Shipped as `CommonAVRini=1`, and `True` in the code as well, which
means every receiver reads `config/avr.ini` directly and `config/<mac>.ini` is
never written or consulted. Set it to `0` and the reverse holds: each receiver
gets its own file keyed by the MAC in the query string, and `avr.ini` becomes
the template that file is seeded from.

This entry used to assert the second half unconditionally, which is how a real
Denon connecting for the first time was expected to produce a
`config/0005CD350400.ini` that could not exist. Check the setting before
deciding which file to edit; filters put in the other one silently do nothing.

**Advertised URLs never carry a port.** Every one is `'http://' + URLHost + path`,
so the service assumes it is reachable on port 80 at `act_as_host`. Changing
`WebServerPort` alone does not move it; it only stops it answering where it says
it is.

**The intercept list is written down in three places.** `INTERCEPT_DNS` in
`src/dnsserver.pas`, `InterceptDNs=` in `cfg/viritin.ini`, and the dnsmasq block
in `doc/APPLIANCE.md`. A domain added to one and not the others is a
manufacturer that quietly stops working. `fuzz-test.sh` compares the last two
and queries the first, so all three go red together.

**A backgrounded subshell is not the process you killed.** `( cd "$WORK" &&
./viritin > log 2>&1 ) &` sets `$!` to the subshell, not the server. Linux hides
it: its shells fold the last command of a subshell into an `exec` on their own,
so the pid happens to be right. bash on macOS does not, and a CI run there ended
with the runner terminating fourteen orphan virittimet — each one able to hold a
port and be adopted by the next phase, which is the stray-server failure above
arriving by another door. Write `exec` and mean it. Then `wait` after `kill`:
`kill` only asks.

**The suites are GNU-flavoured unless someone checks.** `sed -i 's|x|y|' file`
works on Linux and fails on macOS, where BSD sed reads the next argument as the
backup suffix and then runs the file name as a script; `-i.bak` means the same
to both. macOS also caps a UDP datagram at `net.inet.udp.maxdgram`, 9216 bytes,
and `sendto` fails with `EMSGSIZE` rather than truncating — which killed the DNS
fuzzer mid-run and made the suite report that the server had stopped answering,
when it was the sender that had died. The macOS runners run presets,
radio-browser, strikes and fuzz now, so this class of difference goes red there
rather than being discovered by hand and written down as expected.

**Bind a mock through `script/testdata/localserver.py`.**
`HTTPServer.server_bind` calls `socket.getfqdn(host)` to fill in a `server_name`
nothing reads. 127.0.0.1 has no reverse record on a macOS runner, so that one
call waits out the resolver: 35 seconds per bind, once per test phase, which was
ten of the twelve minutes a macOS job took. Four milliseconds on Linux, which is
why it survived until something ran the suites on a Mac.

**The main web server honours `Application.Address`; the web GUI cannot.**
`TFPHTTPServer` has no `Address` property on 3.2.2, so the GUI binds every
interface and filters per connection instead.

## Upstream

Fixes that are not fork-specific are worth offering back. Do not claim in the
docs that they have been offered unless a pull request actually exists —
`doc/VIRITIN.md` said so once when it was not true.
