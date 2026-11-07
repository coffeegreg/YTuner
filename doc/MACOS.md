# Viritin on macOS

If you already have a Mac that stays on and sits on the same network as the
receiver, it can serve the radio itself: no virtual machine, no second address,
and no argument about port 80, because macOS listens on nothing there by
default.

> **CI builds and smoke-tests this.** Both Apple Silicon and Intel runners build
> the binary and run `script/smoke-test.sh` against it on every change, so the
> vTuner endpoints are known to work here and not merely known to compile. The
> first run of that job is what found the crash in `TryToFindSQLite3Lib` that
> made every macOS start fail; it is fixed. What CI cannot check is the parts
> below that are about your machine rather than the program — launchd, the
> firewall, sleep, and `ActAsHost`. Those are still reading rather than testing.

## Build it

You do not have to. Every change publishes a built `aarch64-darwin` (and
`x86_64-darwin`) tarball — see the Actions tab, or a release — and extracting one
skips this whole section. Build from source when you want to change something.

You need FPC 3.2.2, the LazUtils sources, `git` and `zip`.

* **FPC** — the installer from [freepascal.org](https://www.freepascal.org/download.html)
  covers Intel and Apple Silicon. `fpc -iTP` and `fpc -iTO` should answer
  `aarch64` and `darwin` on Apple Silicon. Homebrew's `fpc` works too if it
  gives you a native compiler.
* **LazUtils** — install Lazarus (it lands in `/Applications/Lazarus`) or
  `brew install lazarus`. `script/build.sh` looks in both, plus the Homebrew
  prefixes. If yours is elsewhere: `LAZUTILS_DIR=/path/to/lazutils ./script/build.sh`.
* **git and zip** come with the Xcode command line tools: `xcode-select --install`.

```sh
git clone https://github.com/ollisulopuisto/viritin ~/retuner
cd ~/retuner && ./script/build.sh          # -> bin/aarch64-darwin/retuner
```

The release build leaves out `-Xs`, which strips by passing `-s` to the linker —
Apple's no longer takes it. Run `strip` yourself if the few megabytes matter.

## Install it

```sh
sudo mkdir -p /usr/local/viritin/config /usr/local/viritin/cache
sudo cp bin/aarch64-darwin/retuner cfg/viritin.ini /usr/local/viritin/
sudo cp cfg/avr.ini /usr/local/viritin/config/
```

Then edit `/usr/local/viritin/viritin.ini`.

## The setting that will bite you here: ActAsHost

Viritin tells the receiver where to fetch stations and artwork, and `default`
means "an address of this machine" — chosen by walking the interface list. A Mac
running VMware Fusion or Docker has several: `vmnet1`, `vmnet8`, bridge
interfaces, each with an address that is real, private, and useless to a
receiver. Picking one of those fails in the worst way, because the log looks
perfectly healthy and the receiver simply gets nothing.

Set it explicitly to the Mac's LAN address:

```ini
[Configuration]
ActAsHost=192.168.10.50      ; the address the receiver must reach
IPAddress=default
LocalCountry=Finland
[WebServer]
WebServerPort=80
[DNSServer]
Enable=0                     ; the router is doing the redirect
```

And check what it actually tells the receiver, rather than trusting the setting —
this works on any platform and is the fastest way to catch a wrong `ActAsHost`:

```sh
curl -s "http://<the mac>/setupapp/x/loginxml.asp?mac=aabbccddee" \
  | grep -o 'http://[^<]*' | head
```

Every URL in there is one the receiver will follow. If they name an address the
receiver cannot reach, nothing else you do will help.

## Run it

`doc/viritin.plist` is a launchd daemon for the layout above.

```sh
sudo cp doc/viritin.plist /Library/LaunchDaemons/io.github.ollisulopuisto.viritin.plist
sudo launchctl bootstrap system /Library/LaunchDaemons/io.github.ollisulopuisto.viritin.plist
tail -f /usr/local/viritin/viritin.log
```

`launchctl print system/io.github.ollisulopuisto.viritin` shows its state, and
`sudo launchctl kickstart -k system/io.github.ollisulopuisto.viritin` restarts it.

**It runs as root**, because a daemon in `/Library/LaunchDaemons` does and
because port 80 is not negotiable — the receiver's firmware asks for it and
Viritin advertises URLs without a port. That is more privilege than this program
wants, and macOS offers nothing like the confinement the systemd unit in
`doc/STANDALONE.md` gets. If that bothers you — and it reasonably might, given
that station logos are files chosen by strangers and decoded in this process —
add a `UserName` key to the plist, set `WebServerPort=8080`, and redirect 80 to
8080 with a `pf` anchor. That is more moving parts and a rule that has to survive
a reboot, so it is a deliberate choice rather than the default here.

## Two macOS things that will look like Viritin bugs

**The firewall.** If macOS's firewall is on, the first incoming connection
prompts, and a daemon has nobody to answer the prompt. Allow the binary in
System Settings → Network → Firewall → Options.

**Sleep.** A sleeping Mac serves nothing, and the receiver's menu will simply be
empty until it wakes. `sudo pmset -a sleep 0` (and on a laptop, `disablesleep 1`)
if the machine is meant to be a server.

## Updating

By hand, from a source checkout:

```sh
cd ~/retuner && git pull && ./script/build.sh
sudo cp bin/aarch64-darwin/retuner /usr/local/viritin/viritin
sudo launchctl kickstart -k system/io.github.ollisulopuisto.viritin
```

### Updating itself

`script/viritin-update.sh` takes the latest release straight from GitHub, so a
machine you visit twice a year does not sit on the version it was installed
with. Install it alongside the binary:

```sh
sudo cp script/viritin-update.sh /usr/local/viritin/
sudo chmod +x /usr/local/viritin/viritin-update.sh
echo 26.08.27.4 | sudo tee /usr/local/viritin/.version   # the version you have now
sudo cp doc/viritin-update.plist /Library/LaunchDaemons/
sudo chown root:wheel /Library/LaunchDaemons/io.github.ollisulopuisto.viritin.update.plist
sudo chmod 644 /Library/LaunchDaemons/io.github.ollisulopuisto.viritin.update.plist
sudo launchctl bootstrap system \
  /Library/LaunchDaemons/io.github.ollisulopuisto.viritin.update.plist
```

The `.version` line is only to save a pointless first run: with no stamp the
updater treats the install as unknown and reinstalls the current release once.

See what it would do, without doing it:

```sh
sudo /usr/local/viritin/viritin-update.sh --check
```

What it will and will not touch:

- **Only the binary is replaced.** `viritin.ini` and everything under `config/`
  are yours. The release archive carries its own copies of both, and putting
  those over an install is how a routine update would revert your filters,
  stations and podcasts to the shipped defaults without saying anything.
- **A release that does not run never becomes the installed one.** The download
  is started against a throwaway config on a spare port first, and has to serve
  `loginXML.asp` before it is installed anywhere.
- **If the service does not come back, the old binary does.** The previous
  binary is kept as `retuner.previous`; when the restarted service fails to
  answer, it is put back and restarted again. This is the reason the whole thing
  exists — a bad release on a machine nobody is sitting at otherwise means no
  radio until somebody drives there.
- **Checksums are checked when published.** Releases carry a `SHA256SUMS` file
  beside the archives, and a mismatch refuses the update. Releases made before
  that file existed have none and still install, on the strength of TLS to
  GitHub — a missing file should not mean a machine can never update again.
  `sha256 verified` in the log is how you know the comparison actually ran.

`/usr/local/viritin/update.log` records every run, including the ones that found
nothing to do.

## Checking it works, in an order that localises a failure

```sh
curl -I http://<the mac>/setupapp/x/loginxml.asp?token=0     # Viritin on port 80
curl -s "http://<the mac>/setupapp/x/loginxml.asp?mac=aabbccddee" \
  | grep -o 'http://[^<]*' | head                            # what it advertises
dig radioyamaha.vtuner.com @<your resolver>                  # the DNS override
dig radioyamaha.vtuner.com                                   # what the receiver gets
```

Then the receiver's Internet Radio button. `New AVR connected (<mac>)` in the log
is the moment it is talking to you.
