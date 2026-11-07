# Building a Viritin appliance

One box, Ethernet to the router, that answers vTuner for every receiver on the
network. It runs two things: Viritin on port 80, and a resolver on port 53 that
rewrites the vTuner domains to itself and forwards everything else.

Nothing here is a new component. It is the standalone Linux install from
[STANDALONE.md](STANDALONE.md) with dnsmasq beside it and a static address, put
on hardware small enough to leave behind a cabinet.

## What the box has to do

| | why |
|---|---|
| **Serve HTTP on port 80** | Receivers ask for port 80 and Viritin advertises every station URL without a port. Not negotiable — see the note in `viritin.service`. |
| **Answer DNS on port 53** | The receiver has to be told `radiodenon.com` is this box. |
| **Forward all other DNS** | Once the receiver points here for DNS, *every* lookup it makes comes here, stream hostnames included. Get this wrong and the radio stops. |
| **Hold one address for ever** | The receiver stores the DNS server as a number. If the box's address changes, the receiver silently stops working. |

That last one is the whole reason this is an appliance and not a container on
something else.

## Parts

**The cheapest thing that works today is a used x86 thin client.** An HP t620 or
t630, a Dell Wyse 5070, a Lenovo ThinkCentre Tiny — €25 to €50 second-hand,
gigabit Ethernet, a real SSD instead of an SD card, no fan on most of them, and
it runs the `x86_64-linux` archive that this project already publishes. Nothing
to cross-compile and nothing to wait for.

**A Raspberry Pi 4 Model B (2GB)** is the nicer object: smaller, silent, gigabit
Ethernet, and boots from USB SSD. It needs the `aarch64-linux` build, which CI
now publishes.

What not to buy:

- **Pi Zero 2 W.** No Ethernet, and 512 MB is tight — see *Measure it first*
  below before believing anyone, this document included, about whether it fits.
- **Pi 5.** Nothing here needs it.
- **Anything wireless-only.** A box whose whole job is answering DNS for a
  device that cannot tolerate DNS failure should not be on Wi‑Fi.

Add: a decent power supply (undervoltage on a Pi shows up as corruption months
later), and an SSD or good USB stick rather than an SD card, because logs write
constantly and SD cards die of exactly that.

### Measure it first

Do not take a memory figure on faith, this document's included. Viritin's
footprint depends on how many stations your country lists and which cache type
you set. Ask the install you already have:

```sh
ps -o rss= -p "$(pgrep -f '/retuner$' | head -1)"    # KB, after it has been browsed
```

Browse a few countries on the receiver first — the number before anything has
been asked for is not the number that matters. Whatever it says, double it and
buy a board with room to spare.

## Headroom for other things

The usual question is whether a box like this can also be the Spotify endpoint,
or run something else small beside Viritin. Roughly, and these are estimates
rather than measurements — do the `ps -o rss=` check above once it is running:

| | RAM | CPU |
|---|---|---|
| Viritin | tens of MB, more once a big country is cached | idle between requests |
| librespot / spotifyd, one stream | tens of MB | a few percent of one core |
| Re-encoding that stream to MP3 for a receiver | small | under a tenth of a core |
| dnsmasq | a few MB | nothing |

None of that changes what to buy. A 2 GB Pi 4 or any x86 thin client with 4 GB
absorbs the lot without noticing. Two cores is plenty; the decode and encode of
a single audio stream is not a demanding job on hardware made this century.

**The constraint is latency, not the board.** Nothing here lets a receiver join
Spotify Connect — a receiver plays an HTTP stream, so bridging means running a
Connect client on this box, re-encoding what it plays, and serving that as a
station. [SPOTIFY.md](SPOTIFY.md) is the recipe, and it is straight about the
cost: several seconds between pressing play and hearing sound, again on every
track change, and transport buttons on the receiver that do nothing useful. Fine
for putting an album on; unusable as a remote control, and no amount of CPU
changes that, because the buffer is in the amplifier.

So before buying anything for this reason, check what the receiver already does.
Most AV receivers made since about 2015 have Spotify Connect, AirPlay or
Chromecast built in, and a Chromecast on an HDMI input covers it on the ones
that do not. All three are designed for the job and none of them go through a
radio buffer.

## The build

### 1. The system

Debian stable or Raspberry Pi OS Lite (64-bit), no desktop. Then follow
[STANDALONE.md](STANDALONE.md) for Viritin itself: the `retuner` user,
`/opt/viritin`, and `viritin.service` — which already grants
`CAP_NET_BIND_SERVICE`, so port 80 does not need root.

Take the binary from a release rather than building it, which is the whole point
of an appliance you set up once:

| box | archive |
|---|---|
| x86 thin client, mini PC | `retuner-<version>-x86_64-linux.tar.gz` |
| Raspberry Pi 4/5, 64-bit OS | `retuner-<version>-aarch64-linux.tar.gz` |

Unpack it, copy `retuner` to `/opt/viritin/`, and take `viritin.ini` and
`config/avr.ini` from it **only on a first install** — on an existing box they
are yours and the updater deliberately leaves them alone.

### 2. A fixed address

Give the box a static address outside the DHCP pool, or a DHCP reservation on
the router. Either is fine; having neither is not, because the receiver stores
the number.

```sh
# NetworkManager, which is what current Debian and Raspberry Pi OS use
sudo nmcli con mod "Wired connection 1" \
  ipv4.method manual ipv4.addresses 192.168.10.2/24 ipv4.gateway 192.168.10.1 \
  ipv4.dns 127.0.0.1
sudo nmcli con up "Wired connection 1"
```

`ipv4.dns 127.0.0.1` points the box at its own dnsmasq, so it resolves the same
way the receiver will and you find breakage yourself first.

### 3. DNS, with dnsmasq and not with Viritin's own

Viritin has a built-in DNS server. **Do not use it here.** Turn it off:

```ini
[DNSServer]
Enable=0
```

The reason is measured, not stylistic. Viritin's DNS server is Indy's, which
resolves upstream *on the thread that answers queries*, with a five second wait
per entry in its server list and no concurrency. Two upstreams that do not
answer is ten seconds during which the service answers nothing at all —
intercepted names included. We watched this in CI: a burst of lookups for names
nobody intercepts left a backlog that swallowed two later queries, and it looked
like one manufacturer's domain was broken.

On an appliance that is the receiver's *only* resolver, that same stall is the
radio cutting out mid-song. dnsmasq is asynchronous, caches, and has had twenty
years of exactly this job.

`/etc/dnsmasq.conf`:

```conf
# Answer for the vTuner domains with this box. Each entry covers the apex and
# every subdomain, which matters: receivers ask for both radiodenon.com and
# radio.radiodenon.com depending on the firmware.
address=/vtuner.com/192.168.10.2
address=/radiodenon.com/192.168.10.2
address=/radiomarantz.com/192.168.10.2
address=/radioharmankardon.com/192.168.10.2
address=/radiosetup.com/192.168.10.2
address=/my-noxon.net/192.168.10.2
address=/wifiradiofrontier.com/192.168.10.2

# Everything else goes upstream. Two of them, so one going away is not an
# outage. Use your router if you would rather keep its filtering.
no-resolv
server=1.1.1.1
server=9.9.9.9

# No dhcp-range anywhere in this file, which is what keeps dnsmasq's DHCP
# server switched off. This box is a resolver; answering DHCP on somebody
# else's LAN is a good way to break the whole house.
cache-size=1000
listen-address=127.0.0.1,192.168.10.2
bind-interfaces
```

Keep that list in step with `InterceptDNs` in `cfg/viritin.ini`; they are the
same set and drifting apart is how a manufacturer quietly stops working.

`frontier-nuvola.net` is deliberately absent — but not because it is still
running. Nuvola shut down on 31 October 2024; it was handed to airable rather
than switched off, and radios from brands that signed with airable go on
reaching a working service under those names, so intercepting them by default
would break hardware that works today. If your brand was dropped in that
handover, adding the name by hand is exactly the case for it. See
[OTHER-PORTALS.md](OTHER-PORTALS.md).

### 4. Tell Viritin where it lives

In `/opt/viritin/viritin.ini`:

```ini
ActAsHost=192.168.10.2
```

This is the setting that will bite you. Viritin builds every station URL as
`http://<ActAsHost><path>`, so if it is wrong the menu appears and every station
in it fails. [MACOS.md](MACOS.md) has the longer version.

### 5. Point the receiver at it

Either works; the first needs nothing from the router.

**On the receiver.** Network settings → manual → DNS = `192.168.10.2`. Leave
address, mask and gateway as they were. One number, typed once.

**On the router,** if it lets you set DHCP option 6 per device. Cleaner, because
nothing on the receiver has to be remembered, but many ISP routers will not.
Doing it for the *whole* LAN also works and makes this box everyone's resolver —
which is a bigger promise than it first sounds.

### 6. Updates

```sh
sudo cp script/viritin-update.sh /opt/viritin/
sudo chmod +x /opt/viritin/viritin-update.sh
sudo cp doc/viritin-update.service doc/viritin-update.timer /etc/systemd/system/
sudo systemctl enable --now viritin-update.timer
```

It replaces only the binary, proves a download serves before installing it, and
puts the old one back if the service does not come up. `--check` says what it
would do. The details are under *Updating itself* in [MACOS.md](MACOS.md); the
script is the same one.

## Checking it works, in an order that localises a failure

Run these on the box, then from another machine, then let the receiver try.

```sh
# 1. Viritin is up and serving the endpoint a receiver hits first
curl -sI "http://192.168.10.2/setupapp/x/loginxml.asp?token=0"

# 2. What it advertises - these must be the appliance's address, and portless
curl -s "http://192.168.10.2/setupapp/x/loginxml.asp?mac=aabbccddee" \
  | grep -o 'http://[^<]*' | head

# 3. The rewrite, apex and subdomain both
nslookup radiodenon.com 192.168.10.2
nslookup radio.vtuner.com 192.168.10.2

# 4. Forwarding still works, which is the half that breaks the radio quietly
nslookup github.com 192.168.10.2
```

Then the receiver's Internet Radio button. `New AVR connected (<mac>)` in
`/opt/viritin/viritin.log` means it arrived.

If step 4 is the one that fails, nothing about internet radio will look wrong
until a station will not play.

## Things that will bite

**The receiver caches DNS, sometimes until it is power-cycled.** Pull the plug,
do not just re-enter the setting.

**A second DNS server in the receiver's settings defeats all of this.** If it
has a secondary field pointing at the router or a public resolver, it may ask
that one and get the real answer. Leave the secondary blank, or set it to this
box as well.

**The box is now a single point of failure for that receiver's whole network
experience.** That is the trade for not touching the router. `Restart=on-failure`
in the unit covers a crash; nothing covers the box being unplugged.

**SD card wear.** Set `MessageInfoLevel` no higher than it needs to be, and let
logrotate have `/opt/viritin/viritin.log`. Debug logging on an SD card is a
countdown.

## What this is not

It is not a product. Selling a box whose function is answering for somebody
else's domain is a different proposition from running one on your own network —
vTuner still operates a paid service — and hardware means firmware updates and
returns. Build one for your own cabinet; think much harder before building
fifty.
