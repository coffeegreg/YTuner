# Viritin on a machine of its own

The receiver's firmware asks for port 80, and Viritin advertises every station
and artwork URL as `http://<host>/…` with no port in it. So port 80 at the
address the receiver is sent to has to be Viritin, and nothing else. Moving
`WebServerPort` does not help: it changes where Viritin listens, not what it
tells the receiver.

That is a problem on any machine already serving something on 80 — a Home
Assistant box behind an nginx proxy is the usual one. Three ways out:

1. Take port 80 away from the other service. Cheapest when you can.
2. Give Viritin a second address on the same machine. Only works if the other
   service is bound to one specific address; a wildcard bind (`0.0.0.0:80`,
   which is what a Docker port mapping gives you) occupies port 80 on *every*
   address the machine has, and then whichever service starts first wins and the
   other one fails. Check with `docker ps` or, on a system with no tools,
   `awk '$4=="0A"{print $2}' /proc/net/tcp | grep -i ':0050$'` — `00000000:0050`
   is the wildcard.
3. Give Viritin a machine. That is this document — or, if the machine you have
   in mind is a Mac that already stays on, [MACOS.md](MACOS.md), which needs no
   virtual machine at all.

A machine of its own is not a heavy answer. Measured rather than guessed:

| | |
|---|---|
| the running service | ~8 MB resident, behind a 4.6 MB binary |
| a full build, compiler and linker at their peak | ~144 MB |
| the source tree, the Indy clone and the build output | ~85 MB |

So one core and 512 MB of RAM runs it comfortably, and 8 GB of disk is generous.
The thing that wants more memory is the **installer**, not Viritin: Debian's
graphical installer expects well over a gigabyte. Give the VM 2 GB while you
install, then shut it down and turn the memory back down — or use the text-mode
installer, which is far less hungry. If memory is genuinely scarce, build the
binary on another machine of the same architecture and copy just the binary
across; then the machine only ever needs the 8 MB.

And it puts the part of this program that decodes files chosen by strangers —
station logos come from whoever submitted the station — somewhere of its own,
which is worth something on a box that also runs your house.

## The machine

Any Linux that ships Free Pascal 3.2.2: Debian, Ubuntu, Raspberry Pi OS. On
Apple Silicon under VMware Fusion or UTM, use the arm64 image and build there;
the build script targets whatever compiler it finds.

**Bridge the network adapter.** NAT will not do — the receiver has to reach this
machine directly on the LAN, and it will be told an address that has to work
from where the receiver sits.

**Fix the address**, either static in the guest or as a DHCP reservation on the
router. Two other places name it — `ActAsHost` here, and the DNS override that
sends `*.vtuner.com` to it — and an address that moves breaks both silently. The
receiver just shows an empty menu.

## Build and install

```sh
sudo apt install -y fp-compiler fp-units-fcl fp-units-net fp-units-db \
                    fp-units-misc lazarus-src zip git
git clone https://github.com/ollisulopuisto/viritin ~/retuner
cd ~/retuner && ./script/build.sh          # Indy is fetched on the first run
```

Lay the runtime directory out the way the README describes, with the binary and
its ini side by side:

```sh
sudo mkdir -p /opt/viritin/config /opt/viritin/cache
sudo cp bin/*/retuner cfg/viritin.ini /opt/viritin/
sudo cp cfg/avr.ini /opt/viritin/config/
sudo useradd -r -s /usr/sbin/nologin retuner
sudo chown -R retuner:retuner /opt/viritin
```

Then edit `/opt/viritin/viritin.ini`. The settings that matter for this layout:

```ini
[Configuration]
IPAddress=default
LocalCountry=Finland          ; a Local Stations entry in the main menu
[WebServer]
WebServerPort=80
[DNSServer]
Enable=0                      ; the router is doing the redirect
```

Leave `ActAsHost` at `default` unless the receiver should reach this machine at
some address other than its own — behind NAT, for instance.

## Running it

`doc/viritin.service` is a unit for this layout. It grants `CAP_NET_BIND_SERVICE`
rather than running as root, and confines what an image decoder handed a hostile
file could reach.

```sh
sudo cp doc/viritin.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now retuner
journalctl -u retuner -f
```

If you change `RestrictAddressFamilies`, keep `AF_NETLINK`. Every `default` in
the ini is resolved by enumerating the machine's interfaces, which happens over
a netlink socket; without it those settings resolve to nothing.

## Updating

Running from a clone means an update is a pull and a rebuild. The ini in
`/opt/viritin` is untouched by it.

```sh
cd ~/retuner && git pull && ./script/build.sh
sudo install -o retuner -g retuner bin/*/retuner /opt/viritin/viritin
sudo systemctl restart retuner
```

## Checking it works, in an order that localises a failure

```sh
curl -I http://<this machine>/setupapp/x/loginxml.asp?token=0   # Viritin on 80
dig radioyamaha.vtuner.com @<your resolver>                     # the override
dig radioyamaha.vtuner.com                                      # what the receiver gets
```

Then the receiver's Internet Radio button. `New AVR connected (<mac>)` in the
log is the moment it is talking to you; `MessageInfoLevel=4` logs every request
after that. A menu that appears but will not play is a different layer — the
stream or the playlist behind it — and the log says which.

## A box that does the DNS too

Everything above leaves the DNS override to your router. If your router will not
do it — plenty of ISP-supplied ones will not — [APPLIANCE.md](APPLIANCE.md)
builds the same install onto a dedicated machine that answers DNS as well, so
the only thing configured anywhere else is one number typed into the receiver.

## What you give up

The Home Assistant add-on manages options through a UI, keeps the config in
`/config` where its backups reach, and updates with the rest of your add-ons.
None of that applies here: the ini is the interface, and updating is the pull
above. In exchange, nothing on this machine ever competes for port 80.
