# Running on a remote host (VPS)

Viritin does not have to sit on the same network as your AVR. It runs perfectly well on a VPS — an Oracle Free Tier instance, a small cloud VM, anything with a public address — and the setup on the listener's side is **one setting on the amplifier**. No router configuration, no Pi-hole, no extra box at home.

## The short version

1. Run Viritin on the VPS with the config at the bottom of this page.
2. Open TCP/80 and UDP/53 to the internet.
3. On the AVR: Network settings → set DNS server to the VPS's public address.

That is the whole client-side setup, and it is done with the remote control. Nothing else at home changes, and no other device on the network is affected — only the AVR uses that DNS server.

## Why this works

The AVR's firmware has the vTuner hostname burned in, so the only way to reach Viritin instead is to control what that name resolves to. On a home network you do that at the router. Pointing the AVR's own DNS setting at Viritin achieves the same thing for that one device, and every AVR of this vintage exposes a manual DNS field.

Two settings make it work off-LAN.

**`DNSAdvertiseIP`** — the address Viritin puts in its intercepted answers. By default it answers with the address the query arrived on, which is right on a LAN but wrong behind NAT: on a VPS that is the private address (`10.0.0.x`), which your AVR cannot reach. Set it to the public address.

```ini
[DNSServer]
DNSAdvertiseIP=203.0.113.10
```

**`RestrictForwarding`** — because a DNS server on the public internet that answers anything for anyone is an open resolver, and open resolvers get abused for amplification attacks. With this on:

- **vTuner names are answered for anybody.** Harmless: the reply is tiny and only ever points at your own server.
- **Everything else is refused** unless the client is known.
- **A client becomes known by reaching the web service.** Your AVR looks up the vTuner hostname, connects to Viritin over HTTP, and that connection reveals the address it is really coming from. From then on its station lookups are forwarded too.

That ordering is what keeps the setup to a single field on the amp: the AVR authorises itself, in the course of doing what it was going to do anyway. Entries last 24 hours and refresh on every visit, so a changing home IP sorts itself out. `AllowedClients` can pin extra addresses if you want them permanently allowed.

```ini
RestrictForwarding=1
```

## Also required

**`ActAsHost`.** The URLs Viritin hands the AVR — stations, icons, bookmarks — are built from this. Left at `default` it resolves to the machine's own interface address, private again.

```ini
[Configuration]
ActAsHost=203.0.113.10        ; public address, or a hostname
```

**Port 80.** The AVR's firmware contacts the vTuner host on port 80 and that is not negotiable from Viritin's side. Binding it needs privileges: run as root, grant the capability once with `setcap 'cap_net_bind_service=+ep' ./retuner`, or put a reverse proxy in front.

**On Oracle Cloud, two firewalls must both allow traffic** and only one is obvious:

1. Ingress rules for TCP/80 and UDP/53 in the VCN security list (or network security group).
2. The instance's own firewall. Oracle's images ship with restrictive `iptables`/`firewalld` rules, and forgetting this is the usual reason a correctly-configured security list still looks dead.

## What is exposed, and what is not

Port 80 is open to the internet, so anyone who finds it can browse your instance and stream through it on your bandwidth. Restricting the rule to your home address is worth doing if your address is stable. What is *not* a risk:

- The icon, play and relay endpoints take a **station id**, never a URL, so none of them can be used as an open proxy — they only fetch what is in the station catalogue.
- The DNS service is not an open resolver once `RestrictForwarding=1`.
- The stations editor is off by default, binds to loopback, and refuses to start without a password.
- The maintenance shutdown service is off by default and authorises on the peer address.

`MyToken` is **not** authentication — the login endpoint hands it to anyone who asks. Do not treat it as a lock.

## What actually crosses the VPS

Only menus, by default. Once the AVR has a station's URL it connects to the station **directly**, so audio never touches your VPS and its location does not affect playback — only how quickly menus paint.

That changes if you enable `RelayHTTPS`. A relayed station is fetched by Viritin and re-served, so its audio flows in and out of the VPS for as long as you listen, doubling that traffic against your egress allowance. Oracle's Free Tier allowance is generous enough that ordinary listening is not a concern, but it is worth knowing which switch moves audio onto the machine.

## Full VPS configuration

```ini
[Configuration]
ActAsHost=203.0.113.10        ; public address or hostname -- not "default"

[WebServer]
WebServerIPAddress=default
WebServerPort=80

[DNSServer]
Enable=1
DNSServerIPAddress=default
DNSServerPort=53
DNSAdvertiseIP=203.0.113.10   ; public address: what the AVR is told to connect to
RestrictForwarding=1          ; do not be an open resolver
```

Then, on the AVR: Network → DNS server → `203.0.113.10`.

## If you would rather not expose DNS

The alternative is the traditional one: leave `Enable=0` in `[DNSServer]` and redirect `*.vtuner.com` to the VPS on your own network, using your router, Pi-hole, AdGuard Home, dnsmasq or OPNsense. That needs no open UDP/53 on the VPS, but it does need a router you can configure — which is exactly what the DNS setting on the amp lets you avoid.
