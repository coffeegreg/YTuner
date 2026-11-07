# Viritin

Your AV receiver has an internet radio button that stopped working. Viritin
answers the service the receiver is still trying to reach, and the button works
again — the same menus, the same remote, no app.

This add-on runs Viritin on your Home Assistant machine.

## Installation

1. Settings → Add-ons → Add-on store → ⋮ → **Repositories**, and add
   `https://github.com/ollisulopuisto/viritin`.
2. Install **Viritin** from the list. The first install compiles the source, so
   give it a few minutes — longer on a Raspberry Pi.
3. Start it, then point the receiver at it (below).

## Pointing the receiver at Viritin

The receiver has the old service's hostname built into its firmware, so the
whole job is making that name resolve to this machine. Pick whichever of these
you can actually do:

**On your router, Pi-hole or AdGuard Home** — add an override sending
`*.vtuner.com` to this machine's address. Nothing else changes, and the add-on's
own DNS service stays off. This is the tidiest option when you have a router you
can configure.

**With the add-on's DNS service** — turn on `dns_service`, then set the DNS
server on the receiver itself (Network settings, done with the remote) to this
machine's address. Only that one device is affected. Note that **port 53 is
often already taken** on a Home Assistant machine: if you run the AdGuard Home
or Pi-hole add-on, one of the two will fail to start, and the one that already
owns the port is the better place to do the redirect anyway.

Either way, the receiver then finds Viritin where it expected vTuner, and its
internet radio menu fills up.

## The options

**`act_as_host`** — the address the receiver is told to fetch stations and
artwork from. Leave it blank and Viritin uses this machine's own address, which
is right whenever the receiver is on the same network. Set it only when that
address is not the one the receiver should reach — for example, this machine is
behind NAT from the receiver's point of view.

**`web_port`** — 80 by default, because the receiver's firmware asks for port 80
and that is not negotiable from Viritin's side. If something else on this
machine already answers on port 80, the add-on will not start; changing the port
here only helps if you also have a proxy putting it back on 80. A second address
on this machine usually does not help either, because a Docker port mapping
binds every address it has. When port 80 is genuinely spoken for, running
Viritin on a small machine of its own is the way out —
[doc/STANDALONE.md](https://github.com/ollisulopuisto/viritin/blob/master/doc/STANDALONE.md)
has the whole recipe, including a systemd unit.

**`local_country`** — put your country here (spelled as Radio Browser spells it,
e.g. `Finland`) and a **Local Stations** entry appears in the main menu going
straight to it. Otherwise reaching your own country means scrolling a few hundred
countries with a jog dial, every time. Blank hides the entry.

**`resolve_playlists`** — on by default here. Many station links are `.m3u` or
`.pls` files rather than the audio itself, and firmware of this era often cannot
follow one. With this on, Viritin opens the playlist and hands the receiver the
stream inside it. Costs one small fetch when a station is selected.

**`relay_https`** / **`relay_port`** — a growing share of stations are
HTTPS-only, and these receivers have no TLS at all. With the relay on, Viritin
fetches such a stream itself and re-serves it unencrypted on the local network.
The cost is real and worth knowing: a relayed station keeps a connection and a
thread open on this machine for as long as it plays. Fine on a spare PC, less
welcome on a Pi that is also running everything else in your house.

**`radiobrowser`** and **`cache_type`** — Radio Browser is the station
directory. `catMemStr` keeps the cache in memory and is a good default;
`catPermMemDB` is faster to browse but builds a full station database first,
which wants a few hundred MB and real CPU. On a Pi, stay with `catMemStr`.

**`my_stations`** — your own station list, kept in `stations.ini` in the add-on's
configuration folder. An example is written there on first start.

**`podcasts`** / **`podcast_episodes_limit`** — off by default. Feeds are listed
in `podcasts.ini` in the configuration folder, and an example is written there
the first time you switch this on. Each feed becomes a folder and each episode a
station, so the receiver browses and plays them the way it does radio.

Turn **`relay_https` on as well**. Podcast episodes are very often HTTPS-only,
and these receivers have no TLS, so without the relay the episodes will list and
then refuse to play. The add-on says so in its log if you enable one without the
other.

`podcast_episodes_limit` is how many episodes of a feed to offer, newest first.
A long-running weekly show can carry a decade of back catalogue, which is a lot
of jog dial.

**`bookmarks`** — lets the receiver save favourites with its own remote, if it
supports that.

**`stations_editor`** — a small web page for editing the station list, on
`stations_editor_port`, which defaults to 8090. **It will not start without a
password**, because it writes configuration files. Read the security note below
before turning it on. The default used to be 8080; the add-on shares the host's
network, so it was competing with every proxy and container on the machine. If
you had the editor on 8080 and never changed the option, it moves — set it back
to 8080 in the add-on options if something of yours depends on that.

**`log_level`** — `normal` is right. `debug` logs every request, including
station names, and is for working out why something will not play.

**`advanced_ini`** — anything Viritin supports that the options above do not
cover. Include the `[Section]` header. It is merged into the generated
configuration rather than tacked on the end, so a setting here overrides the
same setting from the options above, and a section that already exists is added
to rather than duplicated:

```ini
[Configuration]
IconSize=75

[Radiobrowser]
RBAPIURL=http://de1.api.radio-browser.info
```

## Where your files live

The add-on's configuration folder (`/addon_configs/…_viritin/` on the host)
holds the things you curate:

- **`stations.ini`** — your own stations, grouped into categories:

  ```ini
  [Jazz]
  Radio Swiss Jazz=http://stream.srg-ssr.ch/m/rsj/mp3_128|http://example.com/logo.png
  ```

  The part after `|` is an optional logo and can be left off.

- **`podcasts.ini`** — your podcast feeds, in the same shape:

  ```ini
  [Podcasts]
  BBC The Documentary=https://podcasts.files.bbci.co.uk/p02nrsl1.rss
  ```

  Written with an example the first time podcasts are switched on.

- **`presets/<code>.ini`** — the country lists Viritin fetched, one file per
  country you enabled. These are **downloaded, not yours**: they are replaced
  whenever a fetch succeeds, so edit `stations.ini` instead if you want a change
  to stick. They are kept so a failed fetch still leaves you with a menu.

- **`avr.ini`** — per-receiver settings: which entries the main menu has,
  and filters for the Radio Browser directory. Written with sensible defaults
  on first start.

- **`<mac>.xml` / `bookmark.xml`** — bookmarks saved from the remote.

`viritin.ini` is **generated from the options above every time the add-on
starts**, so editing it does nothing. Change the options instead, or use
`advanced_ini`.

Caches and the station database live in the add-on's private storage and can be
thrown away safely; they rebuild themselves.

## Frontier Silicon table radios

Viritin is not only for AV receivers. Frontier Silicon chipsets — Hama, Medion,
Technisat, Teufel, Roberts, Pure, Sangean, Auna, Karcher and others — used vTuner
as their station directory until Frontier dropped it in May 2019. They speak the
same protocol on a slightly deeper URL, and Viritin answers it.

Setup is the same as for a receiver, with one different hostname: point
`*.wifiradiofrontier.com` at this machine instead of `*.vtuner.com`. If you use
Viritin's own DNS service, that name is already in its intercept list.

`*.frontier-nuvola.net` is deliberately left alone. Nuvola shut down on
31 October 2024, but it was handed over to airable rather than switched off, so
radios from brands that signed with airable still reach a working service under
those names — intercepting them would break a radio that works. If your brand
was dropped in that handover, that is the case for adding the name by hand.

This is tested at the protocol level but **not yet confirmed on real hardware**.

## Country presets

Turn on **Country presets** and set **Preset countries** to one or more
two-letter codes (`fi`, or `fi,se`). On start, Viritin fetches a curated list
for each and merges it into **My stations**, so the receiver has something worth
playing before you have written a station file.

Your own `stations.ini` is loaded first and always wins: a category that appears
in both is merged rather than duplicated, and a station you already have is not
added twice. A preset can never overwrite anything of yours.

If a fetch fails, the copy from last time is used and the log says so — the
failure names the URL and the reason. Which lists exist, and how to contribute
one, is in the `presets/` folder of the Viritin repository.

## Security

The add-on shares the host's network, because the receiver has to reach it
directly and because the DNS service needs to see the address a query really
came from. That means every port it opens is open on your network. Specifically:

- **The radio service on `web_port`** is meant to be reachable — that is the
  whole point — and it has no authentication. Anyone on your network can browse
  it. It is not an open proxy: the play, icon and relay endpoints take a station
  id, never a URL, so they only ever fetch what is in the catalogue.
- **The stations editor** writes configuration files, so it is off by default
  and refuses to start without a password. It is reachable from anywhere on your
  network, protected by that password and nothing else. Choose a real one.
- **The DNS service** is off by default. Leave it off unless you are using it,
  and never expose port 53 to the internet from here.

## When it does not work

**The add-on will not start.** Check the log for `Binding of socket failed`.
Something else on this machine already owns port 80 (or 53 with `dns_service`
on). Stop the other service or do the DNS redirect at your router instead.

**The receiver still shows the old error.** It is still resolving the vTuner
hostname to the real thing. Confirm the redirect, then power-cycle the receiver
— they cache DNS answers, sometimes for a long time.

**Menus appear but nothing plays.** Turn on `resolve_playlists` if it is off.
If the station's address begins with `https`, turn on `relay_https`; these
receivers cannot do TLS at all. This is the usual reason podcast episodes list
but will not play.

**A whole category is empty.** Check the filters in `avr.ini`. The examples in
Viritin's own `cfg/avr.ini` include a demonstration tag filter that hides almost
everything — this add-on deliberately does not copy that file for exactly that
reason, but if you pasted from it, that is the likely cause.
