# Devices: what is known, and what needs testing

The list in the README is a list of devices *people happened to own*. It is not
a survey, and its shape says more about who has filed a report than about which
hardware can work. This page is the other half: what can be established without
a device in hand, and what still needs someone to try it.

## What actually decides whether a device works

Two things, and only two.

1. **The hostname its firmware asks for** has to be one Viritin can be pointed
   at. That is `InterceptDNs`, or an equivalent override on your own resolver.
2. **The request path shape** has to be one Viritin serves. There are two, and
   both are covered:

   ```
   AVRs               /setupapp/<vendor>/loginxml.asp
   Frontier Silicon   /setupapp/<vendor>/asp/BrowseXML/loginXML.asp
   ```

Everything else — brand, model year, whether the vendor's own portal is dead or
merely paywalled — follows from those two.

## What DNS says about which brands vTuner provisioned

`vtuner.com` has a **wildcard record**. `nad.vtuner.com`, `rotel.vtuner.com` and
`zzzz-no-such-oem.vtuner.com` all resolve to the same address, so a brand
resolving under vtuner.com proves nothing at all. What does mean something is a
name with its **own** A record, pointing where another known vTuner host points.

By that test, these have their own records — vTuner provisioned something for
them:

| address | names with their own record |
|---|---|
| 8.38.76.252 | `denon.vtuner.com`, `marantz.vtuner.com`, `radiomarantz.com`, `radiodenon.com` |
| 23.238.115.210 | `yamaha.vtuner.com`, `yradio.vtuner.com` |
| 192.227.77.203 | `onkyo.vtuner.com`, `radiosetup.com` |
| 75.126.113.179 | `pioneer.vtuner.com`, `technics.vtuner.com`, `arcam.vtuner.com`, `samsung.vtuner.com`, `teac.vtuner.com`, `harmankardon.vtuner.com` |
| 192.227.85.83 | `revox.vtuner.com`, `radioharmankardon.com` |

Two of those are why `radiodenon.com` and `radioharmankardon.com` are now in the
default `InterceptDNs`: each is a separate portal name sharing an address with a
host already known to be vTuner's. **That is infrastructure evidence, not a
device report.** Nobody has yet confirmed a receiver asking for either name.

`radiotechnics.com` resolves, but to an address that matches nothing else here,
so it is not in the list. `radiopioneer.com` and `radiolg.com` resolve to what
look like parking or CDN addresses. Those need a device, not more guessing.

What could not be checked from here: whether any of these hosts still *answer*.
The development environment cannot make outbound HTTP to them, so every claim
above is about DNS only.

## Wanted: these devices, tested

**Brands with their own vTuner record that nobody has reported.** These are the
best candidates — the infrastructure existed, and `*.vtuner.com` already covers
them, so they may work today with no change at all:

- **Technics** — `technics.vtuner.com` has its own record, and Panasonic
  [announced the end of the Technics vTuner feature on 30 November 2022](https://help.na.panasonic.com/answers/notice-discontinuation-of-technics-vtuner-feature/),
  after the service had already stopped in mid-August. Named models: **SU-G30,
  ST-C700D, SC-C500, SU-C550, SC-C70, ST-G30**, and the OTTAVA range.
- **Arcam** — `arcam.vtuner.com`. Arcam is among the brands that
  [moved to Airable](https://hifinews.com/content/internet-radio), which is what
  a vendor does when the old directory stops.
- **Samsung** — `samsung.vtuner.com`. Which product line is unknown; likely the
  networked audio and Blu-ray range rather than televisions.

**Brands documented as vTuner users, with no DNS evidence either way.** The
wildcard means their `<brand>.vtuner.com` tells us nothing, so their firmware
may ask for something else entirely:

- **Cambridge Audio**, **Rotel**, **NAD**, **Naim** — reported alongside Arcam
  as having moved to Airable.
- **LG** — [discontinued vTuner on 28 October 2015](https://www.lg.com/us/support/help-library/vtuner-service-discontinuation--20154860341006),
  the earliest cut-off found.
- **Philips**, **Loewe**, **Grundig**, **Magnat**, **Block**, **Sonoro** —
  European hi-fi and table-radio brands of the right era. Unverified.

**Already covered, unconfirmed on hardware.** The Frontier Silicon radios — Hama,
Medion, Technisat, Teufel, Roberts, Pure, Sangean, Auna, Karcher, Silvercrest.
The deeper path is served and asserted by `script/smoke-test.sh`, and
`*.wifiradiofrontier.com` is intercepted, but no one has reported a full browse
on a real radio.

**Out of scope: Reciva.** Grace Digital, Ocean Digital, Como Audio, Crane and
some Sangean radios used **Reciva**, not vTuner. Reciva shut down in January
2021 and its devices are just as dead, but the protocol is a different one and
Viritin does not speak it. Worth knowing before you buy something second-hand
hoping this will revive it.

## How to test a device, and what to report

The useful part of a report is **the hostname the device asks for**. You do not
have to capture packets to find it — Viritin will tell you.

1. Run Viritin with `[DNSServer] Enable=1` and `MessageInfoLevel=4`.
2. Point the device's DNS at the Viritin machine. On an AVR that usually means
   turning DHCP off in its network menu and entering the address by hand.
3. Press the internet-radio button and watch the log.

```
DNS Query intercept : radioyamaha.vtuner.com     <- a name already handled
DNS query not intercepted: something.example.com <- a name nobody has reported
```

The second line is the one worth sending. Anything a receiver asks for that is
not on the list is either ordinary traffic or a directory this project has never
heard of, and from inside the server those look identical — a human has to look.

Then, whatever happened:

- the exact model, and the hostname from the log;
- whether the menu appeared, and whether a station played;
- if the menu appeared but was empty, the log lines at `MessageInfoLevel=4` for
  the request that came back empty.

A report that says "it did not work" and names the model is still worth sending.
Half the entries in the README started that way.

## Driving a Denon from its own web interface

Denon receivers of this era serve a web remote (`/NetAudio/index.html`, GoAhead)
that drives the *receiver's* network-audio browser. When the source is Internet
Radio that browser is showing Viritin's menu, so this is the fastest way to see
what an AVR actually sees without standing in front of it. `/remote` in the Web
GUI does exactly this; what follows is what it cost to learn.

Verified against an AVR-4520 (2012, `CommApiVers 0210`) and an AVR-X3600H (2019,
`0301`).

**A POST to `AppCommand.xml` must have a newline after the XML declaration.**
Without one the receiver answers `200 OK` with an empty `<rx></rx>` — no error,
no clue. It is not the content type, not the quote style, not the number of
commands; both receivers behave identically. An afternoon went into "this model
refuses single-command requests" and "telnet holds block AppCommand", and both
were this, in a hand-written probe file.

```
<?xml version="1.0" encoding="utf-8"?>\n<tx>...   answers
<?xml version="1.0" encoding="utf-8"?><tx>...     <rx></rx>
```

**The endpoints worth knowing.**

| endpoint | what |
|---|---|
| `GET /goform/Deviceinfo.xml` | model, `CommApiVers`, `DeviceZones`, capabilities |
| `GET /goform/formNetAudio_StatusXml.xml` | the browse list: `szLine` and `chFlag` |
| `POST /NetAudio/index.put.asp` | cursor and search, as `cmd0=` + `cmd1=` |
| `POST /goform/AppCommand.xml` | zone power, source, volume, mute, surround |

`cmd0` takes `PutNetAudioCommand/` + one of `CurUp CurDown CurLeft CurRight
CurEnter CmdPageUp CmdPageDown CmdStop PresetCall PresetMemo`, or
`PutNetFuncSearchiRadio/<keyword>` for a keyword search. `cmd1` is
`aspMainZone_WebUpdateStatus/`. Values are percent-encoded — that is what the
receiver's own jQuery sends, and what it accepts. Encode the whole `cmd0` value
exactly once: `TFPHTTPClient.FormPost` encodes what you hand it, so a term
encoded on the way in arrives as `rock%2520%2526%2520roll`.

`AppCommand.xml` takes at most five `<cmd>` per `<tx>`, but several `<tx>` roots
concatenated in one body are accepted. Commands the receiver does not support
come back as `<error>2</error>`, positionally aligned with the query, so results
can be matched by index.

**`szLine` is always ten entries.** The trailing ones are empty padding, and one
of the ten is a page counter like `[    1/7  ]` rather than something
selectable. Treat "every non-empty line" as the list and you offer the page
counter as a station that does nothing when pressed. Also: the cursor keys do
nothing on the Now Playing screen — `CurLeft` backs out to the list first.

**`chFlag` is a bitmask, not a value.** Captured from a 4520 showing search
results for "helsinki":

| line | `chFlag` | |
|---|---|---|
| `Search by Keyword` | `0` | a heading; not selectable |
| `Radio Dei Helsinki` | `9` | selectable, and where the cursor is |
| `Radio Helsinki` and the rest | `1` | selectable |

So bit 0 means selectable and bit 3 means cursor, and the line under the cursor
on any ordinary list is `9`. Testing `chFlag = 8` therefore finds the cursor
only on a screen whose items are *not* selectable, which is no list worth
browsing — Viritin shipped that for one release and highlighted nothing.

Now Playing reports `0` for every line, cursor included, since nothing there is
selectable. Code that walks the cursor to a line has to treat "no cursor on this
screen" as a reason to stop rather than a starting point of zero: stepping from
a position nobody knows moves the selection somewhere nobody chose, and on a
list Enter then plays it.

**Zone commands are not harmless probes.** Setting a zone's source powers that
zone on: an HTTP `Z2CD` produced a `Z2ON` event and started the outdoor
speakers. There is no inaudible way to test zone commands on a live system.

**Telnet (port 23) is one session, exclusive.** A second connection is refused,
so if Home Assistant has `use_telnet` on it holds the slot and nothing else can
connect — "connection refused" here means *in use*, not absent. The reconnect
cooldown is under half a second: an immediate reconnect after closing is
refused, 0.5 s later succeeds, which is worth knowing for anything that
reconnects in a loop. Network Standby `Always On` does *not* gate telnet — the
4520 still refused with it set — but it does decide whether the receiver answers
at all in standby, and therefore whether it can be powered on over the network.

**The port map differs by generation**, which is what the AVR-X / AVR-X-2016
split is about:

| | AVR-4520 | AVR-X3600H |
|---|---|---|
| API port | 80 | 8080 (80 serves nothing) |
| 8080 `/description.xml` | instant | instant |
| 8080 `/goform/*` | accepts the connection, then never answers | serves normally |
| 60006 | closed | open |

The 4520's 8080 is a trap for anything that probes `/goform` there: the socket
opens, so it is not a refusal, and the request hangs until it times out.

**`Deviceinfo.xml` sometimes lists the real surround modes.** The 4520 publishes
15 by name under `SurroundMode`; the X3600H publishes none. Anything that wants
to offer only the modes a receiver actually has can use that on older models and
must fall back on newer ones.
