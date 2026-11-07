# Other dead portals

vTuner is not the only station directory that was switched off with the hardware
still working. This page is a survey of the others: which ones Viritin's
architecture could serve, which are already served by somebody else, and which
are dead ends and why.

**Epistemic status.** This is desk research done on 2026-08-28 — public
announcements, vendor support pages, and the source of the community projects
that exist. No device was in hand and no traffic was captured for any portal
below. Where a protocol detail is claimed it comes from someone else's capture,
and it is attributed. Nothing here has been tested against Viritin.

## What decides whether a portal can be replaced

Five things, and the first one eliminates most candidates outright.

1. **It has to be a directory, not a catalogue.** vTuner told the receiver
   *where* a station lives; the audio was always a free public stream, which is
   why Radio Browser can stand in for it. The Pandora, Rhapsody, Napster and
   Deezer entries on those same receivers are dead permanently — the service was
   the licence, not the index, and no open implementation can supply that.
2. **Plain HTTP, and a hostname the firmware asks for by name.** DNS
   interception plus XML. TLS pinning or a crypto handshake turns the job from
   reimplementation into reverse engineering.
3. **The service has to be actually dead**, not merely unpopular. Intercepting
   something that still answers breaks a device that works.
4. **The hardware has to be otherwise sound**, with no upgrade path the owner
   would rather take.
5. **The owners have to be reachable.** Every device in the README's list is
   there because somebody found the project and filed a report.

## Candidates

### Reciva — largest stranded base, no drop-in fix

Qualcomm shut the Reciva portal down in 2021 and took the servers off on
13 September 2021, ending station browsing for radios from C. Crane, Grace
Digital, Sangean, Roberts, Tangent, Revo and Rotel — Rotel wrote its own
[customer FAQ](https://www.rotel.com/blog/reciva-3) about it. Dead directory,
free content, hardware that still powers on: the same shape as vTuner.

Two community efforts exist, and they went opposite ways.

[`felixalacampagne/recivaportal`](https://github.com/felixalacampagne/recivaportal)
tried what Viritin does — DNS redirection to a local HTTP server, no firmware
changes. It got the radio talking: plain HTTP, a `HEAD` to
`portal15.7803986842.com/portal/challenge?serial=…` redirecting to reciva.com,
then `GET /portal-newformat`. Then it hit a POST carrying 256-byte encrypted
blocks and an authorisation token described as DES-CBC over
`8-byte token + filename + pad + checksum`. The author never resolved the
session key, got a replacement radio in December 2021, and archived the
repository in February 2025.

[`jisotalo/reciva-radio-patching`](https://github.com/jisotalo/reciva-radio-patching/blob/main/README.md)
works, but by flashing Sharpfin firmware over an intercepted update — a
different patch per model, a bricking risk, and far past what an ordinary owner
will attempt for a kitchen radio.

**Verdict: the biggest opportunity in this document, behind one unknown.** The
pure-server approach died from an unsolved handshake, not from a proof that it
is unsolvable, and the firmware images the keys live in are widely mirrored.
Worth a time-boxed spike on the challenge/response before any commitment. If it
opens, it is a second protocol handler onto everything Viritin already does.

### Frontier radios orphaned by the airable handover

Frontier Smart Technologies shut its Nuvola service down on **31 October 2024**
and handed internet radio, podcasts and favourites to airable — but, by
airable's own account, only for **brands that entered into a service agreement**
with them. Devices from brands that did not are without a portal as of that
date.

This is the cheapest expansion available: same vendor, same chipsets, and the
deeper Frontier request path is already served and asserted in
`script/smoke-test.sh`. What is missing is knowing which hostname an orphaned
radio asks for after the handover, which needs one owner with one capture. See
the note on `frontier-nuvola.net` at the end of this page before changing
`InterceptDNs`.

### Pure Flow

Pure announced the shutdown in 2023, ending Avanti Flow, Contour, Evoke F4,
Evoke Flow, Oasis Flow, One Flow, Sensia, Sensia 200D, Siesta Flow, Sirocco 550
and Jongo speakers driven through the Pure Connect app. Pure's newer radios
moved to airable; these did not. No community server for it turned up.

Unknown protocol, unknown hostnames, no capture published by anyone. It needs a
device and an afternoon with tcpdump before it can be priced at all. Mostly a
UK and German install base.

### Aupeo! and Qualcomm AllPlay — real, but thin

**Aupeo!** stopped on 30 November 2016. It shipped on Onkyo network receivers,
Loewe, Philips and TechniSat, and Panasonic bought it for cars. It was a curated
personal-radio service rather than a plain index, so even a perfect protocol
implementation would be serving different content under an old name — and most
of the Onkyo hardware it ran on is already covered here through vTuner.

**Qualcomm AllPlay** is gone with its radio feature, stranding Panasonic's ALL
series. Small base, and the transport is AllJoyn, itself a dead standard.

## Already served, and what each one teaches

**Bose SoundTouch** is this document's strongest evidence and its clearest
warning. Bose ended the cloud on 6 May 2026, published the API documentation
first, and the community filled the gap within weeks:
[AfterTouch](https://www.gesellix.net/posts/aftertouch-bose-soundtouch/)
emulates the Bose cloud endpoints for browsing, registration and preset sync,
and [OpenCloudTouch](https://github.com/OpenCloudTouch/opencloudtouch) uses
**Radio Browser as its search provider** — the same architecture as this
project, arrived at independently, plus SoundTouchPlus, SoundCork and ÜberBöse
alongside. When a large brand announces a shutdown in public, the window is
months, not years.

**Squeezebox** is the opposite lesson. Logitech retired mysqueezebox.com in
February 2024 and it barely registered, because
[Lyrion Music Server](https://lyrion.org) — the same codebase as SlimServer, and
open since the early 2000s — was already there. Established first, the
replacement is uneventful. This is what Viritin is trying to become for vTuner.

**Slingbox** is the cautionary tale. Dish
[modified the firmware specifically to frustrate community replacement
servers](https://zatznotfunny.com/2022-11/dish-bricks-slingboxes/), and the one
tool that worked needed a hardware password obtainable only from the web
interface that was being shut down. Where a portal has a per-device secret,
it has to be extracted **before** the service goes off, not after.

## Suggested order

1. **Confirm Frontier on real hardware.** Already implemented, still unverified
   — and it is the prerequisite for reaching the orphaned post-Nuvola cohort.
2. **Time-boxed Reciva spike**, on the challenge/response only. Everything else
   about Reciva is favourable; that one answer decides it.
3. **Pure Flow**, if a device turns up.

The argument for doing any of this inside Viritin rather than as new projects is
that the expensive parts are built and tested: Radio Browser, the caches, icon
conversion, the HTTPS relay, playlist unwrapping, DNS interception, and the Home
Assistant packaging. Another dead portal is a front end onto all of it.

## A correction this survey produced

Four places in this repository stated that `frontier-nuvola.net` was "the live
successor service" and therefore deliberately not intercepted. Nuvola shut down
on 31 October 2024. The conclusion still holds and the default list is
unchanged, but for a different reason: the shutdown was a **handover**, and
radios from brands that signed with airable appear to keep reaching a working
service under those names, so intercepting them by default would break devices
that work today. Owners whose brand was dropped are the ones who may want the
name added by hand.

That "appear to" is doing real work: it rests on airable's "no action required,
devices continue to work" wording, not on a capture. Somebody with an affected
radio can settle it in a minute with `MessageInfoLevel=4` and a look at the
"DNS query not intercepted" lines.

## Sources

- [Radio World: Reciva Internet Radio Platform Shutting Down](https://www.radioworld.com/news-and-business/headlines/reciva-internet-radio-platform-shutting-down)
- [The SWLing Post: giving a Reciva radio a second life](https://swling.com/blog/2021/03/how-to-give-your-reciva-wifi-radio-a-second-life-before-the-service-closes-on-april-30-2021/)
- [Rotel: FAQs — Qualcomm Reciva service as used in some Rotel products](https://www.rotel.com/blog/reciva-3)
- [airable: Nuvola service shutdown — transition to airable](https://www.airablenow.com/fs-nuvola-shutdown/)
- [Sangean: Nuvola internet radio service transfers to airable](https://www.sangean.com/en/blog/140)
- [Pure: Flow shutdown FAQ](https://support.pure-audio.com/en/kb/articles/flow-shutdown-faq)
- [Inside Radio: audio streaming service Aupeo closes shop](https://www.insideradio.com/free/audio-streaming-service-aupeo-closes-shop/article_699d5986-9faf-11e6-b593-338cbede98d3.html)
- [Hackster: Bose opens the SoundTouch API at end of life](https://www.hackster.io/news/bose-throws-end-of-life-soundtouch-owners-a-lifeline-plans-an-offline-app-update-and-opens-the-api-e6b4a59bd94c)
- [SoundGuys: Bose SoundTouch support ending](https://www.soundguys.com/bose-soundtouch-support-ending-2026-146530/)
- [Hackaday: Slingbox getting bricked](https://hackaday.com/2022/11/08/slingbox-getting-bricked-you-have-less-than-24-hours/)
- [Lyrion Music Server](https://en.wikipedia.org/wiki/Lyrion_Music_Server)
