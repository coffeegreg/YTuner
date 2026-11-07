# Spotify on a receiver that never got Spotify Connect

Receivers of this vintage split into two groups. Some got a firmware update that
added Spotify Connect and can be picked from the Spotify app directly. Plenty
never did, and for those the app offers nothing — the amplifier is invisible to
it.

This page describes how to give one of those receivers Spotify anyway, using a
part Viritin does not supply. It is worth being straight about the division of
labour:

**Viritin does not speak Spotify's protocol, and this recipe does not make it.**
A separate program logs in to Spotify, appears in the app as a speaker, and
republishes what it is told to play as an ordinary HTTP audio stream. Viritin's
job is the last hop: putting that stream in the receiver's menu as a station it
can select, and — if the bridge is not on the same network — fetching it on the
receiver's behalf.

The result behaves like a radio station that happens to play what you choose
from your phone.

## What you need

- **Spotify Premium.** Every one of these clients is built on Spotify's Connect
  protocol, which Premium gates. There is no free-tier version of this.
- **A machine that is on when you want music** — the same one running Viritin is
  fine. A Raspberry Pi is enough; this is a decode and a re-encode, not much
  more.
- **Docker, or a willingness to run two or three small services.**

One caveat that has nothing to do with Viritin and cannot be worked around:
Spotify has been restricting third-party Connect clients, and accounts created
from roughly 2024 onwards are reported to fail against them. If your account is
recent, test the bridge on its own before building anything on top of it.

## The pieces

```
  Spotify app  ──Connect──▶  bridge  ──PCM──▶  encoder  ──▶  Icecast  ──HTTP──▶  AVR
                          (go-librespot)      (ffmpeg)                       (via Viritin's menu)
```

**The bridge** is [go-librespot](https://github.com/devgianlu/go-librespot) or
[librespot](https://github.com/librespot-org/librespot). Both advertise
themselves on the network with zeroconf, which is the part that matters for
someone who does not want to configure anything: the device simply appears in
the Spotify app's speaker list, and picking it is the whole setup. go-librespot
can write raw PCM to a named pipe, which is what makes the next step possible.

**The encoder** is ffmpeg, turning that PCM into MP3 and pushing it at Icecast.

**Icecast** serves the result as a mount point — a plain HTTP URL, exactly the
shape of an internet radio station.

**Viritin** lists that URL as a station.

[Spotycast](https://github.com/chourmovs/spotycast) packages the first three
into a single container and is the least work if it fits your setup. The recipe
below spells the parts out so you can see what each one is doing.

## The recipe

Point go-librespot at a pipe:

```yaml
# go-librespot config.yml
device_name: Living Room
audio_backend: pipe
audio_output_pipe: /tmp/spotify.pcm
```

Encode that pipe into an Icecast mount:

```sh
ffmpeg -f s16le -ar 44100 -ac 2 -i /tmp/spotify.pcm \
       -c:a libmp3lame -b:a 320k \
       -ice_name "Spotify" -ice_genre "Various" \
       -content_type audio/mpeg \
       -f mp3 icecast://source:PASSWORD@127.0.0.1:8000/spotify
```

Then add the mount to Viritin's stations file:

```ini
[Spotify]
Spotify=http://192.168.1.10:8000/spotify
```

Use the machine's address rather than `localhost`: the receiver has to reach it.
Reload the stations file (or restart Viritin) and the entry appears under **My
Stations**.

To listen: select the station on the amplifier, then pick the bridge in the
Spotify app and press play. The order matters less than it sounds — the station
will sit silent until something is playing.

## Track titles on the display

An Icecast mount can carry the current title in the stream, and these receivers
display it. ffmpeg cannot supply it, because it only ever sees PCM and has no
idea what the track is. The title has to be pushed to Icecast separately:

```sh
curl -u admin:PASSWORD \
  "http://127.0.0.1:8000/admin/metadata?mount=/spotify&mode=updinfo&song=Artist%20-%20Title"
```

go-librespot exposes the currently playing track over an HTTP API with a
WebSocket event stream, so a short script that listens for track changes and
issues that call is all it takes. Without it the display shows the station name
and nothing else, which is a cosmetic loss rather than a functional one.

## Where Viritin's relay comes in — and where it does not

If the bridge is on your own network, the receiver fetches the mount **directly**
and Viritin is not in the audio path at all. It supplied the menu entry and then
got out of the way. That is the normal case and the one to prefer.

The relay (`RelayHTTPS=1`) matters when the bridge is somewhere the receiver
cannot reach as-is — typically a VPS, where you would not want the mount exposed
over plain HTTP. Then Viritin fetches the stream and re-serves it locally, and
the work it does on that path is worth knowing about:

- The real content type and the station's `icy-name`, `icy-genre` and `icy-br`
  are passed through rather than guessed at.
- Interleaved track titles are forwarded, but only when the receiver asked for
  them.
- If the bridge restarts, the fetch is retried — up to five times, with the
  allowance renewed whenever anything at all arrives — so a bridge that comes
  back within a few seconds does not cost you the station.

That last point has a deliberate exception, and it is the one trade-off in the
design. A stream carrying track titles **is not resumed**. The metadata interval
counts bytes from the start of a response, so reconnecting restarts that count
upstream while the receiver keeps counting from where it was, and every later
title would be played as audio instead of displayed. Dropping the connection
lets the receiver re-request the station cleanly. In practice: with titles, a
bridge restart means re-selecting the station; without them, it recovers on its
own.

## What this will not do

**The receiver's transport buttons do nothing useful.** It thinks it is playing
a radio station. Skip and pause are the phone's job. Stop on the amplifier just
disconnects.

**There is a delay of several seconds** between pressing play and hearing sound,
and again on every track change — Icecast buffers, and so does the receiver.
Fine for listening, poor for browsing your library from across the room.

**A paused stream may end the connection.** When Spotify is paused the bridge
stops producing audio, ffmpeg stalls, and Icecast eventually drops the source.
That is a property of this chain, not of Viritin, and the fix belongs at the
Icecast end: a `<fallback-mount>` serving silence keeps the mount alive and the
receiver connected through a pause. Configure it if you pause often.

**The clients are unofficial.** Spotify does not support them, has restricted
them before, and can again. This is a way to get more life out of hardware you
already own, not a product guarantee.

## On the roadmap

The recipe above is entirely outside Viritin today, and it should probably stay
that way — bundling a Spotify client would tie the project's fate to an
unofficial reimplementation of someone else's protocol. What Viritin can
reasonably do is make the join less manual: detect a local bridge, generate the
station entry, and relay track titles from its API to the display without the
glue script. Music Assistant's newer Spotify pairing approach is worth studying
before committing to any of it.
