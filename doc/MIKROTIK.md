# Viritin in a MikroTik RouterOS container
If you own a MikroTik router with container support, you don't need a separate machine for Viritin at all. The router already is the DNS server of your network, so it can point `*.vtuner.com` to a Viritin container running on the router itself.
## Assumptions
- RouterOS v7 on an ARM64 (e.g. RB5009, hAP ax, CCR2004) or x86/CHR device. Tested on RB5009 with RouterOS 7.24.
- A USB/NVMe disk attached to the router (ext4). The image is ~100MB and Viritin writes cache and DB files, so internal flash is not a good place for it.
- Default MikroTik LAN `192.168.88.0/24` on interface `bridge`, with the router at `192.168.88.1`. The container will get `192.168.88.5`. Adjust to your network and pick an address outside your DHCP pool.
- AVRs use the router as their DNS server (default when they get their address from the router's DHCP server).

The files used below are in the [mikrotik](../mikrotik) directory of this repository:
- `Dockerfile` - compiles Viritin from this repository's source with `script/build.sh` and packs it in a Debian bookworm-slim based image with SQLite3 and OpenSSL libraries. Default config files come from `cfg/`.
- `entrypoint.sh` - on first start, copies the default `viritin.ini` and config files to `/data`, then runs Viritin.
- `build.sh` - builds the image and saves it as a tar file ready for RouterOS.

All Viritin state (`viritin.ini`, `config`, `cache`, `db`) lives in `/data`, which is mounted from the router's disk. Updating or recreating the container keeps your stations, bookmarks and database.

The image's default `viritin.ini` differs from `cfg/viritin.ini` in:
```
CacheFolderLocation=/data
ConfigFolderLocation=/data
DBFolderLocation=/data
MessageInfoLevel=1
[DNSServer]
Enable=0
```
Viritin's own DNS service is disabled because RouterOS static DNS entries do the job.
## That's easy
### 1) Build the image
On any x86_64 Linux machine with Docker (buildx), no ARM emulation needed:
```
$ cd mikrotik
$ ./build.sh arm64
...
-rw------- 1 user user 108M retuner-arm64.tar
```
The first build takes a few minutes, because it builds an ARM64 cross compiler from the Free Pascal 3.2.2 release (Debian ships only the native one). Later builds reuse it from the Docker build cache.

Use `./build.sh amd64` for x86/CHR routers, or to test the image on your PC first:
```
$ docker run --rm -p 8080:80 -v /tmp/retuner-data:/data retuner:amd64
```
### 2) Install the container package
Download the `container` package matching your RouterOS version and architecture and reboot:
```
/tool fetch url="https://download.mikrotik.com/routeros/7.24.4/container-7.24.4-arm64.npk"
/system reboot
```
Check that it's installed with `/system package print`.
>Tip: Do the reboot of this step with `/system reboot` and not by pulling the power cable. A freshly downloaded package may not be written to flash yet and will then be silently lost.
### 3) Enable container device-mode
```
/system device-mode update container=yes
```
Within 5 minutes, confirm this by pressing the reset/mode button or by cold power cycling the router (unplug power for a few seconds). Check with `/system device-mode print` that `container: yes`.
### 4) Network for the container
Give the container its own address in your LAN by putting its veth interface in the LAN bridge:
```
/interface veth add name=veth-retuner address=192.168.88.5/24 gateway=192.168.88.1
/interface bridge port add bridge=bridge interface=veth-retuner
```
### 5) Storage and mount
```
/file add name=usb1/retuner type=directory
/file add name=usb1/retuner/data type=directory
/file add name=usb1/retuner/tmp type=directory
/container config set tmpdir=usb1/retuner/tmp
/container mounts add list=retuner src=/usb1/retuner/data dst=/data
```
### 6) Upload the image and create the container
Upload the tar file to the router, e.g. with scp (or drag&drop in WinBox Files):
```
$ scp retuner-arm64.tar admin@192.168.88.1:usb1/retuner/retuner-arm64.tar
```
Create and start the container:
```
/container add name=retuner file=usb1/retuner/retuner-arm64.tar interface=veth-retuner root-dir=usb1/retuner/root mountlists=retuner hostname=retuner logging=yes start-on-boot=yes
/container start retuner
```
With `logging=yes`, Viritin output shows up in the router log:
```
/log print where topics~"container"
...
container,info,debug retuner: *** started /entrypoint.sh
container,info,debug retuner: Viritin - a fork of YTuner 1.2.6 by Greg P. (MIT) - https://github.com/ollisulopuisto/viritin
container,info,debug retuner: 25-10-26 15:28:37 : Inf : Starting services...
container,info,debug retuner: 25-10-26 15:28:37 : Inf : Successfully loaded 10 my stations.
container,info,debug retuner: 25-10-26 15:28:37 : Inf : Checking local database.
container,info,debug retuner: 25-10-26 15:28:37 : Inf : Local database is ready.
container,info,debug retuner: 25-10-26 15:28:37 : Inf : Web Service: listening on: 192.168.88.5:80.
```
On the very first start, Viritin logs `Preparing local database` and downloads the whole Radio-browser.info database first, which takes about 5 minutes on an RB5009. Once the container is running, the tar file and the `tmp` directory are no longer needed:
```
/container config set tmpdir=""
/file remove usb1/retuner/tmp
/file remove usb1/retuner/retuner-arm64.tar
```
Check it from any machine in your LAN:
```
$ curl 'http://192.168.88.5/setupapp/yamaha/asp/BrowseXML/loginXML.asp?token=0'
<EncryptedToken>0123456789ABCDEF</EncryptedToken>
```
### 7) Point vTuner domains to the container
Add a static DNS entry for the vTuner host your AVR uses (e.g. `denon.vtuner.com`, `yamaha.vtuner.com`), or one regexp entry for all of them:
```
/ip dns static add name=denon.vtuner.com address=192.168.88.5
```
or
```
/ip dns static add regexp="(^|\\.)vtuner\\.com\$" address=192.168.88.5
```
and flush the DNS cache:
```
/ip dns cache flush
```
Then power cycle your AVR, so it forgets any cached DNS answers, and open its internet radio.
## Configuration and updates
Edit `viritin.ini` and your stations file in `usb1/retuner/data` (WinBox Files, SFTP or SMB share) and restart the container:
```
/container stop retuner
/container start retuner
```
To update Viritin, build a new tar file, upload it and recreate the container with the same options as in step 6. Your `/data` stays untouched:
```
/container stop retuner
/container remove retuner
/container add name=retuner file=usb1/retuner/retuner-arm64.tar interface=veth-retuner root-dir=usb1/retuner/root mountlists=retuner hostname=retuner logging=yes start-on-boot=yes
/container start retuner
```
With `RBCacheTTL=0` the station database is never refreshed. To get a fresh copy of Radio-browser.info, remove it before starting the container, and Viritin builds a new one:
```
/file remove usb1/retuner/data/db/rb.db
```
### Coming from a YTuner container
Keep your YTuner container's mount list and point the new container at the same `/data`. On first start the entrypoint renames `/data/ytuner.ini` to `viritin.ini`, keeping every setting, and the existing station database, config files and bookmarks are used as they are. `/ytuner/` URLs your AVR stored are still served.

To keep a way back, stop the old container rather than removing it, and use a new `root-dir` for the new one (both can use the same veth interface, as long as only one runs):
```
/container stop ytuner
/container set ytuner start-on-boot=no
/container add name=retuner file=usb1/ytuner/retuner-arm64.tar interface=veth-ytuner root-dir=usb1/ytuner/root-retuner mountlists=ytuner hostname=retuner logging=yes start-on-boot=yes
/container start retuner
```
To go back, rename `viritin.ini` to `ytuner.ini` in the data directory, stop `retuner` and start `ytuner`.
>Tip: Radio-browser.info servers return only 1000 stations for requests without an explicit `limit`, and YTuner 1.2.6 sends none. If your database holds only about 1000 stations, remove `rb.db` as shown above and Viritin builds a complete one.
## Summary
Viritin uses about 50MB of RAM on an RB5009 with `catPermMemDB` cache and the full Radio-browser.info database (~70,000 stations), so it runs comfortably next to the usual router tasks.
Use the MikroTik documentation to learn more about containers (https://help.mikrotik.com/docs/spaces/ROS/pages/84901929/Container).
