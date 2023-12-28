![AVR](img/avr.png)  

# YTuner

YTuner is a simple project inspired by [YCast](https://github.com/milaq/YCast) but rewritten from scratch.
Designed to replace vTuner internet radio service and dedicated to all users of AVRs made by Yamaha, Denon, Onkyo, Marantz and others with built-in vTuner support.
If you own one (or even more) of the vTuner-enabled AVRs mentioned above and want to enjoy free internet radio stations on your device as before, and be sure that your device's streaming service won't suddenly end, you should consider using YTuner.

### Why

YCast is a great project, but my goal was to run a similar service on a very low-spec platform where python along with packages turned out to be too heavy and too slow.
Now, with YTuner you can enjoy similar functionality at full speed of ultra lightweight native app on operating systems such as:

* Windows, Linux, macOS, BSD, Solaris, Raspberry Pi OS 

and with selected CPU architecture:

* Intel i386, AMD64/x86_64, ARM/ARM64, PowerPC/PowerPC64, SPARC/SPARC64

or any others powered by cross-build abilities of [Free Pascal Compiler](https://www.freepascal.org/).

## Features

YTuner supports :
* Custom stations list files (aka MyStations) : YAML files (YCast compatible) or INI files.
* Great [Radio-browser.info](https://www.radio-browser.info) functionality.
* AVR bookmarks. Single bookmark for many AVRs or dedicated bookmark for each AVR (if you own more then one) with support of "add" and "del" operations sent from AVR devices. 
* Easy application configuration with ini files. 
* Optional SSL support for YTuner HTTPS web request.
* Radio stations logo images conversion/resize on the fly with couple of supported image formats (JPG,PNG,GIF,TIFF-optional) 
* Radio stations logo images optional cache.
* Radio browser UUIDs, data structures and stations cache (based on files or RAM storage) with optional auto refresh.
* Radio browser advanced filtering and sorting (single config for many AVRs or dedicated configs for each AVR (if you own more then one))

YTuner also has build in :
* Web service to support AVR requests.
* Optional DNS proxy service to intercept vtuner.com related DNS queries and others if needed.
* Maintenance service.

## Supported devices
***Theoretically, YTuner should work with most AVRs that support vTuner.***  
Now, the list of supported **and tested** devices below is short, but I hope it will expand with your help.

***Please test YTuner with your AVR and give me your feedback***. 

### Confirmed working
- Yamaha
  * Yamaha RX-V671
  * Yamaha RX-V673
  * Yamaha DSP-Z7 (Tested by Beatrice / [TheBossME](https://github.com/TheBossME). Thank you.)
- Marantz
  * Marantz NR1607 (Tested by [brietman](https://github.com/brietman). Thank you.)
- Denon
  * Denon AVR-X3300W (Tested by [citronalco](https://github.com/citronalco). Thank you.)

## Installation
YTuner is a standalone application and in most cases it does not require additional services, frameworks, packages, virtual machines, libraries or tools to run properly (except optional OpenSSL libraries).
You can download from [Releases](https://github.com/coffeegreg/YTuner/releases) a file specific to your operating system and CPU architecture or build YTuner from source (look at [Build](README.md#build) section).

After download (or build) save and extract files into prepared directory with granted read/write/execute privileges.
Remember that the credentials who will run YTuner must also have permission to open TCP port 80 and optionally UDP 53 (note below).

Now, you should have directory with some of the following subdirectories and files :

```
-- ytuner
 |-- config (subdir for config files) 
   |-- stations.ini  (if you want to use a ini file with your favorite radio stations) 
   |-- stations.yaml (if you want to use a yaml/yml file with your favorite radio stations)
   |-- avr.ini (common configuration file for all your AVRs)
   |-- bookmark.xml (common bookmark file for all your AVRs - only if one of your AVR support bookmark)
   |-- ...... (AVRs dedicated bookmark and config files)
 |-- cache (subdir for cache files)
   |-- rbuuids.txt (Radio browser UUIDs cache file)
   |-- ...... (other cache files)
 |-- ytuner (or ytuner.exe for Windows)
 |-- ytuner.ini (YTuner important config file)
```
 Do not forget to add execute privileges to `ytuner` on linux/*nix systems with a command like `chmod +x ytuner`.  


### OpenSSL
If you want to use SSL to support YTuner HTTPS web request you have to get OpenSSL libraries.
- Most linux/*nix systems install OpenSSL by default. Otherwise, use your favorite package manager to get OpenSSL libraries or download them from [Github](https://github.com/openssl/openssl) or visit [OpenSSL Wiki](https://wiki.openssl.org/index.php/Binaries) for binary distributions source.   
- Windows users can download them from [Github](https://github.com/openssl/openssl) (follow [NOTES-WINDOWS.md](https://github.com/openssl/openssl/blob/master/NOTES-WINDOWS.md) instructions) or visit [OpenSSL Wiki](https://wiki.openssl.org/index.php/Binaries) for binary distributions source.
Make sure to get/build the correct version of the OpenSSL libraries with the correct bit length for your OS. 32-bit libraries are needed if you chose to use the 32-bit version of YTuner or 64-bit for the AMD64/x86_64 version of YTuner.
Finally, you should have 2 files:
  * OpenSSL 1.0.2 and earlier:
     + `ssleay32.dll` (or `libssl32.dll`) and `libeay32.dll`
  * OpenSSL 1.1.0 and above:
     + 64-bit: `libssl-1_1-x64.dll` and `libcrypto-1_1-x64.dll`
     + 32-bit: `libssl-1_1.dll` and `libcrypto-1_1.dll`
  * OpenSSL 3.0.0 and above:
     + 64-bit: `libssl-3-x64.dll` and `libcrypto-3-x64.dll`
     + 32-bit: `libssl-3.dll` and `libcrypto-3.dll`

and place them in your `ytuner` directory or anywhere in your system `PATH`.
Make sure your system has valid CA certificates.
>Tip: The YTuner should work with LibreSSL libraries as well.

## Configuration

Your YTuner machine and AVR(s) have to have internet access. Make sure your firewall is properly configured if necessary.
### AVR
Set all DNS servers on your AVR config to your YTuner machine IP address.

### Router
Make sure that your YTuner machine is assigned a static IPv4 address.

### YTuner Web Service
Regardless of what operating system you use, you need to make sure that TCP port 80 is not being used by another application.
YTuner has a built-in multi-threaded web server that listens on TCP port 80 so you don't have to worry about its configuration and performance.
>Tip: In some special cases, it may be necessary to change the default TCP port 80 to another. You can do this by editing the YTuner ini file. See [Application configuration](README.md#application-configuration) section below.

### YTuner DNS Service
YTuner has a built-in multi-threaded DNS proxy server that listens on UDP port 53. This feature is optional and you can simple disable it and/or configure by editing configuration .ini file `ytuner.ini` (See [Application configuration](README.md#application-configuration) section below).
You can also use your favorite DNS server like `dnsmasq`.  
***Most important is to point `*.vtuner.com` domain to you YTuner machine and set all DNS servers on your AVR config to your YTuner machine IP address.***  
>Tip: In some special cases, it may be necessary to change the default UDP port 53 to another. You can do this by editing the YTuner ini file. See [Application configuration](README.md#application-configuration) section below.

### YTuner Maintenance Service
YTuner has a built-in maintenance service for diagnostic and future goals. 
At this moment you can use it to shut down YTuner service only.
>Tip: In most cases, you will not need this functionality. See [Application configuration](README.md#application-configuration) section below.

### Application configuration
YTuner is configured by simple `ytuner.ini` file.  
This file has the following capabilities:
https://github.com/coffeegreg/YTuner/blob/76eb487cb339d00b475a4c26529ac0fce589594a/cfg/ytuner.ini#L1-L127

YTuner's filtering and sorting functionality can be oriented by AVR device.
You can decide about it with `CommonAVRini` setting of `ytuner.ini`.
Common AVR config file `avr.ini` or other AVRs dedicated config files have the following capabilities:
https://github.com/coffeegreg/YTuner/blob/273e48b94f4c1b3bd33077905ae1f4fa8ca3ee95/cfg/example-avr.ini#L1-L78

_Please read the descriptions in both `.ini` files carefully._
### Custom stations
You can enable support for the stations list local file. Two types of files are supported:
* .ini file :
```
[Category one name]
  Station one name=http://url-of-station-one|http://url-of-station-one-logo
  Station two name=http://url-of-station-two|http://url-of-station-two-logo

[Category two name]
  Station three name=http://url-of-station-three|http://url-of-station-three-logo
  Station four name=http://url-of-station-four|http://url-of-station-four-logo
``` 
* .yaml / .yml file :
```
Category one name:
  Station one name: http://url-of-station-one|http://url-of-station-one-logo
  Station two name: http://url-of-station-two|http://url-of-station-two-logo

Category two name:
  Station three name: http://url-of-station-three|http://url-of-station-three-logo
  Station four name: http://url-of-station-four|http://url-of-station-four-logo
```
YTuner can convert and resize on the fly logo image from JPEG, PNG, GIFF and TIFF (optionaly) to JPEG (default) or PNG format. 
>Tip: URLs with logo station images are optional.

### Bookmark
What is the `Bookmark` ? `Bookmark` is what is mentioned in the AVR user's manual. `Bookmark` is operated only from the AVR device using the remote control. When you listen to a new station you can decide to put it into the `Bookmark` or want to remove it from it. All stations added in this way are visible in the `Bookmark` submenu of the AVR receiver.

If you AVR support `Bookmark` you can enable and use this YTuner functionality.
You can configure YTuner to use one common bookmark file (`bookmark.xml`) for all your AVR devices (if you have more then one) or each AVR will own its own bookmark file. 
See [Application configuration](README.md#application-configuration) section above.

## Running the application
### Windows
Simply execute `ytuner.exe`. 

### Linux / Unix (Solaris, BSD) / macOS
If you credentials meet all requirements mentioned above just go to your ytuner directory and start application or simple use `sudo` to execute application: 
```
$ sudo ./ytuner
```
If you `MessageInfoLevel` parameter from `ytuner.ini` has a value greater then 0 and Radio-browser.info with stations list local file support are enabled, you should see something like this :
```
2023-04-25 20:36:38 : Inf : Starting services...
2023-04-25 20:36:38 : Inf : Getting Radio-browser.info UUIDs...
2023-04-25 20:36:38 : Inf : Successfully loaded 10 my stations.
2023-04-25 20:36:38 : Inf : DNS server listening on: 192.168.1.2:53.
2023-04-25 20:36:38 : Inf : Web server listening on: 192.168.1.2:80.
2023-04-25 20:36:42 : Inf : Successfully downloaded 37542 RB UUIDs.
2023-04-25 20:36:42 : Inf : Successfully saved 37542 RB UUIDs to cache file.
```
Now you can see Radio-browser.info cache file `rbuuids.txt` in your ytuner directory.  
If you have enabled the radio station icon cache `IconCache=1` and started browsing radio stations with your AVR, you can see the `cache` subdirectory and the files inside.  
If you AVR support bookmarks and you add some station(s) you should see bookmark file(s).  
It is possible to add or remove stations from one AVR and use this bookmark as a catalogue in another AVR (even without bookmark support).

## Build
You can use [CodeTyphon Studio](https://www.pilotlogic.com) or [Lazarus Free Pascal RAD IDE](https://www.lazarus-ide.org/) to build YTuner.  
Use the latest versions of these IDE. Relevant project files are included.

### Dependencies
YTuner uses [Indy - Internet Direct](https://github.com/IndySockets/Indy) library to build its own binary files. Of course, YTuner binaries no longer need any additional libraries beyond the optional OpenSSL.
>Important: Use the latest version of Indy library to build YTuner.

## License
YTuner is licensed under MIT license.
See the [license.txt](LICENSE.txt) file for more details.
