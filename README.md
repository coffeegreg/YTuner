![AVR](img/avr.png)  

# YTuner

YTuner is a simple project inspired by [YCast](https://github.com/milaq/YCast) but rewritten from scratch.
Designed to replace vTuner internet radio service and dedicated to all users of AVRs made by Yamaha, Denon, Onkyo, Marantz and others with built-in vTuner support.
If you own one (or even more) of the vTuner-enabled AVRs mentioned above and want to enjoy free internet radio stations on your device as before, and be sure that your device's streaming service won't suddenly end, you should consider using YTuner.

### Why?

YCast is a great project, but my goal was to run a similar service on a very low-spec platform where python along with packages turned out to be too heavy and too slow.
Now, with YTuner you can enjoy similar functionality at full speed of ultra lightweight native app on operating systems such as::

* Windows, Linux, macOS, BSD, Solaris, Raspberry Pi OS 

and with selected CPU architecture:

* Intel i386, AMD64/x86_64, ARM, PowerPC/PowerPC64, SPARC/SPARC64

or any others powered by cross-build abilities of [Free Pascal Compiler](https://www.freepascal.org/).

## Features

YTuner supports :
* Custom stations list files (aka MyStations) : YAML files (YCast compatible) or INI files.
* Great [Radio-browser.info](https://www.radio-browser.info) functionality.
* AVR bookmarks. Single bookmark for many AVRs or dedicated bookmark for each AVR (if you own more then one) with support of "add" and "del" operations sended from AVR devices. 
* Easy application configuration with ini file. 
* Optional SSL support for YTuner HTTPS web request.
* Radio stations logo images conversion/resize on the fly with couple of supported image formats (JPG,PNG,GIF,TIFF-optional) 
* Radio stations logo images optional cache storage.
* Radio browser UUID cache with optional auto refresh.

YTuner also has build in :
* Web server to support AVR requests.
* Optional DNS proxy server to intercept vtuner.com related DNS queries and others if needed.

## Supported devices
***Theoretically, YTuner should work with most AVRs that support vTuner.***  
Now, the list of supported **and tested** devices below is short, but I hope it will expand with your help.

***Please test YTuner with your AVR and give me your feedback***. 

### Confirmed working
* Yamaha RX-V671
* Yamaha RX-V673

## Installation
YTuner is a standalone application and in most cases it does not require additional services, frameworks, packages, virtual machines, libraries or tools to run properly (except optional OpenSSL libraries).
You can download from [Releases](https://github.com/coffeegreg/YTuner/releases) a file specific to your operating system and CPU architecture or build YTuner from source (look at [Build](README.md#build) section).

After download (or build) save and extract files into prepared directory with granted read/write/execute privileges.
Remember that the credentials who will run YTuner must also have permission to open TCP port 80 and optionally UDP 53 (note below).

Now, you should have directory with following files :

```
-- ytuner
 |-- ytuner (or ytuner.exe for Windows)
 |-- ytuner.ini (important config file)
 |-- stations.ini  (if you want to use a ini file with your favorite radio stations) 
 |-- stations.yaml (if you want to use a yaml file with your favorite radio stations)
```
 Do not forget to add execute privileges to `ytuner` on linux/*nix systems with a command like `chmod +x ytuner`.  


### OpenSSL
If you want to use SSL to support YTuner HTTPS web request you have to get OpenSSL libraries.
* Most linux/*nix systems install OpenSSL by default. Otherwise, use your favorite package manager to get OpenSSL libraries or download them from [Github](https://github.com/openssl/openssl) or visit [OpenSSL Wiki](https://wiki.openssl.org/index.php/Binaries) for binary distributions source.   
* Windows users can download them from [Github](https://github.com/openssl/openssl) (follow [NOTES-WINDOWS.md](https://github.com/openssl/openssl/blob/master/NOTES-WINDOWS.md) instructions) or visit [OpenSSL Wiki](https://wiki.openssl.org/index.php/Binaries) for binary distributions source.
Make sure to get/build the correct version of the OpenSSL libraries with the correct bit length for your OS. 32-bit libraries are needed if you chose to use the 32-bit version of YTuner or 64-bit for the AMD64/x86_64 version of YTuner.
Finally, you should have 2 files: `ssleay32.dll` and `libeay32.dll` and place them in your `ytuner` directory or anywhere in your system `PATH`.

Make sure your system has valid CA certificates.

## Configuration

Your YTuner machine and AVR(s) have to have internet access. Make sure your firewall is properly configured if necessary.

### Web server
Regardless of what operating system you use, you need to make sure that TCP port 80 is not being used by another application.
YTuner has a built-in multi-threaded web server that listens on TCP port 80 so you don't have to worry about its configuration and performance.
>Tip: In some special cases, it may be necessary to change the default TCP port 80 to another. You can do this by editing the YTuner ini file. See [Application configuration](README.md#application-configuration) section below.

### DNS server
YTuner has a  built-in multi-threaded DNS proxy server that listens on UDP port 53. This feature is optional and you can simple disable it and/or configure by editing configuration .ini file `ytuner.ini` (See [Application configuration](README.md#application-configuration) section below).
You can also use your favorite DNS server like `dnsmasq`.  
***Most important is to point `*.vtuner.com` domain to you YTuner machine and set all DNS servers on your AVR config to your YTuner machine IP address.***  
>Tip: In some special cases, it may be necessary to change the default UDP port 53 to another. You can do this by editing the YTuner ini file. See [Application configuration](README.md#application-configuration) section below.

### Application configuration
YTuner is configured by simple `ytuner.ini` file.  
This file has the following capabilities:
https://github.com/coffeegreg/YTuner/blob/f9d5ff1b8e449dc9d22fc2aabf45d118b15db455/cfg/ytuner.ini#L1-L77

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
* .yaml file :
```
Category one name:
  Station one name: http://url-of-station-one|http://url-of-station-one-logo
  Station two name: http://url-of-station-two|http://url-of-station-two-logo

Category two name:
  Station three name: http://url-of-station-three|http://url-of-station-three-logo
  Station four name: http://url-of-station-four|http://url-of-station-four-logo
```
You can use only one format and file. YTuner can convert and resize on the fly logo image from JPEG, PNG, GIFF and TIFF (optionaly) to JPEG (default) or PNG format. 
>Tip: URLs with logo station images are optional.

### Bookmarks
If you AVR support bookmark you can enable and use this YTuner functionality.  
You can configure YTuner to use one common bookmark file (bookmark.xml) for all your AVR devices (if you have more then one) or each AVR will own its own bookmark file. 
See [Application configuration](README.md#application-configuration) section above.

## Running the application
### Windows
Simply execute `ytuner.exe`. 

### Linux / *nix
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
YTuner use [Indy - Internet Direct](https://github.com/IndySockets/Indy) library.
>Important: Use the latest version of Indy library to build YTuner.

## License
YTuner is licensed under MIT license.
See the [license.txt](LICENSE.txt) file for more details.
