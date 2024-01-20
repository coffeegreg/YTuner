## SQLite library
* Windows users

You can use attached `sqlite3.dll` library or get latest version from https://www.sqlite.org/download.html.
* Linux / Unix (Solaris, BSD) / macOS users 

You should have such a library already installed by default. If you keep your OS up to date you should have a newer version of this library than required (3.33.0).
However, if for some reason you cannot upgrade the package containing the SQLite library on your OS, you should download the source code from https://www.sqlite.org/download.html (E.g. https://www.sqlite.org/2024/sqlite-autoconf-3450000.tar.gz) and build your own latest SQLite3 library.
If you are familiar with building C code, you may want to skip reading further. If not, don't worry and read on.
1) Get source code with command like :
```
$ wget https://www.sqlite.org/2024/sqlite-autoconf-3450000.tar.gz
```
2) Extract the tarball with :
```
$ tar -xvf sqlite-autoconf-3450000.tar.gz && cd sqlite-autoconf-3450000
```
3) Now, if you own 64-bit OS and want to build 64 bit SQLite library you can use:  
```
$ ./configure --prefix=your_64bit_library_target_directory
```
Change the `your_64bit_library_target_directory` to a directory of your choice, which YTuner will then be able to use, or select a directory appropriate for the libraries in your OS, e.g.:
* Debian / Ubuntu / other Debian based Linux :

`/usr/lib/x86_64-linux-gnu` or `/usr/lib/aarch64-linux-gnu` depends on your CPU arch. Check `gcc -dumpmachine` to find out.

* CentOS / RHEL / Fedora / Oracle Linux :

`/usr/lib64`

* macOS :

`/usr/lib` or `/usr/local/lib` or `/opt/local/lib`

>Tip: Please note that from macOS version Big Sur 11.0.1, the system ships with a built-in dynamic linker cache of all system-provided libraries. As part of this change, copies of dynamic libraries are no longer present on the filesystem. 

* BSD :

`/usr/local/lib`

* Solaris :

`/usr/lib/64`

> Tip: You can uninstall obsolete (< 3.33.0) package with sqlite3 library.

4) Run :
```
$ make
$ sudo make install
```
5) If the entire process went correctly, you should find a new `lib/` subdirectory in the indicated directory, containing your new SQLite 64 bit libraries:
* Linux/Unix: `libsqlite3.so.0.8.6` and/or some links like `libsqlite3.so` and/or `libsqlite3.so.0` and/or   
* macOS: `libsqlite3.0.dylib` and/or some links like `libsqlite3.0.dylib`

If for some reason you also need the 32 bit version (if your OS supports 32 bit applications), follow the above steps again but in step 3 use:

```
$ ./configure --prefix=your_32bit_library_target_directory --host=your_32bit_host_param "CFLAGS=-m32" "CXXFLAGS=-m32" "LDFLAGS=-m32"
```
Change the `your_32bit_library_target_directory` to a directory of your choice, which YTuner will then be able to use, or select a directory appropriate for the libraries in your OS, e.g.:
* Debian / Ubuntu / other Debian based Linux :

`/usr/lib/i386-linux-gnu` or `/usr/lib/arm-linux-gnueabi` or `/usr/lib/arm-linux-gnueabihf` depends on your CPU arch. Check `gcc -m32 -Q --help=target | fgrep -- -march` to find out.

* CentOS / Fedora / Oracle Linux :

`/usr/lib`

* Solaris :

`/usr/lib`

Change the `your_32bit_host_param` to:

* Debian / Ubuntu / other Debian based Linux :

`i386-linux-gnu` or `arm-linux-gnu` or any others depends on your CPU arch.

* CentOS / Fedora / Oracle Linux :

`i386-redhat-linux` or `armv7hl-redhat-linux-gnueabi` or any others depends on your CPU arch.

* Solaris :

`i386-pc-solaris2.11` or any others depends on your CPU arch.

> ! Importanat ! : Please make sure you get right 64-bit or 32-bit library file suitable for your YTuner 64-bit or 32-bit version.

If you found any errors or inaccuracies in this description, please let me know.