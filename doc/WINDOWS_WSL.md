# YTuner in Windows Subsystem for Linux (WSL)

Windows users can easily run the Linux releases in WSL 1, following the process below (version 1 of WSL simplifies access to the Windows LAN interface from the Linux environment)

At least YTuner 1.2.6 is known to run in the following configuration:
```
> wsl -l -v
  NAME            STATE           VERSION
* Ubuntu-16.04    Running         1
```

1. [Install](https://learn.microsoft.com/en-us/windows/wsl/install) WSL, following guidance to use WSL v1
2. Enter WSL with `bash`
3. Fetch a pre-built YTuner for Linux that matches the required architecture; eg\
`wget https://github.com/coffeegreg/YTuner/releases/download/1.2.6/ytuner-1.2.6-x86_64-linux.zip`
4. Inflate to a chosen folder:\
`unzip ytuner-1.2.6-x86_64-linux.zip  -d ytuner`
5. Edit `IPAddress` in `ytuner.ini` to match the address of the Windows interface that is accessible from the AVR
6. Follow other instructions for Linux in the [README](https://github.com/coffeegreg/YTuner/blob/master/README.md) to launch `YTuner`
