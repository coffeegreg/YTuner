program ytuner;

// YTuner main unit.

// YTuner is a simple project designed to replace vTuner internet radio service and dedicated to
// all users of AVRs made by Yamaha, Denon, Onkyo, Marantz and others with built-in vTuner support.

// Copyright (c) 2023 Greg P. (https://github.com/coffeegreg)
// YTuner is licensed under MIT license.
// See the https://github.com/coffeegreg/YTuner/blob/master/LICENSE.txt file for more details.

{$mode objfpc}{$H+}

uses
  {$DEFINE USESSL}     //Comment/uncomment this line to disable/enable SSL support.
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  LazUTF8,
  Classes, SysUtils, IniFiles, StrUtils, TypInfo, FileUtil,
  fphttpapp,
  {$IFDEF USESSL}
  openssl, opensslsockets,
  {$ENDIF}
  regexpr, my_stations, vtuner, httpserver, radiobrowser, common, bookmark,
  dnsserver, threadtimer, avr, maintenance;

// {$DEFINE FREE_ON_FINAL}
// Enable the FREE_ON_FINAL directive in IdCompilerDefines.inc of Indy library to remove standard (ie, intentional) Indy memory leaks.
// Look at the comments in the finalization sections of IdThread.pas and IdStack.pas.
// Due to the fact described above YTuner has 3 unfreed memory blocks : 120.

function GetRBStationsUUIDsThread(AP:Pointer):PtrInt;
begin
  GetRBStationsUUIDs;
end;

procedure RBStationsRefreshOnTimer(Sender: TObject);
begin
  BeginThread(@GetRBStationsUUIDsThread);
end;

function ReadMyStations: boolean;
begin
  case IndexStr(ExtractFileExt(ConfigPath+DirectorySeparator+MyStationsFileName),MY_STATIONS_EXT) of
    0: Result:=ReadMyStationsINIFile(ConfigPath+DirectorySeparator+MyStationsFileName);
    1,2: Result:=ReadMyStationsYAMLFile(ConfigPath+DirectorySeparator+MyStationsFileName);
  else
    Result:=False;
  end;
end;

function CheckMyStationsThread(AP:Pointer):PtrInt;
var
  LMyStationsFileAge: Int64;
  LMyStationsFileCRC32: Cardinal;
begin
  try
    if (AnsiMatchText(ExtractFileExt(ConfigPath+DirectorySeparator+MyStationsFileName),MY_STATIONS_EXT)) then
      begin
        LMyStationsFileAge:=FileAge(ConfigPath+DirectorySeparator+MyStationsFileName);
        if MyStationsFileAge<>LMyStationsFileAge then
          begin
            LMyStationsFileCRC32:=CalcFileCRC32(ConfigPath+DirectorySeparator+MyStationsFileName);
            if MyStationsFileCRC32<>LMyStationsFileCRC32 then
              begin
                if (MyStationsFileAge<>0) and (MyStationsFileCRC32<>0) then
                  Logging(ltInfo, '"My Stations" file has been changed. Refreshing..');
                if ReadMyStations then
                  begin
                    MyStationsFileAge:=LMyStationsFileAge;
                    MyStationsFileCRC32:=LMyStationsFileCRC32;
                  end;
              end;
          end;
      end
    else
      Logging(ltError, 'Unsupported "My Stations" file format.');
  except
    on E:Exception do
      Logging(ltError, 'CheckMyStationsThread Error: '+E.Message);
  end;
end;

procedure MyStationsRefreshOnTimer(Sender: TObject);
begin
  BeginThread(@CheckMyStationsThread);
end;

procedure ReadINIConfiguration;
var
  LToken: string;
  LRBCacheTypeIdx: integer;
  LCacheFolderLocation, LConfigFolderLocation: string;
begin
  with TIniFile.Create(MyAppPath+'ytuner.ini') do
    try
      try
        LogType:=TLogType(ReadInteger(INI_CONFIGURATION,'MessageInfoLevel',3));
      except
        LogType:=ltError;
      end;
      if ReadString(INI_CONFIGURATION,'INIVersion','')<>INI_VERSION then
        Logging(ltWarning, 'You are running out of INI file or your ytuner.ini file is outdated! Some features may not work properly! Check https://github.com/coffeegreg/YTuner/tree/master/cfg for the correct INI file for your version of YTuner');
      MyIPAddress:=GetLocalIP(ReadString(INI_CONFIGURATION,'IPAddress',MyIPAddress));
      UseSSL:=ReadBool(INI_CONFIGURATION,'UseSSL',True);
      IconSize:=ReadInteger(INI_CONFIGURATION,'IconSize',IconSize);
      IconCache:=ReadBool(INI_CONFIGURATION,'IconCache',True);
      LToken:=ReadString(INI_CONFIGURATION,'MyToken',MyToken);
      CommonAVRini:=ReadBool(INI_CONFIGURATION,'CommonAVRini',True);

      LCacheFolderLocation:=ReadString(INI_CONFIGURATION,'CacheFolderLocation',CachePath);
      CachePath:=IfThen(LCacheFolderLocation=DEFAULT_STRING,MyAppPath,LCacheFolderLocation);
      if not CachePath.EndsWith(DirectorySeparator) then
        CachePath:=CachePath+DirectorySeparator;
      CachePath:=CachePath+PATH_CACHE;

      LConfigFolderLocation:=ReadString(INI_CONFIGURATION,'ConfigFolderLocation',ConfigPath);
      ConfigPath:=IfThen(LConfigFolderLocation=DEFAULT_STRING,MyAppPath,LConfigFolderLocation);
      if not ConfigPath.EndsWith(DirectorySeparator) then
        ConfigPath:=ConfigPath+DirectorySeparator;
      ConfigPath:=ConfigPath+PATH_CONFIG;

      with TRegexpr.Create do
        try
         Expression:='^[A-F 0-9]{16,16}$';
         if Exec(LToken) then
           MyToken:=LToken;
        finally
          Free;
        end;
      MyStationsEnabled:=ReadBool(INI_MYSTATIONS,'Enable',True);
      MyStationsFileName:=ReadString(INI_MYSTATIONS,'MyStationsFile',MyStationsFileName);
      MyStationsAutoRefreshPeriod:=ReadInteger(INI_MYSTATIONS,'MyStationsAutoRefreshPeriod',0);

      RadioBrowserEnabled:=ReadBool(INI_RADIOBROWSER,'Enable',True);
      RBAPIURL:=ReadString(INI_RADIOBROWSER,'RBAPIURL',RBAPIURL);
      RBPopularAndSearchStationsLimit:=ReadInteger(INI_RADIOBROWSER,'RBPopularAndSearchStationsLimit',RBPopularAndSearchStationsLimit);
      RBMinStationsPerCategory:=ReadInteger(INI_RADIOBROWSER,'RBMinStationsPerCategory',RBMinStationsPerCategory);
      RBUUIDsCacheTTL:=ReadInteger(INI_RADIOBROWSER,'RBUUIDsCacheTTL',RBUUIDsCacheTTL);
      RBUUIDsCacheAutoRefresh:=ReadBool(INI_RADIOBROWSER,'RBUUIDsCacheAutoRefresh',False);
      RBCacheTTL:=ReadInteger(INI_RADIOBROWSER,'RBCacheTTL',RBCacheTTL);
      if ReadString(INI_RADIOBROWSER,'RBCacheType','') <> '' then
        begin
          LRBCacheTypeIdx:=GetEnumValue(TypeInfo(TCacheType),ReadString(INI_RADIOBROWSER,'RBCacheType','catFile'));
          if LRBCacheTypeIdx>=0 then
            RBCacheType:=TCacheType(LRBCacheTypeIdx)
          else
            begin
              Logging(ltError, 'Unexpected value of RBStationsCacheType: "'+ReadString(INI_RADIOBROWSER,'RBCacheType','')+'". Default "catFile" will be used');
              RBCacheType:=catMemStr;
            end;
         end
      else
        begin
          Logging(ltWarning, 'Radio-browser.info cache is disabled.');
          RBCacheType:=catNone;
        end;

      BookmarkEnabled:=ReadBool(INI_BOOKMARK,'Enable',False);
      CommonBookmark:=ReadBool(INI_BOOKMARK,'CommonBookmark',False);
      BookmarkStationsLimit:=ReadInteger(INI_BOOKMARK,'BookmarkStationsLimit',BookmarkStationsLimit);

      WebServerIPAddress:=GetLocalIP(ReadString(INI_WEBSERVER,'WebServerIPAddress',MyIPAddress).Replace(DEFAULT_STRING,MyIPAddress));
      WebServerPort:=ReadInteger(INI_WEBSERVER,'WebServerPort',WebServerPort);

      DNSServerEnabled:=ReadBool(INI_DNSSERVER,'Enable',True);
      DNSServerIPAddress:=GetLocalIP(ReadString(INI_DNSSERVER,'DNSServerIPAddress',MyIPAddress).Replace(DEFAULT_STRING,MyIPAddress));
      DNSServerPort:=ReadInteger(INI_DNSSERVER,'DNSServerPort',DNSServerPort);
      InterceptDNs:=ReadString(INI_DNSSERVER,'InterceptDNs',InterceptDNs);
      DNSServers:=ReadString(INI_DNSSERVER,'DNSServers',DNSServers);

      MaintenanceServiceEnabled:=ReadBool(INI_MAINTENANCESERVER,'Enable',True);
      MaintenanceServerIPAddress:=GetLocalIP(ReadString(INI_MAINTENANCESERVER,'MaintenanceServerIPAddress',MaintenanceServerIPAddress).Replace(DEFAULT_STRING,MaintenanceServerIPAddress));
      MaintenanceServerPort:=ReadInteger(INI_MAINTENANCESERVER,'MaintenanceServerPort',MaintenanceServerPort);
    finally
      Free;
    end;
end;

procedure MoveConfigFiles;
var
  LFileName: string;
  LCfgFiles: TStringList;
begin
  LCfgFiles:=FindAllFiles(MyAppPath,MyStationsFileName+';stations.*;*.xml',False);
  try
    if LCfgFiles.Count>0 then
      begin
        Logging(ltInfo, 'Moving files to config directory ..');
        for LFileName in LCfgFiles do
          if CopyFile(LFileName,LFileName.Replace(MyAppPath,ConfigPath+DirectorySeparator),[cffOverwriteFile]) then DeleteFile(LFileName);
      end;
  finally
    LCfgFiles.Free;
  end;
end;

begin
  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:

  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  MyAppPath:=ProgramDirectory;
  Writeln(APP_NAME+' v'+APP_VERSION+' '+APP_COPYRIGHT);
  ReadINIConfiguration;
  if not DirectoryExists(CachePath) then CreateDir(CachePath);
  if not DirectoryExists(ConfigPath) then CreateDir(ConfigPath);
  if DirectoryExists(ConfigPath) then MoveConfigFiles;
  ReadAVRINIConfiguration('avr');

  Logging(ltInfo, 'Starting services..');
  {$IFDEF USESSL}
  if UseSSL then InitSSLInterface;
  {$ENDIF USESSL}

  if RadioBrowserEnabled then
    if not LoadRBStationsUUIDs then
      BeginThread(@GetRBStationsUUIDsThread);

  if MyStationsEnabled then
    CheckMyStationsThread(nil);

  RemoveOldRBCacheFiles;
  if RBCacheType<>catNone then
    begin
      RBCache:=TRBCache.Create;
      if RBCacheType=catMemStr then
        RBCache.OwnsObjects:=True
      else
        LoadRBCacheFilesInfo(0);
    end;

  if RadioBrowserEnabled and RBUUIDsCacheAutoRefresh and (RBUUIDsCacheTTL>0) then
    begin
      RBThread:=TThreadTimer.Create(RB_THREAD);
      with RBThread do
        begin
          Interval:=RBUUIDsCacheTTL*3600000;
          OnTimer:=@RBStationsRefreshOnTimer;
          StartTimer;
        end;
    end;

  if MyStationsEnabled and (MyStationsAutoRefreshPeriod>0) then
    begin
      MSThread:=TThreadTimer.Create(MS_THREAD);
      with MSThread do
        begin
          Interval:=MyStationsAutoRefreshPeriod*60000;
          OnTimer:=@MyStationsRefreshOnTimer;
          StartTimer;
        end;
    end;

  if DNSServerEnabled and StartDNSServer then
    Logging(ltInfo, DNS_SERVICE+': listening on: '+DNSServerIPAddress+':'+DNSServerPort.ToString);

  if MaintenanceServiceEnabled then
    BeginThread(@MaintenaceHTTPServerThread);

  Application.Address:=WebServerIPAddress;
  Application.Port:=WebServerPort;
  Application.LookupHostNames:=False;
  RegisterServerRoutes;
  Application.Threaded:=True;
  Application.Initialize;
  Logging(ltInfo, WEB_SERVICE+': listening on: '+WebServerIPAddress+':'+WebServerPort.ToString);
  if MyIPAddress <> WebServerIPAddress then
    Logging(ltInfo, 'AVRs HTTP requests IP target: '+MyIPAddress);
  Application.Run;
end.

