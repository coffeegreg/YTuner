program viritin;

// Viritin main unit.

// Viritin is a simple project designed to replace vTuner internet radio service and dedicated to
// all users of AVRs made by Yamaha, Denon, Onkyo, Marantz and others with built-in vTuner support.

// Viritin is a fork of YTuner by Greg P. (https://github.com/coffeegreg/YTuner),
// whose work almost all of this is.
//
// Copyright (c) 2023 Greg P. (https://github.com/coffeegreg)
// Copyright (c) 2026 Olli Sulopuisto (Viritin, formerly Retuner, a fork of YTuner)
// MIT licensed, as YTuner is. See LICENSE.txt, which carries both notices.

{$mode objfpc}{$H+}

uses
  {$DEFINE USESSL}     //Comment/uncomment this line to disable/enable SSL support.
  {$IFDEF UNIX}
  cthreads,
// cmem swaps the memory manager for C malloc/free. heaptrc cannot see through
// it: -gh installs heaptrc first and cmem's initialization then replaces the
// manager underneath, so heaptrc reported "6 memory blocks allocated" for a run
// that parses ini files and serves XML -- zero leaks, always, whatever the
// truth. Loading heaptrc after cmem instead makes it wrap cmem, and that
// segfaults mid-run.
// So the diagnostic build simply leaves cmem out, and -gh then measures the
// whole heap. CHECKED=1 ./script/build.sh passes -dNO_CMEM -gh; every shipped
// build still uses cmem exactly as before.
  {$IFNDEF NO_CMEM}
  cmem,
  {$ENDIF}
  {$ENDIF}
  LazUTF8,
  Classes, SysUtils, IniFiles, StrUtils, TypInfo, FileUtil,
  fphttpapp,
  {$IFDEF USESSL}
  openssl, opensslsockets,
  {$ENDIF}
  regexpr, my_stations, podcasts, presets, vtuner, httpserver, radiobrowser, common, bookmark,
  dnsserver, threadtimer, avr, maintenance, radiobrowserdb, relayserver, webgui,
  avrremote;

// {$DEFINE FREE_ON_FINAL}
// Enable the FREE_ON_FINAL directive in IdCompilerDefines.inc of Indy library to remove standard (ie, intentional) Indy memory leaks.
// Look at the comments in the finalization sections of IdThread.pas and IdStack.pas.
// Due to the fact described above Retuner has 3 unfreed memory blocks : 120.

function GetRBStationsUUIDsThread(AP:Pointer):PtrInt;
begin
  GetRBStationsUUIDs;
end;

procedure RBStationsUUIDsRefreshOnTimer(Sender: TObject);
begin
  BeginThread(@GetRBStationsUUIDsThread);
end;

function DBRBPrepareThread(AP:Pointer):PtrInt;
begin
  if DBRBPrepare then
    DBRBCheckAVRView(0);
end;

procedure RBStationsDBRefreshOnTimer(Sender: TObject);
begin
  BeginThread(@DBRBPrepareThread);
end;

function CheckMyStationsThread(AP:Pointer):PtrInt;
var
  LMyStationsFileAge: Int64;
  LMyStationsFileCRC32: Cardinal;
begin
  try
    if (AnsiMatchText(ExtractFileExt(MyStationsFilePath),MY_STATIONS_EXT)) then
      begin
        LMyStationsFileAge:=FileAge(MyStationsFilePath);
        if MyStationsFileAge<>LMyStationsFileAge then
          begin
            LMyStationsFileCRC32:=CalcFileCRC32(MyStationsFilePath);
            if MyStationsFileCRC32<>LMyStationsFileCRC32 then
              begin
                if (MyStationsFileAge<>0) and (MyStationsFileCRC32<>0) then
                  Logging(ltInfo, '"My Stations" file has been changed. Refreshing..');
                if ReloadStations then
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

// Fetching is network work and must not sit on the timer thread. A refresh that
// changes nothing still ends in ReloadStations, which is cheap and keeps the
// published list correct if a preset file was edited by hand in the cache.
function RefreshPresetsThread(AP:Pointer):PtrInt;
begin
  Result:=0;
  try
    RefreshPresets;
    ReloadStations;
  except
    on E:Exception do
      Logging(ltError, 'RefreshPresetsThread Error: '+E.Message);
  end;
end;

procedure PresetsRefreshOnTimer(Sender: TObject);
begin
  BeginThread(@RefreshPresetsThread);
end;

// An install predating the rename has retuner.ini or ytuner.ini and no viritin.ini.
// Renaming it keeps every setting, rather than silently starting from defaults -- which
// would not look like a failure, it would look like the config was ignored.
procedure MigrateLegacyConfig;
var
  LNew, LOld, LOldYtuner: string;
begin
  LNew:=MyAppPath+CONFIG_FILE_NAME;
  LOld:=MyAppPath+CONFIG_FILE_NAME_LEGACY;
  LOldYtuner:=MyAppPath+CONFIG_FILE_NAME_LEGACY_YTUNER;
  if FileExists(LNew) then
    Exit;
  if FileExists(LOld) then
  begin
    if RenameFile(LOld,LNew) then
      Logging(ltInfo, 'Renamed '+CONFIG_FILE_NAME_LEGACY+' to '+CONFIG_FILE_NAME)
    else
      Logging(ltWarning, 'Found '+CONFIG_FILE_NAME_LEGACY+' but could not rename it to '+
                         CONFIG_FILE_NAME+'; copy it across by hand, or settings will be defaults');
  end
  else if FileExists(LOldYtuner) then
  begin
    if RenameFile(LOldYtuner,LNew) then
      Logging(ltInfo, 'Renamed '+CONFIG_FILE_NAME_LEGACY_YTUNER+' to '+CONFIG_FILE_NAME)
    else
      Logging(ltWarning, 'Found '+CONFIG_FILE_NAME_LEGACY_YTUNER+' but could not rename it to '+
                         CONFIG_FILE_NAME+'; copy it across by hand, or settings will be defaults');
  end;
end;

procedure ReadINIConfiguration;
var
  LToken: string;
  LRBCacheTypeIdx: integer;
  LLogLevel: integer;
  LCacheFolderLocation, LConfigFolderLocation, LDBFolderLocation: string;
begin
  MigrateLegacyConfig;
  with TIniFile.Create(MyAppPath+CONFIG_FILE_NAME) do
    try
      try
        if not ValueExists(INI_CONFIGURATION,INI_MESSAGE_INFO_LEVEL) then
          WriteInteger(INI_CONFIGURATION,INI_MESSAGE_INFO_LEVEL,4);
// Clamped rather than cast blindly: a value outside 0..4 produced an invalid
// enum, which silently changed which messages were logged.
        LLogLevel:=ReadInteger(INI_CONFIGURATION,INI_MESSAGE_INFO_LEVEL,4);
        if (LLogLevel<Ord(Low(TLogType))) or (LLogLevel>Ord(High(TLogType))) then
          begin
            LogType:=ltDebug;
            Logging(ltWarning, INI_MESSAGE_INFO_LEVEL+' must be 0..'+Ord(High(TLogType)).ToString+'. Using '+Ord(ltDebug).ToString+'.');
          end
        else
          LogType:=TLogType(LLogLevel);
      except
        LogType:=ltError;
      end;
      if not ValueExists(INI_CONFIGURATION,INI_INI_VERSION) then
        begin
          Logging(ltWarning, MSG_INI_WARNING1);
          Logging(ltWarning, MSG_INI_WARNING3);
        end
      else
        if ReadString(INI_CONFIGURATION,INI_INI_VERSION,'')<>INI_VERSION then
          begin
            Logging(ltWarning, MSG_INI_WARNING2);
            Logging(ltWarning, MSG_INI_WARNING3);
          end;
      WriteString(INI_CONFIGURATION,INI_INI_VERSION,INI_VERSION);

      if not ValueExists(INI_CONFIGURATION,INI_IP_ADDRESS) then
        WriteString(INI_CONFIGURATION,INI_IP_ADDRESS,DEFAULT_STRING);
      MyIPAddress:=GetLocalIP(ReadString(INI_CONFIGURATION,INI_IP_ADDRESS,MyIPAddress));

      if not ValueExists(INI_CONFIGURATION,INI_ACT_AS_HOST) then
        WriteString(INI_CONFIGURATION,INI_ACT_AS_HOST,DEFAULT_STRING);
      URLHost:=ReadString(INI_CONFIGURATION,INI_ACT_AS_HOST,MyIPAddress).Replace(DEFAULT_STRING,MyIPAddress);
      if URLHost.Trim.IsEmpty then
        URLHost:=MyIPAddress;

      if not ValueExists(INI_CONFIGURATION,INI_USE_SSL) then
        WriteString(INI_CONFIGURATION,INI_USE_SSL,'1');
      UseSSL:=ReadBool(INI_CONFIGURATION,INI_USE_SSL,True);

      if not ValueExists(INI_CONFIGURATION,INI_RESOLVE_PLAYLISTS) then
        WriteString(INI_CONFIGURATION,INI_RESOLVE_PLAYLISTS,'0');
      ResolvePlaylists:=ReadBool(INI_CONFIGURATION,INI_RESOLVE_PLAYLISTS,False);

      if not ValueExists(INI_CONFIGURATION,INI_LOCAL_COUNTRY) then
        WriteString(INI_CONFIGURATION,INI_LOCAL_COUNTRY,'');
      LocalCountry:=ReadString(INI_CONFIGURATION,INI_LOCAL_COUNTRY,'').Trim;

      if not ValueExists(INI_CONFIGURATION,INI_RELAY_HTTPS) then
        WriteString(INI_CONFIGURATION,INI_RELAY_HTTPS,'0');
      RelayHTTPS:=ReadBool(INI_CONFIGURATION,INI_RELAY_HTTPS,False);

      if not ValueExists(INI_CONFIGURATION,INI_RELAY_PORT) then
        WriteInteger(INI_CONFIGURATION,INI_RELAY_PORT,RELAY_PORT);
      RelayPort:=ReadInteger(INI_CONFIGURATION,INI_RELAY_PORT,RelayPort);

      if not ValueExists(INI_CONFIGURATION,INI_REDIRECT_HTTP_CODE) then
        WriteInteger(INI_CONFIGURATION,INI_REDIRECT_HTTP_CODE,HTTP_CODE_REDIRECT);
      HTTPCodeRedirect:=ReadInteger(INI_CONFIGURATION,INI_REDIRECT_HTTP_CODE,HTTP_CODE_REDIRECT);

      if not ValueExists(INI_CONFIGURATION,INI_ICON_SIZE) then
        WriteInteger(INI_CONFIGURATION,INI_ICON_SIZE,ICON_SIZE);
      IconSize:=ReadInteger(INI_CONFIGURATION,INI_ICON_SIZE,ICON_SIZE);

      if not ValueExists(INI_CONFIGURATION,INI_ICON_CACHE) then
        WriteString(INI_CONFIGURATION,INI_ICON_CACHE,'1');
      IconCache:=ReadBool(INI_CONFIGURATION,INI_ICON_CACHE,ICON_CACHE);

      if not ValueExists(INI_CONFIGURATION,INI_ICON_EXTENSION) then
        WriteString(INI_CONFIGURATION,INI_ICON_EXTENSION,'');
      IconPlaceholder:=ReadBool(INI_CONFIGURATION,INI_ICON_PLACEHOLDER,ICON_PLACEHOLDER);
      IconExtension:=ReadString(INI_CONFIGURATION,INI_ICON_EXTENSION,'').ToLower;
      if IconExtension <> '' then
        IconExtension:='.'+IconExtension.Substring(0,3);

      if not ValueExists(INI_CONFIGURATION,INI_MY_TOKEN) then
        WriteString(INI_CONFIGURATION,INI_MY_TOKEN,VT_TOKEN);
      LToken:=ReadString(INI_CONFIGURATION,INI_MY_TOKEN,VT_TOKEN);

      if not ValueExists(INI_CONFIGURATION,INI_COMMON_AVR_INI) then
        WriteString(INI_CONFIGURATION,INI_COMMON_AVR_INI,'1');
      CommonAVRini:=ReadBool(INI_CONFIGURATION,INI_COMMON_AVR_INI,COMMON_AVR_INI);

      if not ValueExists(INI_CONFIGURATION,INI_CACHE_FOLDER_LOCATION) then
        WriteString(INI_CONFIGURATION,INI_CACHE_FOLDER_LOCATION,DEFAULT_STRING);
      LCacheFolderLocation:=ReadString(INI_CONFIGURATION,INI_CACHE_FOLDER_LOCATION,CachePath);
      CachePath:=IfThen(LCacheFolderLocation=DEFAULT_STRING,MyAppPath,LCacheFolderLocation);
      if not CachePath.EndsWith(DirectorySeparator) then
        CachePath:=CachePath+DirectorySeparator;
      CachePath:=CachePath+PATH_CACHE;

      if not ValueExists(INI_CONFIGURATION,INI_CONFIG_FOLDER_LOCATION) then
        WriteString(INI_CONFIGURATION,INI_CONFIG_FOLDER_LOCATION,DEFAULT_STRING);
      LConfigFolderLocation:=ReadString(INI_CONFIGURATION,INI_CONFIG_FOLDER_LOCATION,ConfigPath);
      ConfigPath:=IfThen(LConfigFolderLocation=DEFAULT_STRING,MyAppPath,LConfigFolderLocation);
      if not ConfigPath.EndsWith(DirectorySeparator) then
        ConfigPath:=ConfigPath+DirectorySeparator;
      ConfigPath:=ConfigPath+PATH_CONFIG;

      if not ValueExists(INI_CONFIGURATION,INI_DB_FOLDER_LOCATION) then
        WriteString(INI_CONFIGURATION,INI_DB_FOLDER_LOCATION,DEFAULT_STRING);
      LDBFolderLocation:=ReadString(INI_CONFIGURATION,INI_DB_FOLDER_LOCATION,DBPath);
      DBPath:=IfThen(LDBFolderLocation=DEFAULT_STRING,MyAppPath,LDBFolderLocation);
      if not DBPath.EndsWith(DirectorySeparator) then
        DBPath:=DBPath+DirectorySeparator;
      DBPath:=DBPath+PATH_DB;

      if not ValueExists(INI_CONFIGURATION,INI_DB_LIB_FILE) then
        WriteString(INI_CONFIGURATION,INI_DB_LIB_FILE,DEFAULT_STRING);
      DBLibFile:=TryToFindSQLite3Lib(ReadString(INI_CONFIGURATION,INI_DB_LIB_FILE,DBLibFile));

      with TRegexpr.Create do
        try
         Expression:='^[A-F a-f 0-9]{16,16}$';
         if Exec(LToken) then
           MyToken:=LToken;
        finally
          Free;
        end;

      if not ValueExists(INI_MYSTATIONS,INI_ENABLE) then
        WriteString(INI_MYSTATIONS,INI_ENABLE,'1');
      MyStationsEnabled:=ReadBool(INI_MYSTATIONS,INI_ENABLE,True);

      if not ValueExists(INI_MYSTATIONS,INI_MY_STATIONS_FILE) then
        WriteString(INI_MYSTATIONS,INI_MY_STATIONS_FILE,MY_STATIONS_FILE_NANME);
      MyStationsFileName:=ReadString(INI_MYSTATIONS,INI_MY_STATIONS_FILE,MyStationsFileName);

      if not ValueExists(INI_MYSTATIONS,INI_MY_STATIONS_AUTO_REFRESH_PERIOD) then
        WriteInteger(INI_MYSTATIONS,INI_MY_STATIONS_AUTO_REFRESH_PERIOD,0);
      MyStationsAutoRefreshPeriod:=ReadInteger(INI_MYSTATIONS,INI_MY_STATIONS_AUTO_REFRESH_PERIOD,0);

      if not ValueExists(INI_PRESETS,INI_ENABLE) then
        WriteString(INI_PRESETS,INI_ENABLE,'0');
      PresetsEnabled:=ReadBool(INI_PRESETS,INI_ENABLE,False);

      if not ValueExists(INI_PRESETS,INI_PRESETS_COUNTRIES) then
        WriteString(INI_PRESETS,INI_PRESETS_COUNTRIES,DEFAULT_STRING);
      PresetsCountries:=ReadString(INI_PRESETS,INI_PRESETS_COUNTRIES,DEFAULT_STRING);
      if PresetsCountries=DEFAULT_STRING then
        PresetsCountries:='';

      if not ValueExists(INI_PRESETS,INI_PRESETS_URL) then
        WriteString(INI_PRESETS,INI_PRESETS_URL,PRESETS_DEFAULT_URL);
      PresetsURL:=ReadString(INI_PRESETS,INI_PRESETS_URL,PRESETS_DEFAULT_URL);

      if not ValueExists(INI_PRESETS,INI_PRESETS_AUTO_REFRESH_PERIOD) then
        WriteInteger(INI_PRESETS,INI_PRESETS_AUTO_REFRESH_PERIOD,0);
      PresetsAutoRefreshPeriod:=ReadInteger(INI_PRESETS,INI_PRESETS_AUTO_REFRESH_PERIOD,0);

      if not ValueExists(INI_PODCASTS,INI_ENABLE) then
        WriteString(INI_PODCASTS,INI_ENABLE,'0');
      PodcastsEnabled:=ReadBool(INI_PODCASTS,INI_ENABLE,False);

      if not ValueExists(INI_PODCASTS,INI_PODCASTS_FILE) then
        WriteString(INI_PODCASTS,INI_PODCASTS_FILE,PODCASTS_FILE_NAME);
      PodcastsFileName:=ReadString(INI_PODCASTS,INI_PODCASTS_FILE,PodcastsFileName);

      if not ValueExists(INI_PODCASTS,INI_PODCASTS_EPISODES_LIMIT) then
        WriteInteger(INI_PODCASTS,INI_PODCASTS_EPISODES_LIMIT,PODCAST_EPISODES_LIMIT);
      PodcastEpisodesLimit:=ReadInteger(INI_PODCASTS,INI_PODCASTS_EPISODES_LIMIT,PodcastEpisodesLimit);
      if PodcastEpisodesLimit<1 then
        PodcastEpisodesLimit:=PODCAST_EPISODES_LIMIT;

      if not ValueExists(INI_PODCASTS,INI_PODCASTS_CACHE_TTL) then
        WriteInteger(INI_PODCASTS,INI_PODCASTS_CACHE_TTL,PODCAST_CACHE_TTL);
      PodcastCacheTTL:=ReadInteger(INI_PODCASTS,INI_PODCASTS_CACHE_TTL,PodcastCacheTTL);

      if not ValueExists(INI_RADIOBROWSER,INI_ENABLE) then
        WriteString(INI_RADIOBROWSER,INI_ENABLE,'1');
      RadioBrowserEnabled:=ReadBool(INI_RADIOBROWSER,INI_ENABLE,True);

      if not ValueExists(INI_RADIOBROWSER,INI_RB_API_URL) then
        WriteString(INI_RADIOBROWSER,INI_RB_API_URL,RBAPIURL);
      RBAPIURL:=ReadString(INI_RADIOBROWSER,INI_RB_API_URL,RBAPIURL);

      if not ValueExists(INI_RADIOBROWSER,INI_RB_POPULAR_AND_SEARCH_STATIONS_LIMIT) then
        WriteInteger(INI_RADIOBROWSER,INI_RB_POPULAR_AND_SEARCH_STATIONS_LIMIT,RB_POPULAR_AND_SEARCH_STATIONS_LIMIT);
      RBPopularAndSearchStationsLimit:=ReadInteger(INI_RADIOBROWSER,INI_RB_POPULAR_AND_SEARCH_STATIONS_LIMIT,RBPopularAndSearchStationsLimit);

      if not ValueExists(INI_RADIOBROWSER,INI_RB_MIN_STATIONS_PER_CATEGORY) then
        WriteInteger(INI_RADIOBROWSER,INI_RB_MIN_STATIONS_PER_CATEGORY,RB_MIN_STATIONS_PER_CATEGORY);
      RBMinStationsPerCategory:=ReadInteger(INI_RADIOBROWSER,INI_RB_MIN_STATIONS_PER_CATEGORY,RBMinStationsPerCategory);

      if not ValueExists(INI_RADIOBROWSER,INI_RB_UUIDS_CACHE_TTL) then
        WriteString(INI_RADIOBROWSER,INI_RB_UUIDS_CACHE_TTL,'0');
      RBUUIDsCacheTTL:=ReadInteger(INI_RADIOBROWSER,INI_RB_UUIDS_CACHE_TTL,RBUUIDsCacheTTL);
      if RBUUIDsCacheTTL<0 then
        begin
          WriteString(INI_RADIOBROWSER,INI_RB_UUIDS_CACHE_TTL,'0');
          RBUUIDsCacheTTL:=0;
        end;

      if not ValueExists(INI_RADIOBROWSER,INI_RB_UUIDS_CACHE_AUTO_REFRESH) then
        WriteString(INI_RADIOBROWSER,INI_RB_UUIDS_CACHE_AUTO_REFRESH,'0');
      RBUUIDsCacheAutoRefresh:=ReadBool(INI_RADIOBROWSER,INI_RB_UUIDS_CACHE_AUTO_REFRESH,False);

      if not ValueExists(INI_RADIOBROWSER,INI_RB_CACHE_TTL) then
        WriteString(INI_RADIOBROWSER,INI_RB_CACHE_TTL,'0');
      RBCacheTTL:=ReadInteger(INI_RADIOBROWSER,INI_RB_CACHE_TTL,RBCacheTTL);
      if RBCacheTTL<0 then
        begin
          WriteString(INI_RADIOBROWSER,INI_RB_CACHE_TTL,'0');
          RBCacheTTL:=0;
        end;

      if not ValueExists(INI_RADIOBROWSER,INI_RB_CACHE_TYPE) then
        WriteString(INI_RADIOBROWSER,INI_RB_CACHE_TYPE,'catFile');
      if ReadString(INI_RADIOBROWSER,INI_RB_CACHE_TYPE,'') <> '' then
        begin
          LRBCacheTypeIdx:=GetEnumValue(TypeInfo(TCacheType),ReadString(INI_RADIOBROWSER,INI_RB_CACHE_TYPE,'catFile'));
          if LRBCacheTypeIdx>=0 then
            RBCacheType:=TCacheType(LRBCacheTypeIdx)
          else
            begin
              Logging(ltError, 'Unexpected value of RBStationsCacheType: "'+ReadString(INI_RADIOBROWSER,INI_RB_CACHE_TYPE,'')+'". Default "catFile" will be used');
              RBCacheType:=catMemStr;
            end;
         end
      else
        begin
          Logging(ltWarning, 'Radio-browser.info cache is disabled.');
          RBCacheType:=catNone;
        end;

      if not ValueExists(INI_BOOKMARK,INI_ENABLE) then
        WriteString(INI_BOOKMARK,INI_ENABLE,'1');
      BookmarkEnabled:=ReadBool(INI_BOOKMARK,INI_ENABLE,True);

      if not ValueExists(INI_BOOKMARK,INI_COMMON_BOOKMARK) then
        WriteString(INI_BOOKMARK,INI_COMMON_BOOKMARK,'1');
      CommonBookmark:=ReadBool(INI_BOOKMARK,INI_COMMON_BOOKMARK,True);

      if not ValueExists(INI_BOOKMARK,INI_BOOKMARK_STATIONS_LIMIT) then
        WriteInteger(INI_BOOKMARK,INI_BOOKMARK_STATIONS_LIMIT,BOOKMARK_STATIONS_LIMIT);
      BookmarkStationsLimit:=ReadInteger(INI_BOOKMARK,INI_BOOKMARK_STATIONS_LIMIT,BookmarkStationsLimit);

      if not ValueExists(INI_WEBSERVER,INI_WEBSERVER_IPADDRESS) then
        WriteString(INI_WEBSERVER,INI_WEBSERVER_IPADDRESS,DEFAULT_STRING);
      WebServerIPAddress:=GetLocalIP(ReadString(INI_WEBSERVER,INI_WEBSERVER_IPADDRESS,MyIPAddress).Replace(DEFAULT_STRING,MyIPAddress));

      if not ValueExists(INI_WEBSERVER,INI_WEBSERVER_PORT) then
        WriteInteger(INI_WEBSERVER,INI_WEBSERVER_PORT,WEBSERVER_PORT);
      WebServerPort:=ReadInteger(INI_WEBSERVER,INI_WEBSERVER_PORT,WebServerPort);

      if not ValueExists(INI_DNSSERVER,INI_ENABLE) then
        WriteString(INI_DNSSERVER,INI_ENABLE,'1');
      DNSServerEnabled:=ReadBool(INI_DNSSERVER,INI_ENABLE,True);

      if not ValueExists(INI_DNSSERVER,INI_DNSSERVER_IPADDRESS) then
        WriteString(INI_DNSSERVER,INI_DNSSERVER_IPADDRESS,DEFAULT_STRING);
      DNSServerIPAddress:=GetLocalIP(ReadString(INI_DNSSERVER,INI_DNSSERVER_IPADDRESS,MyIPAddress).Replace(DEFAULT_STRING,MyIPAddress));

      if not ValueExists(INI_DNSSERVER,INI_DNSSERVER_PORT) then
        WriteInteger(INI_DNSSERVER,INI_DNSSERVER_PORT,DNSSERVER_PORT);
      DNSServerPort:=ReadInteger(INI_DNSSERVER,INI_DNSSERVER_PORT,DNSServerPort);

      if not ValueExists(INI_DNSSERVER,INI_INTERCEPT_DNS) then
        WriteString(INI_DNSSERVER,INI_INTERCEPT_DNS,INTERCEPT_DNS);
      InterceptDNs:=ReadString(INI_DNSSERVER,INI_INTERCEPT_DNS,InterceptDNs);

      if not ValueExists(INI_DNSSERVER,INI_DNSSERVERS) then
        WriteString(INI_DNSSERVER,INI_DNSSERVERS,DNS_SERVERS);
      DNSServers:=ReadString(INI_DNSSERVER,INI_DNSSERVERS,DNSServers);

      if not ValueExists(INI_WEBGUI,INI_ENABLE) then
        WriteString(INI_WEBGUI,INI_ENABLE,'0');
      WebGUIEnabled:=ReadBool(INI_WEBGUI,INI_ENABLE,False);

      if not ValueExists(INI_WEBGUI,INI_WEBGUI_IPADDRESS) then
        WriteString(INI_WEBGUI,INI_WEBGUI_IPADDRESS,WEBGUI_IPADDRESS);
      WebGUIIPAddress:=ReadString(INI_WEBGUI,INI_WEBGUI_IPADDRESS,WEBGUI_IPADDRESS);

      if not ValueExists(INI_WEBGUI,INI_WEBGUI_PORT) then
        WriteInteger(INI_WEBGUI,INI_WEBGUI_PORT,WEBGUI_PORT);
      WebGUIPort:=ReadInteger(INI_WEBGUI,INI_WEBGUI_PORT,WebGUIPort);

      if not ValueExists(INI_WEBGUI,INI_WEBGUI_USER) then
        WriteString(INI_WEBGUI,INI_WEBGUI_USER,WEBGUI_USER);
      WebGUIUser:=ReadString(INI_WEBGUI,INI_WEBGUI_USER,WEBGUI_USER);

      if not ValueExists(INI_WEBGUI,INI_WEBGUI_PASSWORD) then
        WriteString(INI_WEBGUI,INI_WEBGUI_PASSWORD,'');
      WebGUIPassword:=ReadString(INI_WEBGUI,INI_WEBGUI_PASSWORD,'');

      if not ValueExists(INI_WEBGUI,INI_WEBGUI_AVR_ADDRESS) then
        WriteString(INI_WEBGUI,INI_WEBGUI_AVR_ADDRESS,'');
      RemoteAVRAddress:=ReadString(INI_WEBGUI,INI_WEBGUI_AVR_ADDRESS,'').Trim;

      if not ValueExists(INI_DNSSERVER,INI_DNS_ADVERTISE_IP) then
        WriteString(INI_DNSSERVER,INI_DNS_ADVERTISE_IP,'');
      DNSAdvertiseIP:=ReadString(INI_DNSSERVER,INI_DNS_ADVERTISE_IP,'').Trim;

      if not ValueExists(INI_DNSSERVER,INI_DNS_RESTRICT_FORWARDING) then
        WriteString(INI_DNSSERVER,INI_DNS_RESTRICT_FORWARDING,'0');
      DNSRestrictForwarding:=ReadBool(INI_DNSSERVER,INI_DNS_RESTRICT_FORWARDING,False);

      if not ValueExists(INI_DNSSERVER,INI_DNS_ALLOWED_CLIENTS) then
        WriteString(INI_DNSSERVER,INI_DNS_ALLOWED_CLIENTS,'');
      DNSAllowedClients:=ReadString(INI_DNSSERVER,INI_DNS_ALLOWED_CLIENTS,'');

      if not ValueExists(INI_MAINTENANCESERVER,INI_ENABLE) then
        WriteString(INI_MAINTENANCESERVER,INI_ENABLE,'0');
      MaintenanceServiceEnabled:=ReadBool(INI_MAINTENANCESERVER,INI_ENABLE,False);

      if not ValueExists(INI_MAINTENANCESERVER,INI_MAINTENANCESERVER_IPADDRESS) then
        WriteString(INI_MAINTENANCESERVER,INI_MAINTENANCESERVER_IPADDRESS,MAINTENANCESERVER_IPADDRESS);
      MaintenanceServerIPAddress:=GetLocalIP(ReadString(INI_MAINTENANCESERVER,INI_MAINTENANCESERVER_IPADDRESS,MaintenanceServerIPAddress).Replace(DEFAULT_STRING,MaintenanceServerIPAddress));

      if not ValueExists(INI_MAINTENANCESERVER,INI_MAINTENANCESERVER_PORT) then
        WriteInteger(INI_MAINTENANCESERVER,INI_MAINTENANCESERVER_PORT,MAINTENANCESERVER_PORT);
      MaintenanceServerPort:=ReadInteger(INI_MAINTENANCESERVER,INI_MAINTENANCESERVER_PORT,MaintenanceServerPort);
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
  MyAppPath:=GetMyAppPath;
  Writeln(APP_NAME+' - '+APP_COPYRIGHT);
  ReadINIConfiguration;

  if not DirectoryExists(CachePath) then CreateDir(CachePath);
  if not DirectoryExists(ConfigPath) then CreateDir(ConfigPath);
  if DirectoryExists(ConfigPath) then MoveConfigFiles;
  ReadAVRINIConfiguration(AVR_AVR);

  Logging(ltInfo, 'Starting services..');
  {$IFDEF USESSL}
  if UseSSL then InitSSLInterface;
  {$ENDIF USESSL}

// Before the first station load, so a fresh install serves presets in its very
// first menu rather than only after a restart. This is the one place the fetch
// is synchronous: it is startup, nothing is serving yet, and the HTTP client
// has its own connect and read timeouts.
  if PresetsEnabled then
    RefreshPresets;

  if MyStationsEnabled then
    CheckMyStationsThread(nil);

  if PodcastsEnabled then
    LoadPodcastFeeds;

  RemoveOldRBCacheFiles;

  if RadioBrowserEnabled then
    begin
      if RBCacheType in [catDB, catMemDB, catPermMemDB] then
        begin
          if (not DirectoryExists(DBPath)) and (not CreateDir(DBPath)) then
            begin
              Logging(ltWarning, string.Join(' ',[MSG_RBDB_CANNOT,MSG_RBDB_CREATE,MSG_RBDB_DB,MSG_DIRECTORY,DBPath]));
              RBCacheType:=catFile;
            end
          else
            if DBLibFile.IsEmpty then
              begin
                Logging(ltWarning, string.Join(' ',[MSG_RBDB_DB,MSG_RBDB_LIBRARY,MSG_NOT_FOUND]));
                RBCacheType:=catFile;
              end
            else
              if LoadSQLite3Lib and CheckSQLite3LibVer and DBRBPrepare then
                begin
                  if not DBRBCheckAVRView(0) then
                    RBCacheType:=catFile
                  else
                    if RBCacheTTL>0 then
                      begin
                        RBThread:=TThreadTimer.Create(RB_THREAD);
                        with RBThread do
                          begin
                            Interval:=RBCacheTTL*3600000;
                            OnTimer:=@RBStationsDBRefreshOnTimer;
                            StartTimer;
                          end;
                      end;
                end
              else
                begin
                  Logging(ltWarning, string.Join(' ',[MSG_RBDB_DB,MSG_RBDB_PREPARING,MSG_ERROR]));
                  RBCacheType:=catFile;
                end;
         if RBCacheType=catFile then
           Logging(ltWarning, string.Join(' ',[MSG_CHANGING,MSG_CACHE,'to "catFile"']));
        end;

      if RBCacheType in [catNone, catFile, catMemStr] then
        begin
          if not LoadRBStationsUUIDs then
            BeginThread(@GetRBStationsUUIDsThread);
          if RBCacheType<>catNone then
            begin
              RBCache:=TRBCache.Create;
              if RBCacheType=catMemStr then
                RBCache.OwnsObjects:=True
              else
                LoadRBCacheFilesInfo(0);
            end;
          if RBUUIDsCacheAutoRefresh and (RBUUIDsCacheTTL>0) then
            begin
              RBThread:=TThreadTimer.Create(RB_THREAD);
              with RBThread do
                begin
                  Interval:=RBUUIDsCacheTTL*3600000;
                  OnTimer:=@RBStationsUUIDsRefreshOnTimer;
                  StartTimer;
                end;
            end;
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

  if PresetsEnabled and (PresetsAutoRefreshPeriod>0) then
    begin
      PSThread:=TThreadTimer.Create(PS_THREAD);
      with PSThread do
        begin
          Interval:=PresetsAutoRefreshPeriod*60000;
          OnTimer:=@PresetsRefreshOnTimer;
          StartTimer;
        end;
    end;

  if WebGUIEnabled and StartWebGUIServer then
// Says who may reach it rather than what it bound to: on FPC 3.2.2 the socket
// is open on every interface regardless, and the setting is enforced per
// connection instead.
    Logging(ltInfo, WEBGUI_SERVICE+': listening on port '+WebGUIPort.ToString
      +', accepting '+IfThen(WebGUIIPAddress.IsEmpty or (WebGUIIPAddress=DEFAULT_STRING) or (WebGUIIPAddress='0.0.0.0'),
                             'any client',WebGUIIPAddress));

  if RelayHTTPS then
    begin
      OnRelayResolveURL:=@ResolveRelayStationURL;
      RelayIPAddress:=WebServerIPAddress;
// The listener logs its own address once the bind has actually succeeded.
      StartRelayServer;
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
  if (URLHost <> '') and  (URLHost <> WebServerIPAddress) then
    Logging(ltInfo, 'AVRs HTTP requests host target: '+URLHost)
  else
    if MyIPAddress <> WebServerIPAddress then
      Logging(ltInfo, 'AVRs HTTP requests IP target: '+MyIPAddress);
  Application.Run;
end.

