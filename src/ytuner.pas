program ytuner;

// Ytuner main unit.

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
  SysUtils, IniFiles, StrUtils,
  fphttpapp,
  {$IFDEF USESSL}
  openssl, opensslsockets,
  {$ENDIF}
  regexpr,
  my_stations, vtuner, httpserver, radiobrowser, common, bookmark, dnsserver, threadtimer;


function GetRBStationsThread(AP:Pointer):PtrInt;
begin
  GetRBStations;
end;

procedure RBStationsRefreshOnTimer(Sender: TObject);
begin
  BeginThread(@GetRBStationsThread);
end;

function ReadMyStations: boolean;
begin
  case IndexStr(ExtractFileExt(MyAppPath+MyStationsFileName),MY_STATIONS_EXT) of
    0: Result:=ReadMyStationsINIFile(MyAppPath+MyStationsFileName);
    1,2: Result:=ReadMyStationsYAMLFile(MyAppPath+MyStationsFileName);
  else
    Result:=False;
  end;
end;

function CheckMyStationsThread(AP:Pointer):PtrInt;
var
  LMyStationsFileAge: LongInt;
  LMyStationsFileCRC32: LongWord;
begin
  try
    if (AnsiMatchText(ExtractFileExt(MyAppPath+MyStationsFileName),MY_STATIONS_EXT)) then
      begin
        LMyStationsFileAge:=FileAge(MyAppPath+MyStationsFileName);
        if MyStationsFileAge<>LMyStationsFileAge then
          begin
            LMyStationsFileCRC32:=CalcFileCRC32(MyAppPath+MyStationsFileName);
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
begin
  with TIniFile.Create(MyAppPath+'ytuner.ini') do
    try
      try
        LogType:=TLogType(ReadInteger(INI_CONFIGURATION,'MessageInfoLevel',3));
      except
        LogType:=ltError;
      end;
      if ReadString(INI_CONFIGURATION,'INIVersion','')<>INI_VERSION then
        Logging(ltWarning, 'You are running out of INI file or your ytuner.ini file is outdated! Some features may not work properly! Check https://github.com/coffeegreg/YTuner for the correct INI file for your version of YTuner');
      MyIPAddress:=GetLocalIP(ReadString(INI_CONFIGURATION,'IPAddress',MyIPAddress));
      UseSSL:=ReadBool(INI_CONFIGURATION,'UseSSL',True);
      IconSize:=ReadInteger(INI_CONFIGURATION,'IconSize',IconSize);
      IconCache:=ReadBool(INI_CONFIGURATION,'IconCache',True);
      LToken:=ReadString(INI_CONFIGURATION,'MyToken',MyToken);
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
    finally
      Free;
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

  MyAppPath:=Application.Location;
  Writeln(APP_NAME+' v'+APP_VERSION+' '+APP_COPYRIGHT);
  ReadINIConfiguration;
  Logging(ltInfo, 'Starting services..');
  if UseSSL then InitSSLInterface;
  if RadioBrowserEnabled then
    if not LoadRBStationsUUIDs then
      BeginThread(@GetRBStationsThread);
  if MyStationsEnabled then
    CheckMyStationsThread(nil);

  if RadioBrowserEnabled and RBUUIDsCacheAutoRefresh and (RBUUIDsCacheTTL>0) then
    with TThreadTimer.Create('YTunerRB') do
      begin
        Interval:=RBUUIDsCacheTTL*3600000;
        OnTimer:=@RBStationsRefreshOnTimer;
        StartTimer;
      end;

  if MyStationsEnabled and (MyStationsAutoRefreshPeriod>0) then
    with TThreadTimer.Create('YTunerMS') do
      begin
        Interval:=MyStationsAutoRefreshPeriod*60000;
        OnTimer:=@MyStationsRefreshOnTimer;
        StartTimer;
      end;

  if DNSServerEnabled and StartDNSServer then
    Logging(ltInfo, 'DNS server listening on: '+DNSServerIPAddress+':'+DNSServerPort.ToString);

  Application.Address:=WebServerIPAddress;
  Application.Port:=WebServerPort;
  Application.LookupHostNames:=False;
  RegisterServerRoutes;
  Application.Threaded:=True;
  Application.Initialize;
  Logging(ltInfo, 'Web server listening on: '+WebServerIPAddress+':'+WebServerPort.ToString);
  if MyIPAddress <> WebServerIPAddress then
    Logging(ltInfo, 'AVRs HTTP requests IP target: '+MyIPAddress);
  Application.Run;
end.

