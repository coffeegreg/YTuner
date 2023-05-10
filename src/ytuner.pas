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
  SysUtils, IniFiles,
  fphttpapp,
  {$IFDEF USESSL}
  openssl, opensslsockets,
  {$ENDIF}
  regexpr,
  my_stations, vtuner, httpserver, radiobrowser, common, bookmark, dnsserver, threadtimer;

type
  TRBStationsAutoRefresh = class(TObject)
    RefreshTimer: TThreadTimer;
    procedure RefreshTimerOnTimer(Sender: TObject);
  end;

function GetRBStationsThread(p:pointer):ptrint;
begin
  GetRBStations;
end;

procedure TRBStationsAutoRefresh.RefreshTimerOnTimer(Sender: TObject);
begin
  BeginThread(@GetRBStationsThread);
end;

procedure ReadINIConfiguration;
var
  LToken: string;
begin
  with TIniFile.Create(MyAppPath+'ytuner.ini') do
    try
      MyIPAddress:=GetLocalIP(ReadString(INI_CONFIGURATION,'IPAddress',MyIPAddress));
      WebServerPort:=ReadInteger(INI_CONFIGURATION,'WebServerPort',WebServerPort);
      UseSSL:=ReadBool(INI_CONFIGURATION,'UseSSL',True);
      try
        LogType:=TLogType(ReadInteger(INI_CONFIGURATION,'MessageInfoLevel',1));
      except
        LogType:=ltInfo;
      end;
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

      RadioBrowserEnabled:=ReadBool(INI_RADIOBROWSER,'Enable',True);
      RBAPIURL:=ReadString(INI_RADIOBROWSER,'RBAPIURL',RBAPIURL);
      RBPopularAndSearchStationsLimit:=ReadInteger(INI_RADIOBROWSER,'RBPopularAndSearchStationsLimit',RBPopularAndSearchStationsLimit);
      RBMinStationsPerCategory:=ReadInteger(INI_RADIOBROWSER,'RBMinStationsPerCategory',RBMinStationsPerCategory);
      RBUUIDsCacheTTL:=ReadInteger(INI_RADIOBROWSER,'RBUUIDsCacheTTL',RBUUIDsCacheTTL);
      RBUUIDsCacheAutoRefresh:=ReadBool(INI_RADIOBROWSER,'RBUUIDsCacheAutoRefresh',False);

      BookmarkEnabled:=ReadBool(INI_BOOKMARK,'Enable',False);
      CommonBookmark:=ReadBool(INI_BOOKMARK,'CommonBookmark',False);
      BookmarkStationsLimit:=ReadInteger(INI_BOOKMARK,'BookmarkStationsLimit',BookmarkStationsLimit);

      DNSServerEnabled:=ReadBool(INI_DNSSERVER,'Enable',True);
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
  ReadINIConfiguration;
  writeln(APP_NAME+' '+APP_VERSION+' '+APP_COPYRIGHT);
  Logging(ltInfo, 'Starting services..');
  if UseSSL then InitSSLInterface;
  if RadioBrowserEnabled then
    if not LoadRBStationsUUIDs then
      BeginThread(@GetRBStationsThread);
  if MyStationsEnabled then
    case ExtractFileExt(MyStationsFileName) of
      '.ini': ReadMyStationsINIFile(MyAppPath+MyStationsFileName);
      '.yaml': ReadMyStationsYAMLFile(MyAppPath+MyStationsFileName);
    end;

  if RBUUIDsCacheAutoRefresh and (RBUUIDsCacheTTL>0) then
    with TRBStationsAutoRefresh.Create do
      begin
        RefreshTimer:=TThreadTimer.Create;
        with RefreshTimer do
          begin
            Interval:=RBUUIDsCacheTTL*3600000;
            OnTimer:=@RefreshTimerOnTimer;
            StartTimer;
          end;
      end;

  if DNSServerEnabled and StartDNSServer then
    Logging(ltInfo, 'DNS server listening on: '+MyIPAddress+':'+DNSServerPort.ToString);

  Application.Address:=MyIPAddress;
  Application.Port:=WebServerPort;
  Application.LookupHostNames:=False;
  RegisterServerRoutes;
  Application.Threaded:=True;
  Application.Initialize;
  Logging(ltInfo, 'Web server listening on: '+MyIPAddress+':'+WebServerPort.ToString);
  Application.Run;
end.

