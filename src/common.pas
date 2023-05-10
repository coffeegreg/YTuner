unit common;

// YTuner: Common constants, variables and procedures.

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, IdStack, IdGlobal;


type
  TLogType = (ltNone, ltInfo, ltWarning, ltError, ltDebug);
  TResponseContentType = (ctNone, ctXML, ctPNG, ctJPG, ctGIF, ctTIFF, ctJSON);

const
  APP_NAME = 'YTuner';
  APP_VERSION = 'v1.0.0';
  APP_COPYRIGHT = 'Copyright (c) 2023 Greg P. (https://github.com/coffeegreg)';

  YTUNER_USER_AGENT = 'YTuner';
  YTUNER_HOST = 'ytunerhost';

  LOG_TYPE_MSG : array[TLogType] of string = ('','Inf','Wrn','Err','Dbg');

  MSG_FILE_LOAD_ERROR = ' file load error';
  MSG_FILE_SAVE_ERROR = ' file save error';
  MSG_LOADING = 'Loading ';
  MSG_GETTING = 'Getting ';
  MSG_ERROR = 'error';
  MSG_SUCCESSFULLY_LOADED = 'Successfully loaded ';
  MSG_SUCCESSFULLY_SAVED = 'Successfully saved ';
  MSG_SUCCESSFULLY_DOWNLOADED = 'Successfully downloaded ';
  MSG_ERROR_LOAD = 'Load error of ';
  MSG_NOT_LOADED = 'not loaded';
  MSG_STATIONS = 'stations';

  INI_CONFIGURATION = 'Configuration';
  INI_RADIOBROWSER = 'RadioBrowser';
  INI_MYSTATIONS = 'MyStations';
  INI_BOOKMARK = 'Bookmark';
  INI_DNSSERVER = 'DNSServer';

  HTTP_HEADER_ACCEPT = 'Accept';
  HTTP_HEADER_USER_AGENT = 'User-Agent';
  HTTP_HEADER_LOCATION = 'Location';
  HTTP_HEADER_SERVER = 'Server';
  HTTP_RESPONSE_CONTENT_TYPE : array[TResponseContentType] of string = ('text/html; charset=utf-8','application/xml','image/png','image/jpeg','image/gif','image/tiff','application/json');

  HTTP_CODE_OK = 200;
  HTTP_CODE_FOUND = 302;
  HTTP_CODE_NOT_FOUND = 404;

var
  MyIPAddress: string = '0.0.0.0';
  LogType: TLogType = ltInfo;
  MyAppPath: string;
  UseSSL: boolean = True;

procedure Logging(ALogType: TLogType; ALogMessage: string);
function GetLocalIP(ADefaultIP: string='0.0.0.0'): string;
function StripHttps(URL: string):string;

implementation

procedure Logging(ALogType: TLogType; ALogMessage: string);
begin
  if ALogType<=LogType then
    Writeln(DateTimeToStr(Now)+' : '+LOG_TYPE_MSG[ALogType]+' : '+ALogMessage+'.');
end;

function GetLocalIP(ADefaultIP: string='0.0.0.0'): string;
var
  IPList: TIdStackLocalAddressList;
  i: integer = 0;
begin
  Result:='';
  try
    IPList := TIdStackLocalAddressList.Create;
    try
      TIdStack.IncUsage;
      try
        GStack.GetLocalAddressList(IPList);
      finally
        TIdStack.DecUsage;
      end;

      if IPList.Count > 0 then
        while (i<=IPList.Count-1) and (Result<>ADefaultIP) do
          begin
            if IPList[i].IPVersion = Id_IPv4 then
              begin
                if Result='' then
                  Result:=IPList[i].IPAddress;
                if IPList[i].IPAddress = ADefaultIP then
                  Result:=ADefaultIP;
              end;
            i:=i+1;
          end
      else
        Logging(ltError, 'No entries on IP List.');
    finally
      IPList.Free;
    end;
  except
    On E: Exception do
      Logging(ltError, 'IP error: ' + E.message);
  end;
end;

function StripHttps(URL: string):string;
begin
  StripHttps:=StringReplace(URL,'https://','http://',[]);
end;

end.


