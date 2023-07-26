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
  APP_VERSION = '1.0.1';
  APP_COPYRIGHT = 'Copyright (c) 2023 Greg P. (https://github.com/coffeegreg)';
  INI_VERSION = '1.0.1';

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
  INI_WEBSERVER = 'WebServer';
  INI_DNSSERVER = 'DNSServer';

  HTTP_HEADER_ACCEPT = 'Accept';
  HTTP_HEADER_USER_AGENT = 'User-Agent';
  HTTP_HEADER_LOCATION = 'Location';
  HTTP_HEADER_SERVER = 'Server';
  HTTP_RESPONSE_CONTENT_TYPE : array[TResponseContentType] of string = ('text/html; charset=utf-8','application/xml','image/png','image/jpeg','image/gif','image/tiff','application/json');

  HTTP_CODE_OK = 200;
  HTTP_CODE_FOUND = 302;
  HTTP_CODE_NOT_FOUND = 404;

  DEFAULT_STRING = 'default';
var
  MyIPAddress: string = DEFAULT_STRING;
  LogType: TLogType = ltError;
  MyAppPath: string;
  UseSSL: boolean = True;

procedure Logging(ALogType: TLogType; ALogMessage: string);
function GetLocalIP(ADefaultIP: string): string;
function StripHttps(URL: string):string;

implementation

procedure Logging(ALogType: TLogType; ALogMessage: string);
begin
  if ALogType<=LogType then
    Writeln(DateTimeToStr(Now)+' : '+LOG_TYPE_MSG[ALogType]+' : '+ALogMessage+'.');
end;

function GetLocalIP(ADefaultIP: string): string;
var
  IPList: TIdStackLocalAddressList;
  i: integer = 0;

  function IsIP_v4(AIP_v4: string): Boolean;
  var
    i: LongInt;

    function TryStrToByte(const s: String; out i: LongInt): Boolean;
    begin
      Result:=((TryStrToInt(s,i)) and (i>=0) and (i<=255));
    end;

  begin
    Result:=((Length(AIP_v4.Split(['.']))=4)
             and (TryStrToByte(AIP_v4.Split(['.'])[0],i))
             and (TryStrToByte(AIP_v4.Split(['.'])[1],i))
             and (TryStrToByte(AIP_v4.Split(['.'])[2],i))
             and (TryStrToByte(AIP_v4.Split(['.'])[3],i)));
  end;
begin
//  Due to the filtering out of loopback interface IP addresses in the GetLocalAddressList procedure of the Indy library, we add it manually at this moment.
//  For more information look at: https://github.com/IndySockets/Indy/issues/494
//  Be careful! As you can see below, the given loopback IP address is not verified with the list of available IP addresses.
  if IsIP_v4(ADefaultIP) and (ADefaultIP.Split(['.'])[0]='127') then
    Result:=ADefaultIP
  else
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
              Logging(ltError, 'No entries on IP List');
        finally
          IPList.Free;
        end;
      except
        On E: Exception do
          Logging(ltError, 'IP error: ' + E.message);
      end;
    end;
end;

function StripHttps(URL: string):string;
begin
  StripHttps:=StringReplace(URL,'https://','http://',[]);
end;

end.


