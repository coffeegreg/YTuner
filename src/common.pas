unit common;

// YTuner: Common constants, variables and procedures.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IdStack, IdGlobal, StrUtils, crc,
{$IFDEF UNIX}
  dl,
{$ENDIF}
  SQLDBLib, SQLDB, SQLite3Conn;

type
  TLogType = (ltNone, ltInfo, ltWarning, ltError, ltDebug);
  TResponseContentType = (ctNone, ctXML, ctPNG, ctJPG, ctGIF, ctTIFF, ctJSON);
  TCacheType = (catNone, catFile, catMemStr, catDB, catMemDB, catPermMemDB);

const
  APP_NAME = 'YTuner';
  APP_VERSION = '1.2.2';
  APP_COPYRIGHT = 'Copyright (c) 2024 Greg P. (https://github.com/coffeegreg)';
  INI_VERSION = '1.2.0';

  YTUNER_USER_AGENT = 'YTuner';
  YTUNER_HOST = 'ytunerhost';

  LOG_TYPE_MSG : array[TLogType] of string = ('','Inf','Wrn','Err','Dbg');

  MSG_FILE_LOAD_ERROR = ' file load error';
  MSG_FILE_SAVE_ERROR = ' file save error';
  MSG_FILE_DELETE_ERROR = ' file delete error';
  MSG_LOADING = 'loading';
  MSG_REMOVING = 'removing';
  MSG_GETTING = 'getting';
  MSG_CHANGING = 'changing';
  MSG_ERROR = 'error';
  MSG_LOADED = 'loaded';
  MSG_SAVE = 'save';
  MSG_SAVED = 'saved';
  MSG_EMPTY = 'empty';
  MSG_CACHE = 'cache';
  MSG_FILE = 'file';
  MSG_DIRECTORY = 'directory';
  MSG_VERSION = 'version';
  MSG_OBSOLETE = 'obsolete';
  MSG_REQUIRED = 'required';
  MSG_STREAM = 'stream';
  MSG_OBJECTS = 'objects';
  MSG_BOOKMARK = 'bookmark';
  MSG_SUCCESSFULLY_LOADED = 'successfully loaded ';
  MSG_SUCCESSFULLY_SAVED = 'successfully saved ';
  MSG_SUCCESSFULLY_DOWNLOADED = 'successfully downloaded ';
  MSG_ERROR_LOAD = 'load error of';
  MSG_NOT_LOADED = 'not loaded';
  MSG_FOUND = 'found';
  MSG_NOT_FOUND = 'not found';
  MSG_STATIONS = 'stations';

  MSG_RBUUID_CACHE_FILE = 'RB UUIDs cache file';

  INI_CONFIGURATION = 'Configuration';
  INI_RADIOBROWSER = 'RadioBrowser';
  INI_MYSTATIONS = 'MyStations';
  INI_BOOKMARK = 'Bookmark';
  INI_WEBSERVER = 'WebServer';
  INI_DNSSERVER = 'DNSServer';
  INI_MAINTENANCESERVER = 'MaintenanceServer';

  HTTP_HEADER_ACCEPT = 'Accept';
  HTTP_HEADER_USER_AGENT = 'User-Agent';
  HTTP_HEADER_LOCATION = 'Location';
  HTTP_HEADER_SERVER = 'Server';
  HTTP_RESPONSE_CONTENT_TYPE : array[TResponseContentType] of string = ('text/html; charset=utf-8','application/xml','image/png','image/jpeg','image/gif','image/tiff','application/json');

  MY_STATIONS_PREFIX = 'MS';
  RADIOBROWSER_PREFIX = 'RB';
  PATH_MY_STATIONS = 'my_stations';

  PATH_PARAM_ID = 'id';
  PATH_PARAM_MAC = 'mac';
  PATH_PARAM_FAV = 'fav';
  PATH_PARAM_SEARCH = 'search';
  PATH_PARAM_SSEARCH_TYPE = 'sSearchtype';
  PATH_PARAM_TOKEN = 'token';

  HTTP_CODE_OK = 200;
  HTTP_CODE_FOUND = 302;
  HTTP_CODE_NOT_FOUND = 404;
  HTTP_CODE_UNAVAILABLE = 503;

  DEFAULT_STRING = 'default';
  ESC_CHARS : Array Of AnsiString = ('\t','\n','\r','\b','\f',';');

  PATH_CACHE = 'cache';
  PATH_CONFIG = 'config';
  PATH_DB = 'db';

  CACHE_EXT = '.cache';

  SQLITE_VER_X_MIN = 3;
  SQLITE_VER_Y_MIN = 33;
  SQLITE_VER_Z_MIN = 0;

var
  MyIPAddress: string = DEFAULT_STRING;
  LogType: TLogType = ltError;
  MyAppPath: string;
  UseSSL: boolean = True;
  CachePath: string = DEFAULT_STRING;
  ConfigPath: string = DEFAULT_STRING;
  DBPath: string = DEFAULT_STRING;
  DBLibFile: string = DEFAULT_STRING;

{$IFDEF WINDOWS}
  DBLib: array[1..1] of string = ('sqlite3.dll');
{$ELSE}
  {$IFDEF DARWIN}
    DBLib: array[1..2] of string = ('libsqlite3.0.dylib','libsqlite3.dylib');
  {$ELSE}
    DBLib: array[1..2] of string = ('libsqlite3.so.0','libsqlite3.so');
  {$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}
  DBLibSearchPath: array[1..1] of string = ('');
{$ELSE}
  {$IFDEF DARWIN}
    DBLibSearchPath: array[1..3] of string = ('/opt/local/lib','/usr/lib','/usr/local/lib');
  {$ELSE}
    {$IFDEF SOLARIS}
      {$IFDEF CPU64}
        DBLibSearchPath: array[1..1] of string = ('/usr/lib/64');
      {$ELSE CPU32}
        DBLibSearchPath: array[1..1] of string = ('/usr/lib');
      {$ENDIF}
    {$ELSE}
      {$IFDEF BSD}
        DBLibSearchPath: array[1..2] of string = ('/usr/local/lib','/usr/lib');
      {$ELSE}
        {$IFDEF LINUX}
          {$IFDEF CPU64}
            DBLibSearchPath: array[1..4] of string = ('/usr/lib64','/usr/lib/x86_64-linux-gnu','/usr/lib/aarch64-linux-gnu','/usr/lib');
          {$ELSE CPU32}
            DBLibSearchPath: array[1..4] of string = ('/usr/lib','/usr/lib/i386-linux-gnu','/usr/lib/arm-linux-gnueabi','/usr/lib/arm-linux-gnueabihf');
          {$ENDIF}
        {$ELSE}
          DBLibSearchPath: array[1..3] of string = ('/usr/lib','/usr/local/lib','/opt/local/lib');
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

procedure Logging(ALogType: TLogType; ALogMessage: string);
function GetLocalIP(ADefaultIP: string): string;
function CalcFileCRC32(AFileName: string): Cardinal;
function RemoveEscChars(LInputStr: RawByteString): RawByteString;
function HaveCommonElements(AStr: string; AStrArray: array of string): boolean;
function ContainsIn(AStr: string; AStrArray: array of string): boolean;
function StripChars(AInputString: string; const AChars: array of AnsiString):string;
function URLEncode(const AStr: String): AnsiString;
function TryToFindSQLite3Lib(ALibFile: string): string;
function LoadSQLite3Lib: boolean;
function CheckSQLite3LibVer: boolean;
function GetMyAppPath: string;

implementation
uses radiobrowserdb;

procedure Logging(ALogType: TLogType; ALogMessage: string);
begin
  if ALogType<=LogType then
    begin
      ALogMessage[1]:=UpCase(ALogMessage[1]);
      Writeln(DateTimeToStr(Now)+' : '+LOG_TYPE_MSG[ALogType]+' : '+ALogMessage+'.');
    end;
end;

function GetLocalIP(ADefaultIP: string): string;
var
  IPList: TIdStackLocalAddressList;
  i: integer = 0;
  LIPLoopback: string = '';

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
              begin
                while (i<=IPList.Count-1) and (Result<>ADefaultIP) do
                  begin
                    if IPList[i].IPVersion = Id_IPv4 then
                      begin
                        if IPList[i].IPAddress.Split(['.'])[0]='127' then
                          LIPLoopback:=IPList[i].IPAddress
                        else
                          if Result='' then
                            Result:=IPList[i].IPAddress;
                        if IPList[i].IPAddress = ADefaultIP then
                          Result:=ADefaultIP;
                      end;
                    i:=i+1;
                  end;
                if (Result='') and (LIPLoopback<>'') then
                  Result:=LIPLoopback;
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

function CalcFileCRC32(AFileName: string): Cardinal;
var
  Buffer : TBytes;
begin
  Result:=CRC32(0,nil,0);
  with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone) do
    try
      SetLength(Buffer, Size);
      Read(Buffer,Size);
      Result:=CRC32(Result,@Buffer[0],Size);
    finally
      Free;
    end;
end;

function RemoveEscChars(LInputStr: RawByteString): RawByteString;
var
  i: integer;
begin
  for i:=Low(ESC_CHARS) to High(ESC_CHARS) do
    Result:=AnsiReplaceStr(LInputStr,ESC_CHARS[i],'');
end;

function HaveCommonElements(AStr: string; AStrArray: array of string): boolean;
var
  LStrEnum,LStrArrayEnum: string;
begin
  Result:=False;
  for LStrEnum in AStr.Split([',','/',';']) do
    for LStrArrayEnum in AStrArray do
      if IsWild(Trim(LStrEnum),LStrArrayEnum,True) then
      begin
        Result:=True;
        Exit;
      end;
end;

function ContainsIn(AStr: string; AStrArray: array of string): boolean;
var
  LStrArrayEnum: string;
begin
  Result:=False;
  for LStrArrayEnum in AStrArray do
    if ContainsText(AStr,LStrArrayEnum) then
      begin
        Result:=True;
        Exit;
      end;
end;

function StripChars(AInputString: string; const AChars: array of AnsiString):string;
begin
  Result:=DelSpace1(StringsReplace(AInputString,AChars,(DupeString(' ,',Length(AChars)-1)+' ').Split([',']),[rfReplaceAll]).Trim);
end;

function URLEncode(const AStr: String): AnsiString;
var
  LAnsiChar: AnsiChar;
begin
  Result:='';
  for LAnsiChar in AStr do
    begin
      if ((Ord(LAnsiChar)<65) or (Ord(LAnsiChar)>90))
         and ((Ord(LAnsiChar)<97) or (Ord(LAnsiChar)>122)) then
        Result:=Result+'%'+IntToHex(Ord(LAnsiChar),2)
      else
        Result:=Result+LAnsiChar;
    end;
end;

function TryToFindSQLite3Lib(ALibFile: string): string;
var
  LFound: boolean = False;
  LLibFile: string = '';
  LDBLibIdx, LDBLibSearchPathIdx: integer;
{$IFDEF UNIX}
  LLibHandle: Pointer;
  LPdlinfo: Pdl_info;
  LPtrLibFile: Pointer;
{$ENDIF}
begin
  ALibFile:=ALibFile.Trim;
  if (ALibFile<>DEFAULT_STRING) and (not ALibFile.IsEmpty) and (FileExists(ALibFile)) then
    Result:=ALibFile
  else
    begin
      Result:='';
      LFound:=False;
      LDBLibIdx:=1;
      while (LDBLibIdx<=Length(DBLib)) and (not LFound) do
        begin
          LDBLibSearchPathIdx:=1;
          while (LDBLibSearchPathIdx<=Length(DBLibSearchPath)) and (not LFound) do
            begin
              LLibFile:=IfThen(DBLibSearchPath[LDBLibSearchPathIdx]='',MyAppPath,DBLibSearchPath[LDBLibSearchPathIdx]);
              if not LLibFile.EndsWith(DirectorySeparator) then
                LLibFile:=LLibFile+DirectorySeparator;
              LLibFile:=LLibFile+DBLib[LDBLibIdx];
              LFound:=FileExists(LLibFile);
              LDBLibSearchPathIdx:=LDBLibSearchPathIdx+1;
            end;
          LDBLibIdx:=LDBLibIdx+1;
        end;
{$IFDEF UNIX}
      if not LFound then
        begin
          LDBLibIdx:=1;
          while (LDBLibIdx<=Length(DBLib)) and (not LFound) do
            begin
              LLibHandle:=dlopen(PAnsiChar(DBLib[LDBLibIdx]), RTLD_LAZY);
              if LLibHandle<>nil then
                begin
                  LPdlinfo:=LLibHandle;
                  LPtrLibFile:=LPdlinfo^.dli_fbase;
                  LLibFile:=String(LPtrLibFile);
{$IFDEF DARWIN}
                  if LLibFile='' then
                    LLibFile:=DBLib[LDBLibIdx];
{$ENDIF}
                  LPtrLibFile:=nil;
                  dlclose(LLibHandle);
                  LFound:=True;
                end;
              LDBLibIdx:=LDBLibIdx+1;
            end;
        end;
{$ENDIF}
      if LFound then
        Result:=LLibFile;
    end;
end;

function LoadSQLite3Lib: boolean;
begin
  Result:=False;
  with TSQLDBLibraryLoader.Create(nil) do
    try
      try
        ConnectionType:='SQLite3';
        LibraryName:=DBLibFile;
        Enabled:=True;
        LoadLibrary;
        Result:=Enabled;
      except
        on E: Exception do
          begin
            Logging(ltError, string.Join(' ',[MSG_RBDB_DB, MSG_RBDB_LIBRARY, MSG_RBDB_ERROR,' ('+E.Message+')']));
            Free;
          end;
      end;
    finally
      Free;
    end;
end;

function CheckSQLite3LibVer: boolean;
var
  LSQLite3Connection: TSQLite3Connection;
  LSQLTransaction: TSQLTransaction;
begin
  Result:=False;
  LSQLite3Connection:=TSQLite3Connection.Create(nil);
  with LSQLite3Connection do
    try
      LogEvents:=[];
      OpenFlags:=[sofReadOnly,sofMemory];
      DatabaseName:=RBDB_FIELD_VER;
      Connected:=True;
      with TSQLQuery.Create(nil) do
        try
          ReadOnly:=True;
          DataBase:=LSQLite3Connection;
          LSQLTransaction:=TSQLTransaction.Create(nil);
          LSQLTransaction.Options:=[stoUseImplicit];
          LSQLTransaction.DataBase:=LSQLite3Connection;
          Transaction:=LSQLTransaction;
          SQL.Text:=DBRBGetResourceSQLScript(RBDB_SQL_VERSION);
          try
            Open;
            if not EOF then
              begin
                Logging(ltDebug, string.Join(' ',[MSG_RBDB_DB,MSG_RBDB_LIBRARY,':',FieldByName(RBDB_FIELD_VER).AsString]));
                Result:=(FieldByName(RBDB_FIELD_VER).AsString.Split(['.'])[0].ToInteger*1000000+
                         FieldByName(RBDB_FIELD_VER).AsString.Split(['.'])[1].ToInteger*1000+
                         FieldByName(RBDB_FIELD_VER).AsString.Split(['.'])[2].ToInteger >= SQLITE_VER_X_MIN*1000000+SQLITE_VER_Y_MIN*1000+SQLITE_VER_Z_MIN);
                if not Result then
                  Logging(ltError, string.Join(' ',[MSG_OBSOLETE,MSG_RBDB_DB,MSG_RBDB_LIBRARY,MSG_VERSION,':',FieldByName(RBDB_FIELD_VER).AsString,MSG_REQUIRED,'>=',SQLITE_VER_X_MIN.ToString+'.'+SQLITE_VER_Y_MIN.ToString+'.'+SQLITE_VER_Z_MIN.ToString]));
              end;
          except
            on E: Exception do
              begin
                Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_DB,MSG_RBDB_LIBRARY,MSG_RBDB_CHECKING,MSG_RBDB_ERROR,' ('+E.Message+')']));
                Exit;
              end;
          end;
        finally
          Close;
          Free;
          LSQLTransaction.Free;
        end;
    finally
      LSQLite3Connection.Connected:=False;
      LSQLite3Connection.Free;
    end;
end;

function GetMyAppPath: string;
begin
{$IFDEF UNIX}
// At this moment I have no better idea how to detect Alpine Linux Busybox ?
// ! In this case, enter the YTuner directory first and then run it with ./ytuner !
  if ParamStr(0).Contains('ld-musl-') then
    Result:='./'
  else
{$ENDIF}
    Result:=ProgramDirectory;
end;

end.


