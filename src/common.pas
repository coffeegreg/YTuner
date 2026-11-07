unit common;

// Retuner: Common constants, variables and procedures.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, IdStack, IdGlobal, StrUtils, crc,
{$IFDEF UNIX}
  dl,
{$ENDIF}
  SQLDBLib, SQLDB, SQLite3Conn, FileUtil,
  fphttpclient;

type
  TLogType = (ltNone, ltInfo, ltWarning, ltError, ltDebug);
  TResponseContentType = (ctNone, ctXML, ctPNG, ctJPG, ctGIF, ctTIFF, ctJSON);
  TCacheType = (catNone, catFile, catMemStr, catDB, catMemDB, catPermMemDB);

  TLocalHttpClient = class(TFPHTTPClient)
  private
    FMaxDataToReceive: integer;
    procedure HandleDataReceived(Sender : TObject; Const ContentLength, CurrentPos : Int64);
  public
    constructor Create(AMaxDataToReceive: integer = 0); overload;
  end;

const
  APP_NAME = 'Viritin';
// CalVer: YY.MM.DD.N, N being the build within that day. The date a build was
// cut is the useful thing to know about it -- semantic versioning would promise
// an API this has never had.
  APP_VERSION = '26.11.07.287';
// The YTuner release this fork started from. Separate from APP_VERSION because
// it means something different and does not move when we cut a build: putting
// a Viritin version in the credit line below would attribute our releases to
// Greg P., which is the opposite of the point.
  UPSTREAM_VERSION = '1.2.6';
// Greg P. keeps his line in the banner because nearly all of this code is his.
// The notice in LICENSE.txt is the licence requirement; this is the courtesy
// that ought to go with it.
  APP_COPYRIGHT = 'a fork of YTuner '+UPSTREAM_VERSION+' by Greg P. (MIT) - https://github.com/ollisulopuisto/viritin';
// NOT CalVer, and not tied to APP_VERSION: this is compared against the
// INIVersion= line in every existing config file. It moves only when the config
// format actually changes, or every install is told its file is outdated.
  INI_VERSION = '1.2.2';

  VIRITIN_USER_AGENT = 'Viritin';
  RETUNER_USER_AGENT = VIRITIN_USER_AGENT;

  CONFIG_FILE_NAME = 'viritin.ini';
// What the file was called before the renames. Renamed to the above on first
// start so an existing install keeps its settings; see MigrateLegacyConfig.
  CONFIG_FILE_NAME_LEGACY = 'retuner.ini';
  CONFIG_FILE_NAME_LEGACY_YTUNER = 'ytuner.ini';
// Bookmark files store the host as a placeholder and get the real one put back
// when they are read. New files are written with this.
  BOOKMARK_HOST = 'viritinhost';
// What files written before the renames carry. Accepted on read and never
// written, so an AVR's existing bookmarks keep resolving and quietly convert to
// the new placeholder the next time they are saved. See SubstHost in
// bookmark.pas -- dropping this would orphan every bookmark already stored.
  BOOKMARK_HOST_LEGACY = 'retunerhost';
  BOOKMARK_HOST_LEGACY_YTUNER = 'ytunerhost';

  LOG_TYPE_MSG : array[TLogType] of string = ('','Inf','Wrn','Err','Dbg');

  MSG_FILE_LOAD_ERROR = ' file load error';
  MSG_FILE_SAVE_ERROR = ' file save error';
  MSG_FILE_DELETE_ERROR = ' file delete error';
  MSG_FILE_CREATE_ERROR = ' file create error';
  MSG_LOADING = 'loading';
  MSG_REMOVED = 'removed';
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
  MSG_GOOGLE_TRANSLATE = 'Google Translate:';
  MSG_TRANSLATOR = 'Translator';

  MSG_RBUUID_CACHE_FILE = 'RB UUIDs cache file';

  MSG_INI_WARNING1 = 'You are running out of INI file! Some features may not work properly!';
  MSG_INI_WARNING2 = 'Your retuner.ini file is outdated! Some features may not work properly!';
  MSG_INI_WARNING3 = 'Retuner will try to fill in the missing options, but check their description and usage on https://github.com/ollisulopuisto/retuner/tree/master/cfg.';
  MSG_VTUNER_ERROR_LINK1 = 'Obslolete vTuner radio station link - trying redirect to ';
  MSG_VTUNER_ERROR_LINK2 = 'Obslolete vTuner radio station link can not be resolved - trying redirect to your first radio station of stations.ini/yaml file.';
  MSG_FIRST_STATION_NEEDED = 'Be sure you have at least one category and one (first one) radio station DIRECT link in stations.ini/yaml file and option "Enable=1" is placed in "[MyStations]" section of retuner.ini file .';
  MSG_REDIRECT_SUPPORT_NEEDED = 'Your AVR must support HTTP redirects! See "RedirectHTTPCode" option.';

  INI_CONFIGURATION = 'Configuration';
  INI_INI_VERSION = 'INIVersion';
  INI_IP_ADDRESS = 'IPAddress';
  INI_ACT_AS_HOST = 'ActAsHost';
  INI_USE_SSL = 'UseSSL';
  INI_RESOLVE_PLAYLISTS = 'ResolvePlaylists';
  INI_LOCAL_COUNTRY = 'LocalCountry';
  INI_RELAY_HTTPS = 'RelayHTTPS';
  INI_RELAY_PORT = 'RelayPort';
  INI_REDIRECT_HTTP_CODE = 'RedirectHTTPCode';
  INI_MESSAGE_INFO_LEVEL = 'MessageInfoLevel';
  INI_ICON_SIZE = 'IconSize';
  INI_ICON_CACHE = 'IconCache';
  INI_ICON_PLACEHOLDER = 'IconPlaceholder';
  INI_ICON_EXTENSION = 'IconEndPointExtension';
  INI_MY_TOKEN = 'MyToken';
  INI_COMMON_AVR_INI = 'CommonAVRini';
  INI_CACHE_FOLDER_LOCATION = 'CacheFolderLocation';
  INI_CONFIG_FOLDER_LOCATION = 'ConfigFolderLocation';
  INI_DB_FOLDER_LOCATION = 'DBFolderLocation';
  INI_DB_LIB_FILE = 'DBLibFile';
  INI_ENABLE = 'Enable';
  INI_MYSTATIONS = 'MyStations';
  INI_MY_STATIONS_FILE = 'MyStationsFile';
  INI_MY_STATIONS_AUTO_REFRESH_PERIOD = 'MyStationsAutoRefreshPeriod';
  INI_PRESETS = 'Presets';
  INI_PRESETS_COUNTRIES = 'PresetsCountries';
  INI_PRESETS_URL = 'PresetsURL';
  INI_PRESETS_AUTO_REFRESH_PERIOD = 'PresetsAutoRefreshPeriod';
  INI_PODCASTS = 'Podcasts';
  INI_PODCASTS_FILE = 'PodcastsFile';
  INI_PODCASTS_EPISODES_LIMIT = 'PodcastEpisodesLimit';
  INI_PODCASTS_CACHE_TTL = 'PodcastCacheTTL';
  INI_RADIOBROWSER = 'RadioBrowser';
  INI_RB_API_URL = 'RBAPIURL';
  INI_RB_POPULAR_AND_SEARCH_STATIONS_LIMIT = 'RBPopularAndSearchStationsLimit';
  INI_RB_MIN_STATIONS_PER_CATEGORY = 'RBMinStationsPerCategory';
  INI_RB_UUIDS_CACHE_TTL = 'RBUUIDsCacheTTL';
  INI_RB_UUIDS_CACHE_AUTO_REFRESH = 'RBUUIDsCacheAutoRefresh';
  INI_RB_CACHE_TYPE = 'RBCacheType';
  INI_RB_CACHE_TTL = 'RBCacheTTL';
  INI_BOOKMARK = 'Bookmark';
  INI_COMMON_BOOKMARK = 'CommonBookmark';
  INI_BOOKMARK_STATIONS_LIMIT = 'BookmarkStationsLimit';
  INI_WEBSERVER = 'WebServer';
  INI_WEBSERVER_IPADDRESS = 'WebServerIPAddress';
  INI_WEBSERVER_PORT = 'WebServerPort';
  INI_DNSSERVER = 'DNSServer';
  INI_DNSSERVER_IPADDRESS = 'DNSServerIPAddress';
  INI_DNSSERVER_PORT = 'DNSServerPort';
  INI_INTERCEPT_DNS = 'InterceptDNs';
  INI_DNSSERVERS = 'DNSServers';
  INI_DNS_ADVERTISE_IP = 'DNSAdvertiseIP';
  INI_DNS_RESTRICT_FORWARDING = 'RestrictForwarding';
  INI_DNS_ALLOWED_CLIENTS = 'AllowedClients';
  INI_MAINTENANCESERVER = 'MaintenanceServer';
  INI_MAINTENANCESERVER_IPADDRESS = 'MaintenanceServerIPAddress';
  INI_MAINTENANCESERVER_PORT = 'MaintenanceServerPort';
  INI_WEBGUI = 'WebGUI';
  INI_WEBGUI_IPADDRESS = 'WebGUIIPAddress';
  INI_WEBGUI_PORT = 'WebGUIPort';
  INI_WEBGUI_USER = 'WebGUIUser';
  INI_WEBGUI_PASSWORD = 'WebGUIPassword';
  INI_WEBGUI_AVR_ADDRESS = 'RemoteAVRAddress';

// Outbound HTTP is bounded so a stalled remote host cannot pin a request thread
// forever. An AVR's vTuner browser waits on one request at a time, so an
// unbounded fetch freezes the whole device UI.
  HTTP_CLIENT_CONNECT_TIMEOUT = 5000;    // ms
  HTTP_CLIENT_IO_TIMEOUT = 15000;        // ms, per socket read

  HTTP_HEADER_ACCEPT = 'Accept';
  HTTP_HEADER_USER_AGENT = 'User-Agent';
  HTTP_HEADER_LOCATION = 'Location';
  HTTP_HEADER_SERVER = 'Server';
  HTTP_HEADER_ACCEPT_ENCODING = 'Accept-Encoding';
  HTTP_HEADER_ACCEPT_LANGUAGE = 'Accept-Language';
  HTTP_HEADER_CACHE_CONTROL = 'Cache-Control';
  HTTP_HEADER_CONNECTION = 'Connection';
  HTTP_HEADER_PRAGMA = 'Pragma';
  HTTP_HEADER_UPGRADE_INSECURE_REQUESTS = 'Upgrade-Insecure-Requests';

  WEBBROWSER_HTTP_HEADER_USER_AGENT = 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0';
  WEBBROWSER_HTTP_HEADER_ACCEPT ='text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9';
  WEBBROWSER_HTTP_HEADER_ACCEPT_ENCODING = 'gzip, deflate';
  WEBBROWSER_HTTP_HEADER_ACCEPT_LANGUAGE = 'en-US,en;q=0.9';
  WEBBROWSER_HTTP_HEADER_CACHE_CONTROL = 'no-cache';
  WEBBROWSER_HTTP_HEADER_CONNECTION = 'keep-alive';
  WEBBROWSER_HTTP_HEADER_PRAGMA = 'no-cache';

  HTTP_RESPONSE_CONTENT_TYPE : array[TResponseContentType] of string = ('text/html; charset=utf-8','application/xml','image/png','image/jpeg','image/gif','image/tiff','application/json');

  MY_STATIONS_PREFIX = 'MS';
  RADIOBROWSER_PREFIX = 'RB';
  UNKNOWN_PREFIX = 'UN';
  PATH_MY_STATIONS = 'mystations';
  PATH_PODCASTS = 'podcasts';

  PATH_PARAM_ID = 'id';
  PATH_PARAM_MAC = 'mac';
  PATH_PARAM_FAV = 'fav';
  PATH_PARAM_SEARCH = 'search';
  PATH_PARAM_SSEARCH_TYPE = 'sSearchtype';
  PATH_PARAM_TOKEN = 'token';

  HTTP_CODE_OK = 200;
  HTTP_CODE_REDIRECT = 302;
  HTTP_CODE_NOT_FOUND = 404;
  HTTP_CODE_UNAVAILABLE = 503;

  DEFAULT_STRING = 'default';
  ESC_CHARS : Array Of AnsiString = ('\t','\n','\r','\b','\f',';');

  PATH_CACHE = 'cache';
  PATH_CONFIG = 'config';
  PATH_DB = 'db';

  CACHE_EXT = '.cache';

// Many radio-browser URLs point at a playlist wrapper rather than the audio
// stream. Firmware of this era often cannot follow one, so Retuner can unwrap it
// first. .m3u8 is deliberately absent: it is HLS, and its entries are variant
// playlists or segments, none of which such a device could play either.
  PLAYLIST_EXTENSIONS : array of string = ('.m3u','.pls','.asx','.xspf');
  PLAYLIST_MAX_BYTES = 65536;
  PLAYLIST_MAX_DEPTH = 2;
  URL_DELIMITERS = ['"','''','<','>',' ',#9,#10,#13];

  ICON_SIZE = 200;
  ICON_CACHE = True;
// On, because the alternative is what shipped: a 404, which a receiver takes as
// final and never retries. Off restores the old behaviour for anyone who
// would rather see the receiver's own blank tile than a mark that is not the
// station's.
  ICON_PLACEHOLDER = True;
// A station logo is fetched from whatever URL the directory carries, so these
// are limits on what a stranger can make this process allocate. TFPMemoryImage
// gives every pixel a TFPColor, which is eight bytes, so the pixel ceiling is
// the one that matters: 16 Mpx is 128 MB of image, and a PNG that declares
// 64000x64000 is a few KB on the wire and 32 GB decoded.
  ICON_MAX_BYTES = 4*1024*1024;
  ICON_MAX_EDGE = 8192;
  ICON_MAX_PIXELS = 16*1024*1024;
// Ids this server issues are a two letter prefix, an underscore and hex.
  ICON_KEY_MAX_LENGTH = 64;
// A Denon sends 32 hex characters as 'mac'; twelve is the older form. 64 leaves
// room for a longer identifier without leaving room for a path.
  AVR_MAC_MAX_LENGTH = 64;
  COMMON_AVR_INI = True;
  RB_POPULAR_AND_SEARCH_STATIONS_LIMIT = 100;
  RB_MIN_STATIONS_PER_CATEGORY = 3;
  RB_UUIDS_CACHE_TTL = 0;
  RB_CACHE_TTL = 0;

  SQLITE_VER_X_MIN = 3;
  SQLITE_VER_Y_MIN = 33;
  SQLITE_VER_Z_MIN = 0;

var
  MyIPAddress: string = DEFAULT_STRING;
  URLHost: string = '';
  LogType: TLogType = ltError;
  MyAppPath: string;
  UseSSL: boolean = True;
  ResolvePlaylists: boolean = False;
// Country whose stations the "local" main-menu entry jumps straight to.
// Empty means the entry is skipped entirely.
  LocalCountry: string = '';
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

var
// Serialises log output. Created before any other unit's initialization can
// log, since every unit depends on this one.
  LogLock: TCriticalSection;

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
function LooksLikePlaylist(AURL: string): boolean;
function ResolveStreamURL(AURL: string): string;

implementation
uses radiobrowserdb;

constructor TLocalHttpClient.Create(AMaxDataToReceive: integer = 0);
begin
  inherited Create(nil);
  ConnectTimeout:=HTTP_CLIENT_CONNECT_TIMEOUT;
  IOTimeout:=HTTP_CLIENT_IO_TIMEOUT;
  FMaxDataToReceive:=AMaxDataToReceive;
  if FMaxDataToReceive>0 then
    OnDataReceived := @HandleDataReceived;
end;

procedure TLocalHttpClient.HandleDataReceived(Sender : TObject; Const ContentLength, CurrentPos : Int64);
begin
  if (FMaxDataToReceive>0) and (CurrentPos>FMaxDataToReceive) then
    Self.Terminate;
end;

procedure Logging(ALogType: TLogType; ALogMessage: string);
begin
  if ALogType<=LogType then
    begin
// An empty message used to index position 1 of an empty string.
      if not ALogMessage.IsEmpty then
        ALogMessage[1]:=UpCase(ALogMessage[1]);
// Every request runs on its own thread and Writeln is not atomic, so without
// this lock lines from concurrent handlers interleave mid-word.
      LogLock.Acquire;
      try
        Writeln(DateTimeToStr(Now)+' : '+LOG_TYPE_MSG[ALogType]+' : '+ALogMessage+'.');
// Output is block-buffered whenever it is not a terminal, which is every way
// this is actually deployed -- systemd, Docker, a Home Assistant add-on. Without
// this the log arrives in 4 KB instalments, so the startup lines saying which
// ports were bound can sit unseen for hours, and a crash loses them entirely.
        Flush(Output);
      finally
        LogLock.Release;
      end;
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
      if Length(Buffer)>0 then
        begin
          ReadBuffer(Buffer[0],Length(Buffer));
          Result:=CRC32(Result,@Buffer[0],Length(Buffer));
        end;
    finally
      Free;
    end;
end;

function RemoveEscChars(LInputStr: RawByteString): RawByteString;
var
  i: integer;
begin
  Result:=LInputStr;
  for i:=Low(ESC_CHARS) to High(ESC_CHARS) do
    Result:=AnsiReplaceStr(Result,ESC_CHARS[i],'');
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
// What dlopen returns is an opaque handle, not a dl_info record. This used to
// cast it to one and read dli_fbase out of it: on glibc the handle points at a
// link_map, whose second field is char *l_name, so the read landed on a real
// path and gave the right answer for the wrong reason. On macOS the same eight
// bytes are not a pointer to anything, and the process died before it finished
// starting - EXC_BAD_ACCESS in FPC_ANSISTR_ASSIGN, which is what a Mac runner
// reported the first time this code was ever run on one. There was already a
// Darwin-only guard here for the empty string it sometimes produced, which is
// the same bug seen from the outside.
//
// The loader found the library by name, so the name is what to pass on:
// TSQLDBLibraryLoader resolves a bare soname exactly the way dlopen just did.
// The absolute path is not recoverable from a handle without dlsym and dladdr,
// and nothing here needs it.
                  LLibFile:=DBLib[LDBLibIdx];
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

function LooksLikePlaylist(AURL: string): boolean;
var
  LPath, LExtension: string;
begin
  Result:=False;
  LPath:=AURL.Split(['?'])[0].Split(['#'])[0].ToLower;
  for LExtension in PLAYLIST_EXTENSIONS do
    if LPath.EndsWith(LExtension) then
      Exit(True);
end;

// Pulls the first playable URL out of a playlist body. One scan copes with all
// four formats: PLS names it as FileN=, M3U puts it on its own line, and the
// XML ones quote it inside an attribute or element.
function FirstStreamURLInPlaylist(const ABody: string): string;
var
  LLine, LCandidate: string;
  LStart, LEnd: integer;
begin
  Result:='';
  for LLine in ABody.Split([#10,#13],TStringSplitOptions.ExcludeEmpty) do
    begin
      LCandidate:=LLine.Trim;
      if LCandidate.IsEmpty or LCandidate.StartsWith('#') or LCandidate.StartsWith(';') then
        Continue;
      if LCandidate.ToLower.StartsWith('file') and LCandidate.Contains('=') then
        LCandidate:=LCandidate.Substring(LCandidate.IndexOf('=')+1).Trim;
      if LCandidate.ToLower.StartsWith('http') then
        Exit(LCandidate);
    end;
// Nothing line-oriented matched, so this is one of the XML formats: take the
// first absolute URL and stop at whatever quotes or closes it.
  LStart:=ABody.ToLower.IndexOf('http://');
  if LStart<0 then
    LStart:=ABody.ToLower.IndexOf('https://');
  if LStart<0 then
    Exit;
  LEnd:=LStart;
  while (LEnd<ABody.Length) and (not (ABody.Chars[LEnd] in URL_DELIMITERS)) do
    Inc(LEnd);
  Result:=ABody.Substring(LStart,LEnd-LStart).Trim;
end;

function FetchPlaylist(AURL: string): string;
var
  LStream: TStringStream;
begin
  Result:='';
  LStream:=TStringStream.Create('');
  try
    with TLocalHttpClient.Create(PLAYLIST_MAX_BYTES) do
      try
        AllowRedirect:=True;
        AddHeader(HTTP_HEADER_USER_AGENT,RETUNER_USER_AGENT+'/'+APP_VERSION);
        try
          Get(AURL,LStream);
        except
          on E: Exception do
// A playlist bigger than the cap is aborted mid-transfer; whatever arrived is
// still worth parsing, since the entry we want is at the top.
            Logging(ltDebug, string.Join(' ',['Playlist',MSG_GETTING,MSG_ERROR,AURL,'('+E.Message+')']));
        end;
      finally
        Free;
      end;
    Result:=LStream.DataString;
  finally
    LStream.Free;
  end;
end;

function ResolveStreamURL(AURL: string): string;
var
  LDepth: integer = 0;
  LNext: string;
begin
  Result:=AURL;
  while (LDepth<PLAYLIST_MAX_DEPTH) and LooksLikePlaylist(Result) do
    begin
      LNext:=FirstStreamURLInPlaylist(FetchPlaylist(Result));
      if LNext.IsEmpty or (LNext=Result) then
        begin
          Logging(ltDebug, string.Join(' ',['Playlist could not be resolved:',Result]));
          Exit;
        end;
      Logging(ltDebug, string.Join(' ',['Playlist resolved:',Result,'->',LNext]));
      Result:=LNext;
      LDepth:=LDepth+1;
    end;
end;

function GetMyAppPath: string;
begin
{$IFDEF UNIX}
// At this moment I have no better idea how to detect Alpine Linux Busybox ?
// ! In this case, enter the Retuner directory first and then run it with ./retuner !
  if ParamStr(0).Contains('ld-musl-') then
    Result:='./'
  else
{$ENDIF}
    Result:=ProgramDirectory;
end;

initialization
  LogLock:=TCriticalSection.Create;

finalization
  LogLock.Free;

end.


