unit radiobrowserdb;

// YTuner: Radio-browser.info DB cache unit.

{$mode ObjFPC}{$H+}

{$IFDEF WINDOWS}
{$R ..\res\sql.rc}
{$ELSE}
{$R ../res/sql.rc}
{$ENDIF}

interface

uses
  Classes, StrUtils, SysUtils,
  SQLDB, SQLite3Conn, SQLite3, SQLite3Backup,
  radiobrowser;

const
  RBDBFILE_NAME = 'rb.db';
  RBDB1FILE_NAME = 'rb1.db';
  RBDB2FILE_NAME = 'rb2.db';
  RBTMPDBFILE_NAME = 'rb-tmp.db';

  RBJSON_COUNTRIES = 'countries';
  RBJSON_LANGUAGES = 'languages';
  RBJSON_CODECS = 'codecs';
  RBJSON_TAGS = 'tags';
  RBJSON_STATIONS = 'stations';

  MSG_RBDB_DB = 'database';
  MSG_RBDB_LIBRARY = 'library';
  MSG_RBDB_ERROR = 'error';
  MSG_RBDB_CONNECTION = 'connection';
  MSG_RBDB_DISCONNECT = 'disconnect';
  MSG_RBDB_CREATE = 'create';
  MSG_RBDB_DELETE = 'delete';
  MSG_RBDB_RESTORE = 'restore';
  MSG_RBDB_BACKUP = 'backup';
  MSG_RBDB_DBINUSE = '(DB in use?)';
  MSG_RBDB_LOCAL = 'local';
  MSG_RBDB_WAIT = 'This may take a while..';
  MSG_RBDB_CLOSING = 'closing';
  MSG_RBDB_CHECKING = 'checking';
  MSG_RBDB_PREPARING = 'preparing';
  MSG_RBDB_CREATING = 'creating';
  MSG_RBDB_REBUILDING = 'rebuilding';
  MSG_RBDB_CLEANING = 'cleaning';
  MSG_RBDB_FILLING = 'filling out';
  MSG_RBDB_EXECUTION = 'execution';
  MSG_RBDB_SCRIPT = 'script';
  MSG_RBDB_QUERY = 'query';
  MSG_RBDB_VIEW = 'view';
  MSG_RBDB_TABLE = 'table';
  MSG_RBDB_TABLES = 'tables';
  MSG_RBDB_INDEXES = 'indexes';
  MSG_RBDB_READY = 'is ready';
  MSG_RBDB_CANNOT = 'Can not';

  RBDB_FIELD_VER = 'ver';
  RBDB_FIELD_TOTALCOUNT = 'totalcount';
  RBDB_FIELD_CATEGORY = 'category';
  RBDB_FIELD_STATIONCOUNT = 'stationcount';
  RBDB_FIELD_NAME = 'name';
  RBDB_FIELD_UUID = 'stationuuid';
  RBDB_FIELD_URL = 'url';
  RBDB_FIELD_FAVICON = 'favicon';
  RBDB_FIELD_HOMEPAGE = 'homepage';
  RBDB_FIELD_COUNTRY = 'country';
  RBDB_FIELD_LANGUAGE = 'language';
  RBDB_FIELD_CODEC = 'codec';
  RBDB_FIELD_BITRATE = 'bitrate';
  RBDB_FIELD_VOTES = 'votes';
  RBDB_FIELD_TAGS = 'tags';
  RBDB_FIELD_POPULAR = 'popular';

  RBDB_SQL_VERSION = 'RBDB_SQL_VERSION';
  RBDB_SQL_LIST_VIEWS = 'RBDB_SQL_LIST_VIEWS';
  RBDB_SQL_CREATE_VIEW_AVR = 'RBDB_SQL_CREATE_VIEW_AVR';
  RBDB_SQL_DROP_VIEW_AVR = 'RBDB_SQL_DROP_VIEW_AVR';
  RBDB_SQL_SELECT_CATEGORY = 'RBDB_SQL_SELECT_CATEGORY';
  RBDB_SQL_SELECT_STATION = 'RBDB_SQL_SELECT_STATION';
  RBDB_SQL_SELECT_STATIONS = 'RBDB_SQL_SELECT_STATIONS';
  RBDB_SQL_SELECT_ROOT = 'RBDB_SQL_SELECT_ROOT';
  RBDB_SQL_ASC = 'ASC';
  RBDB_SQL_DESC = 'DESC';

  RBDB_MARKER_AT = '@@@';
  RBDB_MARKER_JSON = 'JSONCONTENTS';
  RBDB_MARKER_CATEGORY = 'Category';
  RBDB_MARKER_ID = 'ID';
  RBDB_MARKER_AVR = 'AVR';
  RBDB_MARKER_ORDER = 'Order';
  RBDB_MARKER_DIR_ORDER = 'DirOrder';
  RBDB_MARKER_START = 'Start';
  RBDB_MARKER_OFFSET = 'Offset';
  RBDB_MARKER_BEGIN = '--Begin ';
  RBDB_MARKER_END = '--End ';

  RBDB_FIELDS_CATEGORIES : array[TRBAllCategoryTypes] of string = ('tag','country','language','tc','name');

function DBRBValid: boolean;
function DBRBPrepare: boolean;
function DBRBPrepareDBConnection(ADBConnection: TSQLite3Connection; ADBFileName: string; ATag: integer): boolean;
function DBRBGetResourceSQLScript(AResourceName: string): RawByteString;
function DBRBScriptExec(ARBDBConnection: TSQLite3Connection; ASQLScript: string): boolean;
function DBRBScriptExecWithJSON(ARBDBConnection: TSQLite3Connection; ASQLScripts: string; AURL: string): boolean;
function DBRBCheckAVRView(AAVRConfigIdx: integer): boolean;
function DBRBCreateAVRView(AAVRConfigIdx: integer): boolean;
function DBRBGetCategoryItems(var ARBCategories: TRBCategories; ARBCategoryType: TRBCategoryTypes; AAVRConfigIdx, AStart, AHowMany: integer): integer;
function DBRBGetStationsByCategory(var ARBStations: TRBStations; ARBAllCategoryType: TRBAllCategoryTypes; AName: string; AAVRConfigIdx, AStart, AHowMany: integer): integer;
function DBRBGetStationByID(var ARBStation: TRBStation; AID: string; AAVRConfigIdx: integer): integer;
function DBRBGetRootItems(var ARootCategoryTypesCount: TRBRootCategoryTypesCount; AAVRConfigIdx: integer): integer;

var
  DBRBMainConnection: TSQLite3Connection = nil;
  RBDBCreationDateTime: TDateTime = 0;

implementation

uses common, avr, ResStreamUnZipper;

function DBRBValid: boolean;
var
  LDBFileName: string = '';
begin
  Result:=False;
  case RBCacheType of
    catDB, catPermMemDB:
      begin
        LDBFileName:=DBPath+DirectorySeparator+RBDBFILE_NAME;
        try
          Result:=(FileExists(LDBFileName) and ((RBCacheTTL<=0) or (FileDateToDateTime(FileAge(LDBFileName))+(RBCacheTTL/24) > Now)));
        except
          Result:=False;
        end;
      end;
    catMemDB:
      Result:=(Assigned(DBRBMainConnection) and ((RBCacheTTL<=0) or (RBDBCreationDateTime+(RBCacheTTL/24) > Now)));
  end;
end;

function DBRBPrepareDBConnection(ADBConnection: TSQLite3Connection; ADBFileName: string; ATag: integer): boolean;
begin
  Result:=False;
  with ADBConnection do
    try
      LogEvents:=[];
      OpenFlags:=[sofCreate,sofReadWrite];
      if RBCacheType in [catMemDB, catPermMemDB] then
        OpenFlags:=OpenFlags+[sofMemory];
      DatabaseName:=ADBFileName;
      KeepConnection:=True;
      Result:=True;
      Tag:=ATag;
    except
      on E: Exception do
        begin
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_PREPARING, MSG_RBDB_CONNECTION, MSG_RBDB_ERROR,' ('+E.Message+')']));
          Exit;
        end;
    end;
end;

function DBRBGetResourceSQLScript(AResourceName: string): RawByteString;
var
  LResStream: TResourceStream;
  LResultStream: TMemoryStream;
begin
  LResultStream:=TMemoryStream.Create;
  LResStream:=TResourceStream.Create(HInstance,AResourceName,MAKEINTRESOURCE(10));
  try
    LResStream.Position:=0;
    with TResStreamUnZipper.Create do
      try
        Unzip(LResStream,LResultStream);
      finally
        Free;
      end;
    LResultStream.Position:=0;
    SetLength(Result,LResultStream.Size);
    LResultStream.Read(PAnsiChar(Result)^,LResultStream.Size);
  finally
    LResultStream.Free;
    LResStream.Free;
  end;
end;

function DBRBScriptExec(ARBDBConnection: TSQLite3Connection; ASQLScript: string): boolean;
var
  LSQLTransaction: TSQLTransaction;
begin
  Result:=False;
  LSQLTransaction:=TSQLTransaction.Create(nil);
  with TSQLScript.Create(nil) do
    try
      AutoCommit:=True;
      CommentsInSQL:=True;
      UseCommit:=False;
      UseDefines:=False;
      UseSetTerm:=False;
      Database:=ARBDBConnection;
      LSQLTransaction.Options:=[stoUseImplicit];
      LSQLTransaction.DataBase:=ARBDBConnection;
      Transaction:=LSQLTransaction;
      Script.Text:=ASQLScript;
      try
        ExecuteScript;
        Result:=True;
      except
        on E: Exception do
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_SCRIPT, MSG_RBDB_EXECUTION, MSG_RBDB_ERROR, ' : ', ASQLScript, ' ('+E.Message+')']));
      end;
    finally
      Free;
      LSQLTransaction.Free;
    end;
end;

function DBRBScriptExecWithJSON(ARBDBConnection: TSQLite3Connection; ASQLScripts: string; AURL: string): boolean;
const
//  JSON is a cancer!
//  Who invented and why use a human-readable data format for machine-to-machine communication?
//  However, let's split long JSONs to save some memory.
  LPartLen: integer = 100000;
var
  LSQLTransaction: TSQLTransaction;
  i, LJSONArrayItemBegin, LJSONArrayItemPos: integer;
  LSQLScript: string;
  LJSON: AnsiString;
begin
  Result:=False;
  try
    LJSON:=RadiobrowserAPIRequest(AURL);
  except
    on E: Exception do
      begin
        Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_ERROR, MSG_RBDB_CONNECTION]));
        Exit;
      end;
  end;

  if (LJSON<>'[]') and (Length(LJSON)>3) then
    begin
      LJSON:=StringReplace(LJSON,'''','''''',[rfReplaceAll]);
      LSQLTransaction:=TSQLTransaction.Create(nil);
      with TSQLScript.Create(nil) do
        try
          AutoCommit:=True;
          CommentsInSQL:=True;
          UseCommit:=False;
          UseDefines:=False;
          UseSetTerm:=False;
          Database:=ARBDBConnection;
          LSQLTransaction.Options:=[stoUseImplicit];
          LSQLTransaction.DataBase:=ARBDBConnection;
          Transaction:=LSQLTransaction;
          for LSQLScript in ASQLScripts.Split(['--Next Step--']) do
            begin
              if not LSQLScript.Contains(RBDB_MARKER_AT+RBDB_MARKER_JSON) then
                begin
                  Script.Text:=LSQLScript;
                  ExecuteScript;
                end
              else
                begin
                  LJSONArrayItemBegin:=1;
                  LJSONArrayItemPos:=0;
                  i:=1;
                  while LJSONArrayItemPos<LJSON.Length-3 do
                    begin
                      LJSONArrayItemPos:=Pos('},{',LJSON,LJSONArrayItemBegin+i*LPartLen);
                      if LJSONArrayItemPos=0 then
                        LJSONArrayItemPos:=LJSON.Length-1;
                      Script.Text:=LSQLScript.Split([RBDB_MARKER_AT+RBDB_MARKER_JSON])[0]+'''['+LJSON.Substring(LJSONArrayItemBegin,LJSONArrayItemPos-LJSONArrayItemBegin)+']'''+LSQLScript.Split([RBDB_MARKER_AT+RBDB_MARKER_JSON])[1]; //This solution takes less memory.
                      ExecuteScript;
                      LJSONArrayItemBegin:=LJSONArrayItemPos+1;
                      i:=i+1;
                    end;
                end;
            end;
          Result:=True;
        finally
          Free;
          LSQLTransaction.Free;
        end;
    end;
end;

function DBRBPrepare: boolean;
var
  LMainDBConnectionReady: boolean = False;
  LDBConnectionReady: boolean = False;
  LDBReady: boolean = False;
  LTag: integer = 0;
  LDBFileName: string;
  LRBDBConnection: TSQLite3Connection = nil;

  function MainRBDBConnect: boolean;
  begin
    Result:=False;
    if Assigned(DBRBMainConnection) then
      begin
        DBRBMainConnection.Connected:=False;
        if not DBRBMainConnection.Connected then
          FreeAndNil(DBRBMainConnection)
        else
          begin
            Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_CANNOT, MSG_RBDB_DISCONNECT, MSG_RBDB_DB]));
            Exit;
          end;
      end;
    DBRBMainConnection:=LRBDBConnection;
    if not DBRBMainConnection.Connected then
      try
        DBRBMainConnection.Connected:=True;
      except
        on E: Exception do
          begin
            Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_ERROR, MSG_RBDB_CONNECTION,' ('+E.Message+')']));
            Exit;
          end;
      end;
    Result:=DBRBMainConnection.Connected;
  end;

  function CheckAndPrepareDBConnection: boolean;
  begin
    Result:=False;
    if RBDBCreationDateTime+(RBCacheTTL/24) < Now then
      try
        if FileExists(LDBFileName) and not DeleteFile(LDBFileName) then
          begin
            Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_ERROR, MSG_RBDB_DELETE, LDBFileName, MSG_RBDB_DBINUSE]));
            Exit;
          end;
        if not FileExists(LDBFileName)  then
          Result:=DBRBPrepareDBConnection(LRBDBConnection,LDBFileName,LTag);
      except
        on E: Exception do
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_PREPARING, MSG_RBDB_CONNECTION, MSG_RBDB_ERROR,' ('+E.Message+')']));
      end;
  end;

begin
  Result:=False;
  Logging(ltInfo, string.Join(' ',[MSG_RBDB_CHECKING, MSG_RBDB_LOCAL, MSG_RBDB_DB]));
  LRBDBConnection:=TSQLite3Connection.Create(nil);
  try
    if not Assigned(DBRBMainConnection) then
      begin
        case RBCacheType of
          catDB:
            begin
              LDBFileName:=DBPath+DirectorySeparator+RBDBFILE_NAME;
              if (FileExists(LDBFileName)) and ((RBCacheTTL=0) or (FileDateToDateTime(FileAge(LDBFileName))+(RBCacheTTL/24) > Now)) then
                LMainDBConnectionReady:=DBRBPrepareDBConnection(LRBDBConnection,LDBFileName,0);
            end;
          catPermMemDB:
            begin
              LDBFileName:=DBPath+DirectorySeparator+RBDBFILE_NAME;
              if (FileExists(LDBFileName)) and ((RBCacheTTL=0) or (FileDateToDateTime(FileAge(LDBFileName))+(RBCacheTTL/24) > Now)) then
                if DBRBPrepareDBConnection(LRBDBConnection,DBPath+DirectorySeparator+RBDB1FILE_NAME,1) then
                  with TSQLite3Backup.Create do
                    try
                      try
                        Restore(LDBFileName,LRBDBConnection);
                        LMainDBConnectionReady:=True;
                      except
                        on E: Exception do
                          begin
                            Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_CANNOT, MSG_RBDB_RESTORE, MSG_RBDB_DB,' ('+E.Message+')']));
                            Exit;
                          end;
                      end;
                    finally
                      Free;
                    end;
            end;
        end;
        if LMainDBConnectionReady then
          begin
            Result:=MainRBDBConnect;
            if Result then
              RBDBCreationDateTime:=FileDateToDateTime(FileAge(LDBFileName));
            Exit;
          end
        else
          begin
            case RBCacheType of
              catDB:
                begin
                  LDBFileName:=DBPath+DirectorySeparator+RBTMPDBFILE_NAME;
                  LTag:=0;
                end;
              catMemDB, catPermMemDB:
                begin
                  LDBFileName:=DBPath+DirectorySeparator+RBDB1FILE_NAME;
                  LTag:=1;
                end;
            end;
            LDBConnectionReady:=CheckAndPrepareDBConnection;
          end;
      end
    else
      begin
        case RBCacheType of
          catDB:
            begin
              LDBFileName:=DBPath+DirectorySeparator+RBTMPDBFILE_NAME;
              LTag:=0;
            end;
          catMemDB, catPermMemDB:
            begin
              case DBRBMainConnection.Tag of
                1: LDBFileName:=DBPath+DirectorySeparator+RBDB2FILE_NAME;
                2: LDBFileName:=DBPath+DirectorySeparator+RBDB1FILE_NAME;
              end;
              LTag:=(DBRBMainConnection.Tag mod 2)+1;
            end;
        end;
        LDBConnectionReady:=CheckAndPrepareDBConnection;
      end;

    if LDBConnectionReady then
      begin
        try
          LRBDBConnection.Connected:=True;
        except
          on E: Exception do
            begin
              Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_ERROR, MSG_RBDB_CONNECTION,' ('+E.Message+')']));
              Exit;
            end;
        end;
        if not LRBDBConnection.Connected then
          Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_CREATING ,MSG_RBDB_TABLES]))
        else
          begin
            Logging(ltInfo, string.Join(' ',[MSG_RBDB_PREPARING, MSG_RBDB_LOCAL, MSG_RBDB_DB, MSG_RBDB_WAIT]));
            Logging(ltDebug, string.Join(' ',[MSG_RBDB_CREATING, MSG_RBDB_TABLES]));
            if not DBRBScriptExec(LRBDBConnection,DBRBGetResourceSQLScript('RBDB_SQL_CREATE_TABLES')) then
              Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_CREATING ,MSG_RBDB_TABLES]))
            else
              begin
                Logging(ltDebug, string.Join(' ',[MSG_RBDB_FILLING, RBJSON_COUNTRIES, MSG_RBDB_TABLE]));
                if not DBRBScriptExecWithJSON(LRBDBConnection,DBRBGetResourceSQLScript('RBDB_SQL_INSERT_COUNTRIES'),RBJSON_COUNTRIES) then
                  Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_FILLING, RBJSON_COUNTRIES, MSG_RBDB_TABLE]))
                else
                  begin
                    Logging(ltDebug, string.Join(' ',[MSG_RBDB_FILLING, RBJSON_LANGUAGES, MSG_RBDB_TABLE]));
                    if not DBRBScriptExecWithJSON(LRBDBConnection,DBRBGetResourceSQLScript('RBDB_SQL_INSERT_LANGUAGES'),RBJSON_LANGUAGES) then
                      Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_FILLING, RBJSON_LANGUAGES, MSG_RBDB_TABLE]))
                    else
                      begin
                        Logging(ltDebug, string.Join(' ',[MSG_RBDB_FILLING, RBJSON_CODECS, MSG_RBDB_TABLE]));
                        if not DBRBScriptExecWithJSON(LRBDBConnection,DBRBGetResourceSQLScript('RBDB_SQL_INSERT_CODECS'),RBJSON_CODECS) then
                          Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_FILLING, RBJSON_CODECS, MSG_RBDB_TABLE]))
                        else
                          begin
                            Logging(ltDebug, string.Join(' ',[MSG_RBDB_FILLING, RBJSON_TAGS, MSG_RBDB_TABLE]));
                            if not DBRBScriptExecWithJSON(LRBDBConnection,DBRBGetResourceSQLScript('RBDB_SQL_INSERT_TAGS'),RBJSON_TAGS) then
                              Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_FILLING, RBJSON_TAGS, MSG_RBDB_TABLE]))
                            else
                              begin
                                Logging(ltDebug, string.Join(' ',[MSG_RBDB_CREATING, MSG_RBDB_INDEXES]));
                                if not DBRBScriptExec(LRBDBConnection,DBRBGetResourceSQLScript('RBDB_SQL_CREATE_IDX')) then
                                  Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_CREATING, MSG_RBDB_INDEXES]))
                                else
                                  begin
                                    Logging(ltDebug, string.Join(' ',[MSG_RBDB_FILLING, RBJSON_STATIONS, MSG_RBDB_TABLE]));
                                    if not DBRBScriptExecWithJSON(LRBDBConnection,DBRBGetResourceSQLScript('RBDB_SQL_INSERT_STATIONS'),RBJSON_STATIONS) then
                                      Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_FILLING, RBJSON_STATIONS, MSG_RBDB_TABLE]))
                                    else
                                      begin
                                        Logging(ltDebug, string.Join(' ',[MSG_RBDB_CLEANING, MSG_RBDB_TABLES]));
                                        if not DBRBScriptExec(LRBDBConnection,DBRBGetResourceSQLScript('RBDB_SQL_FIX_DATA')) then
                                          Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_CLEANING, MSG_RBDB_TABLES]))
                                        else
                                          begin
                                            Logging(ltDebug, string.Join(' ',[MSG_RBDB_REBUILDING, MSG_RBDB_INDEXES]));
                                            if not DBRBScriptExec(LRBDBConnection,DBRBGetResourceSQLScript('RBDB_SQL_CREATE_REIDX')) then
                                              Logging(ltError, string.Join(' ',[MSG_RBDB_ERROR, MSG_RBDB_REBUILDING, MSG_RBDB_INDEXES]))
                                            else
                                              LDBReady:=True;
                                          end;
                                      end;
                                  end;
                              end;
                          end;
                      end;
                  end;
              end;
          end;
        if LDBReady then
          begin
            case RBCacheType of
              catDB :
                begin
                  if Assigned(DBRBMainConnection) then
                    begin
                      DBRBMainConnection.Connected:=False;
                      if DBRBMainConnection.Connected then
                        begin
                          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_CANNOT, MSG_RBDB_DISCONNECT, MSG_RBDB_DB]));
                          Exit;
                        end;
                    end;
                  if FileExists(DBPath+DirectorySeparator+RBDBFILE_NAME) then
                    begin
                      if not DeleteFile(DBPath+DirectorySeparator+RBDBFILE_NAME) then
                        begin
                          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_ERROR, MSG_RBDB_DELETE, RBTMPDBFILE_NAME, MSG_RBDB_DBINUSE]));
                          Exit;
                        end;
                    end;
                  if not FileExists(DBPath+DirectorySeparator+RBDBFILE_NAME) then
                    begin
                      LRBDBConnection.Connected:=False;
                      if RenameFile(DBPath+DirectorySeparator+RBTMPDBFILE_NAME,DBPath+DirectorySeparator+RBDBFILE_NAME) then
                        LRBDBConnection.DatabaseName:=DBPath+DirectorySeparator+RBDBFILE_NAME;
                    end
                  else
                    begin
                      Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_ERROR, MSG_RBDB_DELETE, RBTMPDBFILE_NAME, MSG_RBDB_DBINUSE]));
                      Exit;
                    end;
                end;
              catPermMemDB :
                begin
                  LDBFileName:=DBPath+DirectorySeparator+RBDBFILE_NAME;
                  if Assigned(DBRBMainConnection) then
                    DBRBMainConnection.Connected:=False;
                  if FileExists(LDBFileName) and not DeleteFile(LDBFileName) then
                    Logging(ltWarning, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_ERROR, MSG_RBDB_DELETE, LDBFileName, MSG_RBDB_DBINUSE]));
                  if not FileExists(LDBFileName) then
                    with TSQLite3Backup.Create do
                      try
                        try
                          Backup(LRBDBConnection,LDBFileName);
                        except
                          on E: Exception do
                            begin
                              Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_CANNOT, MSG_RBDB_BACKUP, MSG_RBDB_DB,' ('+E.Message+')']));
                              Exit;
                            end;
                        end;
                      finally
                        Free;
                      end;
                end;
            end;
            Result:=MainRBDBConnect;
            if Result then
              RBDBCreationDateTime:=Now;
          end;
      end;
  finally
    if Assigned(LRBDBConnection) and (LRBDBConnection<>DBRBMainConnection) then
      begin
        LRBDBConnection.Connected:=False;
        FreeAndNil(LRBDBConnection);
      end;
    if Assigned(DBRBMainConnection) and (LRBDBConnection<>DBRBMainConnection) and (not DBRBMainConnection.Connected) then
      FreeAndNil(DBRBMainConnection);
    if Result then
      Logging(ltInfo, string.Join(' ',[MSG_RBDB_LOCAL, MSG_RBDB_DB, MSG_RBDB_READY]));
  end;
end;

function DBRBCreateAVRView(AAVRConfigIdx: integer): boolean;
var
  LSQLTransaction: TSQLTransaction;
  LSQLScript: TStringList;
  LIntHelper: integer;

  procedure DeleteSection(AMarker: string);
  begin
    while LSQLScript.IndexOf(RBDB_MARKER_BEGIN+AMarker)>=0 do
      begin
        LIntHelper:=LSQLScript.IndexOf(RBDB_MARKER_BEGIN+AMarker);
        while (LSQLScript[LIntHelper]<>RBDB_MARKER_END+AMarker) and (LIntHelper<=LSQLScript.Count-1) do
          LSQLScript.Delete(LIntHelper);
        if LSQLScript[LIntHelper]=RBDB_MARKER_END+AMarker then
          LSQLScript.Delete(LIntHelper);
      end;
  end;

  procedure ReplaceVariable(AMarker: string; AValue: array of string);
  begin
    LSQLScript.Text:=LSQLScript.Text.Replace(RBDB_MARKER_AT+AMarker,StripChars(string.Join(';',AValue),RB_FILTER_STRIP_CHARS),[rfReplaceAll]).Replace('*','%',[rfReplaceAll]);
  end;

begin
  Result:=False;
  LSQLScript:=TStringList.Create;
  LSQLTransaction:=TSQLTransaction.Create(nil);
  with TSQLScript.Create(nil) do
    try
      AutoCommit:=True;
      CommentsInSQL:=True;
      UseCommit:=False;
      UseDefines:=False;
      UseSetTerm:=False;
      Database:=DBRBMainConnection;
      LSQLTransaction.Options:=[stoUseImplicit];
      LSQLTransaction.DataBase:=DBRBMainConnection;
      Transaction:=LSQLTransaction;
      LSQLScript.Text:=DBRBGetResourceSQLScript(RBDB_SQL_CREATE_VIEW_AVR);
      ReplaceVariable(RBDB_MARKER_AVR,AVR_AVR+AVRMACsArray[AAVRConfigIdx]+'_'+Abs(AVRConfigArray[AAVRConfigIdx].CRC32).ToString);
      with AVRConfigArray[AAVRConfigIdx].RBFilter do
        begin
          if Length(AllowedTags)=0 then
            DeleteSection(AVR_INI_ALLOWED_TAGS)
          else
            ReplaceVariable(AVR_INI_ALLOWED_TAGS,AllowedTags);
          if Length(NotAllowedTags)=0 then
            DeleteSection(AVR_INI_NOTALLOWED_TAGS)
          else
            ReplaceVariable(AVR_INI_NOTALLOWED_TAGS,NotAllowedTags);
          if Length(AllowedCountries)=0 then
            DeleteSection(AVR_INI_ALLOWED_COUNTRIES)
          else
            ReplaceVariable(AVR_INI_ALLOWED_COUNTRIES,AllowedCountries);
          if Length(AllowedLanguages)=0 then
            DeleteSection(AVR_INI_ALLOWED_LANGUAGES)
          else
            ReplaceVariable(AVR_INI_ALLOWED_LANGUAGES,AllowedLanguages);
          if Length(AllowedCodecs)=0 then
            DeleteSection(AVR_INI_ALLOWED_CODECS)
          else
            ReplaceVariable(AVR_INI_ALLOWED_CODECS,AllowedCodecs);
          if Length(NotAllowedCodecs)=0 then
            DeleteSection(AVR_INI_NOTALLOWED_CODECS)
          else
            ReplaceVariable(AVR_INI_NOTALLOWED_CODECS,NotAllowedCodecs);
          if Length(NotAllowedInName)=0 then
            DeleteSection(AVR_INI_NOTALLOWED_IN_NAME)
          else
            ReplaceVariable(AVR_INI_NOTALLOWED_IN_NAME,NotAllowedInName);
          if Length(NotAllowedInURL)=0 then
            DeleteSection(AVR_INI_NOTALLOWED_IN_URL)
          else
            ReplaceVariable(AVR_INI_NOTALLOWED_IN_URL,NotAllowedInURL);
          if BitrateMax=0 then
            DeleteSection(AVR_INI_BITRATEMAX)
          else
            ReplaceVariable(AVR_INI_BITRATEMAX,BitrateMax.ToString.Split([';']));
        end;
      try
        Script:=LSQLScript;
        ExecuteScript;
        Result:=True;
      except
        on E: Exception do
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_SCRIPT, MSG_RBDB_EXECUTION, MSG_RBDB_ERROR, ' : ', RBDB_SQL_CREATE_VIEW_AVR, ' ('+E.Message+')']));
      end;
    finally
      LSQLScript.Free;
      Free;
      LSQLTransaction.Free;
    end;
end;

function DBRBCheckAVRView(AAVRConfigIdx: integer): boolean;
var
  LView: string;
  LSQLTransaction: TSQLTransaction;
begin
  Result:=False;
  LSQLTransaction:=TSQLTransaction.Create(nil);
  with TSQLQuery.Create(nil) do
    try
      ReadOnly:=True;
      DataBase:=DBRBMainConnection;
      LSQLTransaction.Options:=[stoUseImplicit];
      LSQLTransaction.DataBase:=DBRBMainConnection;
      Transaction:=LSQLTransaction;
      SQL.Text:=DBRBGetResourceSQLScript(RBDB_SQL_LIST_VIEWS);
      try
        Open;
        First;
        while not EOF do
          begin
            LView:=FieldByName(RBDB_FIELD_NAME).AsString;
            if (Length(LView.Split(['_']))=2)
              and (LView.Split(['_'])[0]=AVR_AVR+AVRMACsArray[AAVRConfigIdx])
              and (LView.Split(['_'])[1]<>Abs(AVRConfigArray[AAVRConfigIdx].CRC32).ToString)
              and (not DBRBScriptExec(DBRBMainConnection,StringReplace(DBRBGetResourceSQLScript(RBDB_SQL_DROP_VIEW_AVR),RBDB_MARKER_AT+RBDB_MARKER_AVR,LView,[rfReplaceAll]))) then
              Logging(ltWarning, string.Join(' ',[{$I %CURRENTROUTINE%}+':', MSG_RBDB_DELETE, MSG_RBDB_VIEW, MSG_RBDB_ERROR, ' : ', LView]));
            Next;
          end;
        Transaction.Active:=False;
        Result:=DBRBCreateAVRView(AAVRConfigIdx);
      except
        on E: Exception do
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':', MSG_RBDB_QUERY, MSG_RBDB_EXECUTION, MSG_RBDB_ERROR, ' : ', RBDB_SQL_LIST_VIEWS, ' ('+E.Message+')']));
      end;
    finally
      Close;
      Free;
      LSQLTransaction.Free;
    end;
end;

function DBRBGetCategoryItems(var ARBCategories: TRBCategories; ARBCategoryType: TRBCategoryTypes; AAVRConfigIdx, AStart, AHowMany: integer): integer;
var
  LRBCategory: TRBCategory;
  LSQLTransaction: TSQLTransaction;
begin
  Result:=0;
  LSQLTransaction:=TSQLTransaction.Create(nil);
  with TSQLQuery.Create(nil) do
    try
      ReadOnly:=True;
      DataBase:=DBRBMainConnection;
      LSQLTransaction.Options:=[stoUseImplicit];
      LSQLTransaction.DataBase:=DBRBMainConnection;
      Transaction:=LSQLTransaction;
      SQL.Text:=DBRBGetResourceSQLScript(RBDB_SQL_SELECT_CATEGORY);
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_CATEGORY,RBDB_FIELDS_CATEGORIES[ARBCategoryType],[rfReplaceAll]);
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_AVR,AVR_AVR+AVRMACsArray[AAVRConfigIdx]+'_'+Abs(AVRConfigArray[AAVRConfigIdx].CRC32).ToString,[rfReplaceAll]);
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_START,AStart.ToString);
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_OFFSET,AHowMany.ToString);
      try
        Open;
        First;
        if not EOF then
          begin
            Result:=FieldByName(RBDB_FIELD_TOTALCOUNT).AsInteger;
            while not EOF do
              begin
                LRBCategory:=TRBCategory.Create;
                LRBCategory.RBCName:=FieldByName(RBDB_FIELD_CATEGORY).AsString;
                LRBCategory.RBCMaxCount:=FieldByName(RBDB_FIELD_STATIONCOUNT).AsInteger;
                ARBCategories.AddObject(LRBCategory.RBCName,LRBCategory);
                Next;
              end;
          end;
      except
        on E: Exception do
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_QUERY, MSG_RBDB_EXECUTION, MSG_RBDB_ERROR, ' : ', RBDB_FIELDS_CATEGORIES[ARBCategoryType], ' ('+E.Message+')']));
      end;
    finally
      Close;
      Free;
      LSQLTransaction.Free;
    end;
end;

function DBRBGetStationsByCategory(var ARBStations: TRBStations; ARBAllCategoryType: TRBAllCategoryTypes; AName: string; AAVRConfigIdx, AStart, AHowMany: integer): integer;
var
  LRBStation: TRBStation;
  LSQLTransaction: TSQLTransaction;
begin
  Result:=0;
  LSQLTransaction:=TSQLTransaction.Create(nil);
  with TSQLQuery.Create(nil) do
    try
      ReadOnly:=True;
      DataBase:=DBRBMainConnection;
      LSQLTransaction.Options:=[stoUseImplicit];
      LSQLTransaction.DataBase:=DBRBMainConnection;
      Transaction:=LSQLTransaction;
      SQL.Text:=DBRBGetResourceSQLScript(RBDB_SQL_SELECT_STATIONS);
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_AVR,AVR_AVR+AVRMACsArray[AAVRConfigIdx]+'_'+Abs(AVRConfigArray[AAVRConfigIdx].CRC32).ToString,[rfReplaceAll]);
      case ARBAllCategoryType of
        rbctGenre..rbctLanguage:
          begin
            SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_CATEGORY,RBDB_FIELDS_CATEGORIES[ARBAllCategoryType]+'= '''+StripChars(AName,RB_FILTER_STRIP_CHARS)+'''',[rfReplaceAll]);
            SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_ORDER,AVRConfigArray[AAVRConfigIdx].RBSort.Order);
            SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_DIR_ORDER,IfThen(AVRConfigArray[AAVRConfigIdx].RBSort.Reverse,RBDB_SQL_DESC,RBDB_SQL_ASC));
          end;
        rbctPopular:
          begin
            SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_CATEGORY,RBDB_FIELDS_CATEGORIES[ARBAllCategoryType]+'=1',[rfReplaceAll]);
            SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_ORDER,RBDB_FIELD_VOTES);
            SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_DIR_ORDER,RBDB_SQL_DESC);
          end;
        rbctSearch:
          begin
            SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_CATEGORY,RBDB_FIELDS_CATEGORIES[ARBAllCategoryType]+' LIKE ''%'+StripChars(AName,RB_FILTER_STRIP_CHARS)+'%''',[rfReplaceAll]);
            SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_ORDER,AVRConfigArray[AAVRConfigIdx].RBSort.Order);
            SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_DIR_ORDER,IfThen(AVRConfigArray[AAVRConfigIdx].RBSort.Reverse,RBDB_SQL_DESC,RBDB_SQL_ASC));
          end;
      end;
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_START,AStart.ToString);
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_OFFSET,AHowMany.ToString);
      try
        Open;
        First;
        if not EOF then
          begin
            Result:=FieldByName(RBDB_FIELD_TOTALCOUNT).AsInteger;
            while not EOF do
              begin
                LRBStation:=TRBStation.Create;
                LRBStation.RBSID:=FieldByName(RBDB_FIELD_UUID).AsString;
                LRBStation.RBSName:=FieldByName(RBDB_FIELD_NAME).AsString;
                LRBStation.RBSURL:=FieldByName(RBDB_FIELD_URL).AsString;
                LRBStation.RBSIcon:=FieldByName(RBDB_FIELD_FAVICON).AsString;
                LRBStation.RBSHomePageURL:=FieldByName(RBDB_FIELD_HOMEPAGE).AsString;
                LRBStation.RBSCountry:=FieldByName(RBDB_FIELD_COUNTRY).AsString;
                LRBStation.RBSLanguage:=FieldByName(RBDB_FIELD_LANGUAGE).AsString;
                LRBStation.RBSCodec:=FieldByName(RBDB_FIELD_CODEC).AsString;
                LRBStation.RBSBitrate:=FieldByName(RBDB_FIELD_BITRATE).AsString;
                LRBStation.RBSVotes:=FieldByName(RBDB_FIELD_VOTES).AsString;
                LRBStation.RBSTags:=FieldByName(RBDB_FIELD_TAGS).AsString;
                ARBStations.AddObject(LRBStation.RBSID,LRBStation);
                Next;
              end;
          end;
      except
        on E: Exception do
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_QUERY, MSG_RBDB_EXECUTION, MSG_RBDB_ERROR, ' : ', RBDB_FIELDS_CATEGORIES[ARBAllCategoryType], ' ('+E.Message+')']));
      end;
    finally
      Close;
      Free;
      LSQLTransaction.Free;
    end;
end;

function DBRBGetStationByID(var ARBStation: TRBStation; AID: string; AAVRConfigIdx: integer): integer;
var
  LSQLTransaction: TSQLTransaction;
begin
  Result:=0;
  LSQLTransaction:=TSQLTransaction.Create(nil);
  with TSQLQuery.Create(nil) do
    try
      ReadOnly:=True;
      DataBase:=DBRBMainConnection;
      LSQLTransaction.Options:=[stoUseImplicit];
      LSQLTransaction.DataBase:=DBRBMainConnection;
      Transaction:=LSQLTransaction;
      SQL.Text:=DBRBGetResourceSQLScript(RBDB_SQL_SELECT_STATION);
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_AVR,AVR_AVR+AVRMACsArray[AAVRConfigIdx]+'_'+Abs(AVRConfigArray[AAVRConfigIdx].CRC32).ToString,[rfReplaceAll]);
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_ID,''''+StripChars(AID,RB_FILTER_STRIP_CHARS).Insert(8,'-')+'-%''');
      try
        Open;
        First;
        if not EOF then
          with ARBStation do
            begin
              Result:=1;
              RBSID:=FieldByName(RBDB_FIELD_UUID).AsString;
              RBSName:=FieldByName(RBDB_FIELD_NAME).AsString;
              RBSURL:=FieldByName(RBDB_FIELD_URL).AsString;
              RBSIcon:=FieldByName(RBDB_FIELD_FAVICON).AsString;
              RBSHomePageURL:=FieldByName(RBDB_FIELD_HOMEPAGE).AsString;
              RBSCountry:=FieldByName(RBDB_FIELD_COUNTRY).AsString;
              RBSLanguage:=FieldByName(RBDB_FIELD_LANGUAGE).AsString;
              RBSCodec:=FieldByName(RBDB_FIELD_CODEC).AsString;
              RBSBitrate:=FieldByName(RBDB_FIELD_BITRATE).AsString;
              RBSVotes:=FieldByName(RBDB_FIELD_VOTES).AsString;
              RBSTags:=FieldByName(RBDB_FIELD_TAGS).AsString;
            end;
      except
        on E: Exception do
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_QUERY, MSG_RBDB_EXECUTION, MSG_RBDB_ERROR, ' : ', AID, ' ('+E.Message+')']));
      end;
    finally
      Close;
      Free;
      LSQLTransaction.Free;
    end;
end;

function DBRBGetRootItems(var ARootCategoryTypesCount: TRBRootCategoryTypesCount; AAVRConfigIdx: integer): integer;
var
  LSQLTransaction: TSQLTransaction;
begin
  Result:=0;
  LSQLTransaction:=TSQLTransaction.Create(nil);
  with TSQLQuery.Create(nil) do
    try
      ReadOnly:=True;
      DataBase:=DBRBMainConnection;
      LSQLTransaction.Options:=[stoUseImplicit];
      LSQLTransaction.DataBase:=DBRBMainConnection;
      Transaction:=LSQLTransaction;
      SQL.Text:=DBRBGetResourceSQLScript(RBDB_SQL_SELECT_ROOT);
      SQL.Text:=SQL.Text.Replace(RBDB_MARKER_AT+RBDB_MARKER_AVR,AVR_AVR+AVRMACsArray[AAVRConfigIdx]+'_'+Abs(AVRConfigArray[AAVRConfigIdx].CRC32).ToString,[rfReplaceAll]);
      try
        Open;
        if not EOF then
          begin
            ARootCategoryTypesCount[rbctGenre]:=FieldByName(RBDB_FIELD_TAGS).AsInteger;
            ARootCategoryTypesCount[rbctCountry]:=FieldByName(RBDB_FIELD_COUNTRY).AsInteger;
            ARootCategoryTypesCount[rbctLanguage]:=FieldByName(RBDB_FIELD_LANGUAGE).AsInteger;
            ARootCategoryTypesCount[rbctPopular]:=FieldByName(RBDB_FIELD_POPULAR).AsInteger;
            Result:=1;
          end;
      except
        on E: Exception do
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',MSG_RBDB_QUERY, MSG_RBDB_EXECUTION, MSG_RBDB_ERROR, ' ('+E.Message+')']));
      end;
    finally
      Close;
      Free;
      LSQLTransaction.Free;
    end;
end;

end.

