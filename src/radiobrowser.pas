unit radiobrowser;

// YTuner: Radio-browser.info API feature support unit.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  fpjson, jsonparser,
  fphttpclient, httpprotocol, httpdefs,
  RegExpr, StrUtils, DateUtils, FileUtil,
  common, avr;

type
  TRBStation = class(TObject)
    RBSID, RBSName, RBSHomePageURL, RBSURL, RBSIcon, RBSTags, RBSCountry, RBSCountryCode, RBSLanguage, RBSVotes, RBSCodec, RBSBitrate : string;
    procedure Assign(Source: TRBStation);
  end;

  TRBCategory = class(TObject)
    RBCName : string;
    RBCMaxCount: integer;
  end;

  TRBObjectsList = class(TStringList)
    constructor Create;
  end;

  TRBStations = TRBObjectsList;
  TRBCategories = TRBObjectsList;

  TRBAllCategoryTypes = (rbctGenre, rbctCountry, rbctLanguage, rbctPopular, rbctSearch);
  TRBCategoryTypes = rbctGenre..rbctLanguage;

  TRBRootCategoryTypesCount = array[TRBAllCategoryTypes] of integer;

  TRBCacheObject = class(TObject)
    CRBRecords: RawByteString;
    CRBEOL: TDateTime;
  end;

  TRBCache = TStringList;

const
  PATH_RADIOBROWSER = 'radiobrowser';
  API_SEARCH_PATH = 'search';
  API_STATIONS_PATH = 'stations';
  API_ORDER_PATH = 'order';
  API_REVERSE_PATH = 'reverse';
  API_OFFSET_PATH = 'offset';
  API_LIMIT_PATH = 'limit';

  API_POPULAR_FILTER_PATH = '?order=votes&reverse=true&limit=';
  API_SEARCH_FILTER_PATH = '?order=name&reverse=false&limit=';
  API_HIDEBROKEN_FILTER_PATH = 'hidebroken=true';
//  API_CHECK_FILTER_PATH = 'lastcheckok=1';

  API_BYTAGEXACT_PATH = '/bytagexact/';
  API_BYCOUNTRYEXACT_PATH = '/bycountryexact/';
  API_BYLANGUAGEEXACT_PATH = '/bylanguageexact/';
  API_BYUUID_PATH = '/byuuid/';

  API_ATTR_STATIONUUID = 'stationuuid';
  API_ATTR_SERVERUUID = 'serveruuid';
  API_ATTR_NAME = 'name';
  API_ATTR_URL = 'url';
  API_ATTR_HOMEPAGE = 'homepage';
  API_ATTR_FAVICON ='favicon';
  API_ATTR_TAGS = 'tags';
  API_ATTR_COUNTRY = 'country';
  API_ATTR_COUNTRYCODE = 'countrycode';
  API_ATTR_LANGUAGE = 'language';
  API_ATTR_VOTES = 'votes';
  API_ATTR_CODEC = 'codec';
  API_ATTR_BITRATE = 'bitrate';
  API_ATTR_STATIONCOUNT = 'stationcount';

  API_ATTR_PROPERTIES:string = ','+API_ATTR_STATIONUUID+','+API_ATTR_NAME+','+API_ATTR_URL+','+API_ATTR_FAVICON+','+API_ATTR_TAGS+','+API_ATTR_COUNTRY+','+API_ATTR_LANGUAGE+','+API_ATTR_CODEC+','+API_ATTR_BITRATE+',';

  MSG_RADIOBROWSER = 'Radio-browser.info';
  MSG_RBCACHE = 'cache';
  MSG_RBCACHE_FILETYPE = 'catFile';
  MSG_RBCACHE_MEMSTRTYPE = 'catMemStr';
  MSG_RBCACHE_TTLEXPRELOAD = 'TTL expired. Reloading';

  RADIOBROWSER_UUIDS_FILE = 'rbuuids.txt';

  RBCACHE_STATIONS_PREFIX = 'RBS-';
  RBCACHE_CATEGORIES_PREFIX = 'RBC-';

  HIDE_BROKEN_STATIONS : boolean = True;
  RB_CATEGORIES_PATH : array[TRBCategoryTypes] of string = ('tags','countries','languages');
  RB_JSON_STRIP_CHARS: array of AnsiString =  ('!','*','''','(',')',';',':','@','&','=','+','$',',','/','?','%','#','[',']','-','_','.','~',#10,#13,#09);
  RB_FILTER_STRIP_CHARS: array of AnsiString =  ('!','"','''','(',')',':','@','&','=','+','$',',','/','?','%','#','[',']','-','.','~',#10,#13,#09);

var
  RadioBrowserEnabled: boolean = True;
  RBAPIURL: string = 'http://all.api.radio-browser.info';
  RBPopularAndSearchStationsLimit: integer = RB_POPULAR_AND_SEARCH_STATIONS_LIMIT;
  RBMinStationsPerCategory: integer = RB_MIN_STATIONS_PER_CATEGORY;
  RBUUIDsFilePath: string = '';
  RBUUIDsCacheTTL: integer = RB_UUIDS_CACHE_TTL;
  RBUUIDsCacheAutoRefresh: boolean = False;
  RBCacheTTL: integer = RB_CACHE_TTL;
  RBCacheType: TCacheType = catFile;
  RBStationsUUIDs: string = '';
  RBCache: TRBCache;

function GetAPIURLRange(AElementNumber,AElementCount: integer): string;
function RadiobrowserAPIRequest(AURL: string): RawByteString;
function RadiobrowserAPIRequestJSON(AURL: string): TJSONData;
function GetRootItems(var ARootCategoryTypesCount: TRBRootCategoryTypesCount; AAVRConfigIdx: integer): integer;
function GetCategoryItemsCount(ARBCategoryType: TRBCategoryTypes; AAVRConfigIdx: integer): integer;
function GetCategoryItems(var ARBCategories: TRBCategories; ARBCategoryType: TRBCategoryTypes; AReq: TRequest): integer;
function GetCategoryItems(var ARBCategories: TRBCategories; ARBCategoryType: TRBCategoryTypes; AAVRConfigIdx, AStart, AHowMany: integer): integer;
function GetStationsByCategory(var ARBStations: TRBStations; ARBAllCategoryType: TRBAllCategoryTypes; AName: string; AReq: TRequest): integer;
function GetStationsByCategory(var ARBStations: TRBStations; ARBAllCategoryType: TRBAllCategoryTypes; AName: string; AAVRConfigIdx, AStart, AHowMany: integer): integer;
procedure GetRBStationByID(var ARBStation: TRBStation; AID: string; AAVRConfigIdx: integer);
procedure SetRBCategory(var ARBCategory: TRBCategory; ARBCategoryJSONObject: TJSONObject);
procedure SetRBStation(var ARBStation: TRBStation; ARBStationJSONObject: TJSONObject);
function LoadRBStationsUUIDs: boolean;
procedure GetRBStationsUUIDs;
function RBCategoriesAVRFilter(AJSONObject: TJSONObject; ARBCategoryType: TRBCategoryTypes; AAVRConfigIdx: integer): boolean;
function RBStationsAVRFilter(ARBStationJSONObject: TJSONObject; ARBAllCategoryType: TRBAllCategoryTypes; AAVRConfigIdx: integer): boolean;
function GetRBCache(var ARBObjectsList: TRBObjectsList; ACacheName: string; AAVRConfigIdx, AStart, AHowMany: integer): integer;
function SetRBCache(ARBObjectsList: TRBObjectsList; ACacheName: string; AAVRConfigIdx: integer): boolean;
function GetRBStationCache(var ARBStation: TRBStation; AUUID: string; AAVRConfigIdx: integer): boolean;
function RemoveEmptyCategory(ARBAllCategoryType: TRBAllCategoryTypes; ACacheName,AName: string; AAVRConfigIdx: integer): boolean;
function LoadRBObjects(ARBObjectsList: TRBObjectsList; ACRBObjects: RawByteString; AStart, AHowMany: integer; AObjectName: string = ''): integer;
function LoadRBObjects(ARBObjectsList: TRBObjectsList; AFileName: string; AStart, AHowMany: integer; AObjectName: string = ''): integer;
function SaveRBObjects(ARBObjectsList: TRBObjectsList; AFileName: string): boolean;
function SaveRBObjects(ARBObjectsList: TRBObjectsList; var ACRBObjects: RawByteString): boolean;
function SerializeRBObjects(ARBObjectsList: TRBObjectsList; var ACacheStream: TRawByteStringStream): boolean;
function DeSerializeRBObjects(ARBObjectsList: TRBObjectsList; var ACacheStream: TRawByteStringStream; AStart, AHowMany: integer; AObjectName: string = ''): integer;
procedure LoadRBCacheFilesInfo(AAVRConfigIdx: integer);
procedure RemoveOldRBCacheFiles;

implementation

uses httpserver, radiobrowserdb;

procedure TRBStation.Assign(Source: TRBStation);
begin
  if Assigned(Source) then
    begin
      RBSID:=Source.RBSID;
      RBSName:=Source.RBSName;
      RBSHomePageURL:=Source.RBSHomePageURL;
      RBSURL:=Source.RBSURL;
      RBSIcon:=Source.RBSIcon;
      RBSTags:=Source.RBSTags;
      RBSCountry:=Source.RBSCountry;
      RBSCountryCode:=Source.RBSCountryCode;
      RBSLanguage:=Source.RBSLanguage;
      RBSVotes:=Source.RBSVotes;
      RBSCodec:=Source.RBSCodec;
      RBSBitrate:=Source.RBSBitrate;
    end;
end;

constructor TRBObjectsList.Create;
begin
  inherited Create;
  OwnsObjects:=True;
end;

function GetAPIURLRange(AElementNumber,AElementCount: integer): string;
begin
  Result:='?'+API_OFFSET_PATH+'='+IntToStr(AElementNumber)+'&'+API_LIMIT_PATH+'='+IntToStr(AElementCount);
end;

function RadiobrowserAPIRequest(AURL: string): RawByteString;
begin
  Logging(ltDebug,MSG_RADIOBROWSER+': GET '+AURL);
  with TFPHttpClient.Create(nil) do
  try
    try
      AddHeader(HTTP_HEADER_ACCEPT,HTTP_RESPONSE_CONTENT_TYPE[ctJSON]);
      AddHeader(HTTP_HEADER_USER_AGENT,YTUNER_USER_AGENT+'/'+APP_VERSION);
      Result:=RemoveEscChars(Get(RBAPIURL+'/json/'+AURL));
      Result:=Get(RBAPIURL+'/json/'+AURL);
    except
      Logging(ltError,MSG_RADIOBROWSER+': '+MSG_ERROR+' '+MSG_GETTING+' '+AURL);
      Result:='[]';
    end;
  finally
    Free;
  end;
end;

function RadiobrowserAPIRequestJSON(AURL: string): TJSONData;
begin
  Result:=GetJSON(RadiobrowserAPIRequest(AURL));
end;

procedure SetRBCategory(var ARBCategory: TRBCategory; ARBCategoryJSONObject: TJSONObject);
begin
  with ARBCategory, ARBCategoryJSONObject do
    begin
      RBCName:=StripChars(Get(API_ATTR_NAME),RB_JSON_STRIP_CHARS);
      RBCMaxCount:=Get(API_ATTR_STATIONCOUNT);
    end;
end;

procedure SetRBStation(var ARBStation: TRBStation; ARBStationJSONObject: TJSONObject);
begin
  with ARBStation, ARBStationJSONObject do
    begin
      RBSID:=Get(API_ATTR_STATIONUUID);
      RBSName:=StripChars(Get(API_ATTR_NAME),RB_JSON_STRIP_CHARS);
      RBSHomePageURL:=Get(API_ATTR_HOMEPAGE);
      RBSURL:=Get(API_ATTR_URL);
      RBSIcon:=Get(API_ATTR_FAVICON);
      RBSTags:=StripChars(Get(API_ATTR_TAGS),RB_JSON_STRIP_CHARS);
      RBSCountry:=StripChars(Get(API_ATTR_COUNTRY),RB_JSON_STRIP_CHARS);
      RBSCountryCode:=Get(API_ATTR_COUNTRYCODE);
      RBSLanguage:=StripChars(Get(API_ATTR_LANGUAGE),RB_JSON_STRIP_CHARS);
      RBSVotes:=Get(API_ATTR_VOTES);
      RBSCodec:=Get(API_ATTR_CODEC);
      RBSBitrate:=Get(API_ATTR_BITRATE);
    end;
end;

function LoadRBStationsUUIDs: boolean;
var
  LFA: Int64;

  function LoadRBStationsUUIDsFromFile:boolean;
  var
    F: Text;
  begin
    Result:=False;
    if FileExists(RBUUIDsFilePath) then
      begin
        AssignFile(F,RBUUIDsFilePath);
        Reset(F);
        try
          ReadLn(F,RBStationsUUIDs);
          Result:=True;
        finally
          Close(F);
        end;
      end;
  end;

begin
  Result:=False;
  RBUUIDsFilePath:=CachePath+DirectorySeparator+RADIOBROWSER_UUIDS_FILE;
  if FileExists(RBUUIDsFilePath) then
    try
      Logging(ltInfo, MSG_LOADING+' '+MSG_RBUUID_CACHE_FILE);
// Alternatively:
//      RBStationsUUIDs:=GetFileAsString(CachePath+DirectorySeparator+RADIOBROWSER_UUIDS_FILE);
//      if RBStationsUUIDs.Length>0 then
      if LoadRBStationsUUIDsFromFile then
        begin
          if (RBStationsUUIDs.Length<=2) then
            Logging(ltWarning, MSG_RBUUID_CACHE_FILE+' seems empty..')
          else
            begin
              Logging(ltInfo, MSG_SUCCESSFULLY_LOADED+IfThen(RBStationsUUIDs.IsEmpty,'0',IntToStr(RBStationsUUIDs.CountChar(',')+1))+' RB UUIDs from cache file');
              LFA:=FileAge(CachePath+DirectorySeparator+RADIOBROWSER_UUIDS_FILE);
              if (LFA<>-1) and ((RBUUIDsCacheTTL<0) or (HoursBetween(Now,FileDateToDateTime(LFA))<RBUUIDsCacheTTL)) then
                Result:=True
              else
                Logging(ltInfo, MSG_RBUUID_CACHE_FILE+' is older then '+IntToStr(RBUUIDsCacheTTL)+' hours. Refreshing RB UUIDs list..');
            end;
        end;
    except
      Logging(ltError, MSG_ERROR_LOAD+' '+MSG_RBUUID_CACHE_FILE);
    end;
end;

procedure GetRBStationsUUIDs;
var
  LLoadError: boolean = False;
  LStations: TStrings;

  function SaveRBStationsUUIDsToFile:boolean;
  var
    F: Longint;
  begin
    Result:=False;
    if FileExists(RBUUIDsFilePath) then
      begin
        F:=FileOpen(RBUUIDsFilePath,fmOpenReadWrite);
        if not FileTruncate(F,0) then
          if DeleteFile(RBUUIDsFilePath) then
            F:=FileCreate(RBUUIDsFilePath)
          else
            begin
              Logging(ltError, RADIOBROWSER_UUIDS_FILE+' access error');
              Exit;
            end
      end
    else
      F:=FileCreate(RBUUIDsFilePath);
    if F<>-1 then
      begin
        FileWrite(F,RBStationsUUIDs[1],RBStationsUUIDs.Length);
        FileClose(F);
        Result:=True;
      end;
  end;

begin
  Logging(ltInfo, MSG_GETTING+' '+MSG_RADIOBROWSER+' UUIDs..');
  LStations:=TStringList.Create;
  try
    try
      SplitRegExpr('","'+API_ATTR_SERVERUUID+'".*?"'+API_ATTR_STATIONUUID+'":"',RadiobrowserAPIRequest(API_STATIONS_PATH),LStations);
    except
      On E: Exception do
        begin
          LLoadError:=True;
          Logging(ltWarning, MSG_RADIOBROWSER+' UUIDs '+MSG_NOT_LOADED+'. Error: '+E.Message);
        end;
    end;
  finally
    if not LLoadError then
      begin
        LStations[0]:=LStations[0].Remove(0,LStations[0].IndexOf('"'+API_ATTR_STATIONUUID+'":"')+15);
        LStations[LStations.Count-1]:=LStations[LStations.Count-1].Substring(0,36);
        RBStationsUUIDs:=LStations.CommaText;
        if RBStationsUUIDs.Length>2 then
          begin
            Logging(ltInfo, MSG_SUCCESSFULLY_DOWNLOADED+IfThen(RBStationsUUIDs.IsEmpty,'0',IntToStr(RBStationsUUIDs.CountChar(',')+1))+' RB UUIDs');
            if SaveRBStationsUUIDsToFile then
              Logging(ltInfo, MSG_SUCCESSFULLY_SAVED+IfThen(RBStationsUUIDs.IsEmpty,'0',IntToStr(RBStationsUUIDs.CountChar(',')+1))+' RB UUIDs to cache file');
          end;
      end;
    LStations.Free;
  end;
end;

procedure GetRBStationByID(var ARBStation: TRBStation; AID: string; AAVRConfigIdx: integer);
var
  LMatches: SizeIntArray;
  LUUID: string;
begin
  if RBCacheType in [catDB, catMemDB, catPermMemDB] then
    DBRBGetStationByID(ARBStation,AID,AAVRConfigIdx)
  else
    if FindMatchesBoyerMooreCaseInSensitive(RBStationsUUIDs,AID.Insert(8,'-')+'-',LMatches,False) then
      try
        LUUID:=RBStationsUUIDs.Substring(LMatches[0]-1,36);
        if (RBCacheType=catNone) or (not GetRBStationCache(ARBStation,LUUID,AAVRConfigIdx)) then
          with TJSONArray(RadiobrowserAPIRequestJSON(API_STATIONS_PATH+API_BYUUID_PATH+LUUID)) do
            try
              SetRBStation(ARBStation, Objects[0]);
            finally
              Free;
            end;
      except
        on E: Exception do
          begin
            ARBStation.RBSID:='';
            Logging(ltError,'GetRBStationByID error: '+E.Message);
          end;
      end
    else
      ARBStation.RBSID:='';
end;

function GetStationsByCategory(var ARBStations: TRBStations; ARBAllCategoryType: TRBAllCategoryTypes; AName: string; AReq: TRequest): integer;
var
  LStart: integer = 0;
  LHowMany: integer = 0;
begin
  GetRange(LStart,LHowMany,AReq.QueryFields);
  if RBCacheType in [catDB, catMemDB, catPermMemDB] then
    Result:=DBRBGetStationsByCategory(ARBStations,ARBAllCategoryType,HTTPDecode(AName),GetAVRConfigIdx(AReq),LStart,LHowMany)
  else
    Result:=GetStationsByCategory(ARBStations,ARBAllCategoryType,AName,GetAVRConfigIdx(AReq),LStart,LHowMany);
end;

function GetStationsByCategory(var ARBStations: TRBStations; ARBAllCategoryType: TRBAllCategoryTypes; AName: string; AAVRConfigIdx, AStart, AHowMany: integer): integer;
var
  LURL: string;
  LJSONArray: TJSONArray = nil;
  LItemsFiltered: integer = 0;
  LItemsAdded: integer = 0;
  LItemIdx: integer = 0;
  LSortingPath: string;
  LRBStation: TRBStation;
  LRBStations: TRBStations;
  LCacheName: string;
begin
  Result:=0;
  LCacheName:=RBCACHE_STATIONS_PREFIX+AVRMACsArray[AAVRConfigIdx]+'-'+Ord(ARBAllCategoryType).ToString+'-'+Abs(AName.ToLower.GetHashCode).ToString;
  if RBCacheType in [catFile,catMemStr] then
    Result:=GetRBCache(ARBStations,LCacheName,AAVRConfigIdx,AStart,AHowMany);
  if (RBCacheType=catNone) or (Result=0) then
    begin
      with AVRConfigArray[AAVRConfigIdx].RBSort do
        LSortingPath:='?'+API_ORDER_PATH+'='+HTTPEncode(Order)+'&'+API_REVERSE_PATH+'='+Reverse.ToString(TUseBoolStrs.True).ToLower;

      case ARBAllCategoryType of
        rbctGenre: LURL:=API_STATIONS_PATH+API_BYTAGEXACT_PATH+AName+LSortingPath;
        rbctCountry: LURL:=API_STATIONS_PATH+API_BYCOUNTRYEXACT_PATH+AName+LSortingPath;
        rbctLanguage: LURL:=API_STATIONS_PATH+API_BYLANGUAGEEXACT_PATH+AName+LSortingPath;
        rbctPopular: LURL:=API_STATIONS_PATH+API_POPULAR_FILTER_PATH+IntToStr(RBPopularAndSearchStationsLimit);
        rbctSearch: LURL:=API_STATIONS_PATH+'/'+API_SEARCH_PATH+API_SEARCH_FILTER_PATH+IntToStr(RBPopularAndSearchStationsLimit)+'&'+API_ATTR_NAME+'='+AName;
      end;

      try
        LJSONArray:=TJSONArray(RadiobrowserAPIRequestJSON(LURL+IfThen(HIDE_BROKEN_STATIONS,'&'+API_HIDEBROKEN_FILTER_PATH,'')));
        try
          if LJSONArray.Count>0 then
            begin
              if RBCacheType in [catFile,catMemStr] then
                LRBStations:=TRBStations.Create;
              try
                while LItemIdx<LJSONArray.Count do
                  begin
                    if RBStationsAVRFilter(TJSONObject(LJSONArray[LItemIdx]),ARBAllCategoryType,AAVRConfigIdx) then
                      begin
                        with TJSONObject(LJSONArray[LItemIdx]) do
                          begin
                            if RBCacheType in [catFile,catMemStr] then
                              begin
                                LRBStation:=TRBStation.Create;
                                SetRBStation(LRBStation,TJSONObject(LJSONArray[LItemIdx]));
                                LRBStations.AddObject(LRBStation.RBSID,LRBStation);
                              end;
                            if (LItemsFiltered>=AStart) and ((LItemsAdded<AHowMany) or (AHowMany<=0)) then
                              begin
                                LRBStation:=TRBStation.Create;
                                SetRBStation(LRBStation,TJSONObject(LJSONArray[LItemIdx]));
                                ARBStations.AddObject(LRBStation.RBSID,LRBStation);
                                LItemsAdded:=LItemsAdded+1;
                              end;
                          end;
                        LItemsFiltered:=LItemsFiltered+1;
                      end;
                    LItemIdx:=LItemIdx+1;
                  end;
                Result:=LItemsFiltered;

                if RBCacheType in [catFile,catMemStr] then
                  begin
                    if (LRBStations.Count=0) and (ARBAllCategoryType in [rbctGenre..rbctLanguage]) then
                      RemoveEmptyCategory(ARBAllCategoryType,RBCACHE_CATEGORIES_PREFIX+AVRMACsArray[AAVRConfigIdx]+'-'+Ord(ARBAllCategoryType).ToString,HTTPDecode(AName),AAVRConfigIdx)
                    else
                      SetRBCache(LRBStations,LCacheName,AAVRConfigIdx);
                  end;
              finally
                if RBCacheType in [catFile,catMemStr] then
                  LRBStations.Free;
              end;
            end
          else
            if RBCacheType in [catFile,catMemStr] then
              RemoveEmptyCategory(ARBAllCategoryType,RBCACHE_CATEGORIES_PREFIX+AVRMACsArray[AAVRConfigIdx]+'-'+Ord(ARBAllCategoryType).ToString,HTTPDecode(AName),AAVRConfigIdx);
        finally
          if Assigned(LJSONArray) then LJSONArray.Free;
        end;
      except
        on E: Exception do
          begin
            Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_ERROR,' ('+E.Message+')']));
            if Assigned(LJSONArray) then LJSONArray.Free;
          end;
      end;
    end;
end;

function GetRootItems(var ARootCategoryTypesCount: TRBRootCategoryTypesCount; AAVRConfigIdx: integer): integer;
begin
  if RBCacheType in [catDB, catMemDB, catPermMemDB] then
    Result:=DBRBGetRootItems(ARootCategoryTypesCount,AAVRConfigIdx)
  else
    begin
      ARootCategoryTypesCount[rbctGenre]:=GetCategoryItemsCount(rbctGenre,AAVRConfigIdx);
      ARootCategoryTypesCount[rbctCountry]:=GetCategoryItemsCount(rbctCountry,AAVRConfigIdx);
      ARootCategoryTypesCount[rbctLanguage]:=GetCategoryItemsCount(rbctLanguage,AAVRConfigIdx);
      ARootCategoryTypesCount[rbctPopular]:=RBPopularAndSearchStationsLimit;
      Result:=1;
    end;
end;

function GetCategoryItemsCount(ARBCategoryType: TRBCategoryTypes; AAVRConfigIdx: integer): integer;
var
  LJSONArray: TJSONArray = nil;
  LItemsFiltered: integer = 0;
  LItemIdx: integer = 0;
  LRBCategory: TRBCategory;
  LRBCategories: TRBCategories;
  LCacheName: string;
begin
  Result:=0;
  LRBCategories:=TRBCategories.Create;
  try
    if RBCacheType in [catDB, catMemDB, catPermMemDB] then
      Result:=DBRBGetCategoryItems(LRBCategories,ARBCategoryType,AAVRConfigIdx,0,1)
    else
      begin
        LCacheName:=RBCACHE_CATEGORIES_PREFIX+AVRMACsArray[AAVRConfigIdx]+'-'+Ord(ARBCategoryType).ToString;
        if RBCacheType in [catFile,catMemStr] then
          Result:=GetRBCache(LRBCategories,LCacheName,AAVRConfigIdx,0,1);
        if (RBCacheType=catNone) or (Result=0) then
          begin
            try
              LJSONArray:=TJSONArray(RadiobrowserAPIRequestJSON(RB_CATEGORIES_PATH[ARBCategoryType]+IfThen(HIDE_BROKEN_STATIONS,'?'+API_HIDEBROKEN_FILTER_PATH,'')));
              try
                if LJSONArray.Count>0 then
                  begin
                    while LItemIdx<LJSONArray.Count do
                      begin
                        if RBCategoriesAVRFilter(TJSONObject(LJSONArray[LItemIdx]),ARBCategoryType,AAVRConfigIdx) then
                          begin
                            if RBCacheType in [catFile,catMemStr] then
                              begin
                                with TJSONObject(LJSONArray[LItemIdx]) do
                                  begin
                                    LRBCategory:=TRBCategory.Create;
                                    SetRBCategory(LRBCategory,TJSONObject(LJSONArray[LItemIdx]));
                                    LRBCategories.AddObject(LRBCategory.RBCName,LRBCategory);
                                  end;
                              end;
                            LItemsFiltered:=LItemsFiltered+1;
                          end;
                        LItemIdx:=LItemIdx+1;
                      end;
                    Result:=LItemsFiltered;
                    if (RBCacheType in [catFile,catMemStr]) and (LRBCategories.Count>0) then
                      SetRBCache(LRBCategories,LCacheName,AAVRConfigIdx);
                  end;
              finally
                if Assigned(LJSONArray) then LJSONArray.Free;
              end;
            except
              on E: Exception do
                begin
                  Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_ERROR,' ('+E.Message+')']));
                  if Assigned(LJSONArray) then LJSONArray.Free;
                end;
            end;
          end;
      end;
  finally
    LRBCategories.Free;
  end;
end;

function GetCategoryItems(var ARBCategories: TRBCategories; ARBCategoryType: TRBCategoryTypes; AReq: TRequest): integer;
var
  LStart: integer = 0;
  LHowMany: integer = 0;
begin
  GetRange(LStart,LHowMany,AReq.QueryFields);
  if RBCacheType in [catDB, catMemDB, catPermMemDB] then
    Result:=DBRBGetCategoryItems(ARBCategories,ARBCategoryType,GetAVRConfigIdx(AReq),LStart,LHowMany)
  else
    Result:=GetCategoryItems(ARBCategories,ARBCategoryType,GetAVRConfigIdx(AReq),LStart,LHowMany);
end;

function GetCategoryItems(var ARBCategories: TRBCategories; ARBCategoryType: TRBCategoryTypes; AAVRConfigIdx, AStart, AHowMany: integer): integer;
var
  LJSONArray: TJSONArray = nil;
  LItemsFiltered: integer = 0;
  LItemsAdded: integer = 0;
  LItemIdx: integer = 0;
  LRBCategory: TRBCategory;
  LRBCategories: TRBCategories;
  LCacheName: string;
begin
  Result:=0;
  LCacheName:=RBCACHE_CATEGORIES_PREFIX+AVRMACsArray[AAVRConfigIdx]+'-'+Ord(ARBCategoryType).ToString;
  if RBCacheType in [catFile,catMemStr] then
    Result:=GetRBCache(ARBCategories,LCacheName,AAVRConfigIdx,AStart,AHowMany);
  if (RBCacheType=catNone) or (Result=0) then
    begin
      try
        LJSONArray:=TJSONArray(RadiobrowserAPIRequestJSON(RB_CATEGORIES_PATH[ARBCategoryType]+IfThen(HIDE_BROKEN_STATIONS,'?'+API_HIDEBROKEN_FILTER_PATH,'')));
        try
          if LJSONArray.Count>0 then
            begin
              if RBCacheType in [catFile,catMemStr] then
                LRBCategories:=TRBCategories.Create;
              try
                while LItemIdx<LJSONArray.Count do
                  begin
                    if RBCategoriesAVRFilter(TJSONObject(LJSONArray[LItemIdx]),ARBCategoryType,AAVRConfigIdx) then
                      begin
                        with TJSONObject(LJSONArray[LItemIdx]) do
                          begin
                            if RBCacheType in [catFile,catMemStr] then
                              begin
                                LRBCategory:=TRBCategory.Create;
                                SetRBCategory(LRBCategory,TJSONObject(LJSONArray[LItemIdx]));
                                LRBCategories.AddObject(LRBCategory.RBCName,LRBCategory);
                              end;
                            if (LItemsFiltered>=AStart) and ((LItemsAdded<AHowMany) or (AHowMany<=0)) then
                              begin
                                LRBCategory:=TRBCategory.Create;
                                SetRBCategory(LRBCategory,TJSONObject(LJSONArray[LItemIdx]));
                                ARBCategories.AddObject(LRBCategory.RBCName,LRBCategory);
                                LItemsAdded:=LItemsAdded+1;
                              end;
                          end;
                        LItemsFiltered:=LItemsFiltered+1;
                      end;
                    LItemIdx:=LItemIdx+1;
                  end;
                Result:=LItemsFiltered;
                if (RBCacheType in [catFile,catMemStr]) and (LRBCategories.Count>0) then
                  SetRBCache(LRBCategories,LCacheName,AAVRConfigIdx);
              finally
                if RBCacheType in [catFile,catMemStr] then
                  LRBCategories.Free;
              end;
            end;
        finally
          if Assigned(LJSONArray) then LJSONArray.Free;
        end;
      except
        on E: Exception do
          begin
            Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_ERROR,' ('+E.Message+')']));
            if Assigned(LJSONArray) then LJSONArray.Free;
          end;
      end;
    end;
end;

function RBCategoriesAVRFilter(AJSONObject: TJSONObject; ARBCategoryType: TRBCategoryTypes; AAVRConfigIdx: integer): boolean;
var
  LAttrName: string;
begin
  Result:=True;
  with AJSONObject, AVRConfigArray[AAVRConfigIdx], AVRConfigArray[AAVRConfigIdx].RBFilter do
    begin
      LAttrName:=Get(API_ATTR_NAME,'');
      if (LAttrName<>'') and (Get(API_ATTR_STATIONCOUNT,0)>=RBMinStationsPerCategory) then
        case ARBCategoryType of
          rbctGenre:
            if (Length(AllowedTags)>0) and (not HaveCommonElements(LAttrName,AllowedTags)) then Result:=False else
            if (Length(NotAllowedTags)>0) and (HaveCommonElements(LAttrName,NotAllowedTags)) then Result:=False;
          rbctCountry:
            if (Length(AllowedCountries)>0) and (not HaveCommonElements(LAttrName,AllowedCountries)) then Result:=False;
          rbctLanguage:
            if (Length(AllowedLanguages)>0) and (not HaveCommonElements(LAttrName,AllowedLanguages)) then Result:=False;
        end
      else
        Result:=False;
    end;
end;

function RBStationsAVRFilter(ARBStationJSONObject: TJSONObject; ARBAllCategoryType: TRBAllCategoryTypes; AAVRConfigIdx: integer): boolean;
var
  LBR: integer = 0;
begin
  Result:=True;
  with ARBStationJSONObject, AVRConfigArray[AAVRConfigIdx], AVRConfigArray[AAVRConfigIdx].RBFilter do
    begin
      case ARBAllCategoryType of
        rbctGenre:
          if (Length(AllowedCountries)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_COUNTRY)='',AVR_FILTER_EMPTY,Get(API_ATTR_COUNTRY)),AllowedCountries)) then Result:=False else
          if (Length(AllowedLanguages)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_LANGUAGE)='',AVR_FILTER_EMPTY,Get(API_ATTR_LANGUAGE)),AllowedLanguages)) then Result:=False;
        rbctCountry:
          if (Length(AllowedTags)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_TAGS)='',AVR_FILTER_EMPTY,Get(API_ATTR_TAGS)),AllowedTags)) then Result:=False else
          if (Length(NotAllowedTags)>0) and (HaveCommonElements(IfThen(Get(API_ATTR_TAGS)='',AVR_FILTER_EMPTY,Get(API_ATTR_TAGS)),NotAllowedTags)) then Result:=False else
          if (Length(AllowedLanguages)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_LANGUAGE)='',AVR_FILTER_EMPTY,Get(API_ATTR_LANGUAGE)),AllowedLanguages)) then Result:=False;
        rbctLanguage:
          if (Length(AllowedTags)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_TAGS)='',AVR_FILTER_EMPTY,Get(API_ATTR_TAGS)),AllowedTags)) then Result:=False else
          if (Length(NotAllowedTags)>0) and (HaveCommonElements(IfThen(Get(API_ATTR_TAGS)='',AVR_FILTER_EMPTY,Get(API_ATTR_TAGS)),NotAllowedTags)) then Result:=False else
          if (Length(AllowedCountries)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_COUNTRY)='',AVR_FILTER_EMPTY,Get(API_ATTR_COUNTRY)),AllowedCountries)) then Result:=False;
        rbctPopular:
          if (Length(AllowedTags)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_TAGS)='',AVR_FILTER_EMPTY,Get(API_ATTR_TAGS)),AllowedTags)) then Result:=False else
          if (Length(NotAllowedTags)>0) and (HaveCommonElements(IfThen(Get(API_ATTR_TAGS)='',AVR_FILTER_EMPTY,Get(API_ATTR_TAGS)),NotAllowedTags)) then Result:=False else
          if (Length(AllowedCountries)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_COUNTRY)='',AVR_FILTER_EMPTY,Get(API_ATTR_COUNTRY)),AllowedCountries)) then Result:=False else
          if (Length(AllowedLanguages)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_LANGUAGE)='',AVR_FILTER_EMPTY,Get(API_ATTR_LANGUAGE)),AllowedLanguages)) then Result:=False;
      end;

      if Result then
        begin
          if (Length(AllowedCodecs)>0) and (not HaveCommonElements(IfThen(Get(API_ATTR_CODEC)='',AVR_FILTER_EMPTY,Get(API_ATTR_CODEC)),AllowedCodecs)) then Result:=False else
          if (Length(NotAllowedCodecs)>0) and (HaveCommonElements(IfThen(Get(API_ATTR_CODEC)='',AVR_FILTER_EMPTY,Get(API_ATTR_CODEC)),NotAllowedCodecs)) then Result:=False else
          if ((Protocol=AVR_PROTOCOL_HTTP) or (Protocol=AVR_PROTOCOL_HTTPS)) and (Pos(Protocol+'://',Get(API_ATTR_URL))=0) then Result:=False else
          if (BitrateMax>0) and (TryStrToInt(IfThen(Get(API_ATTR_BITRATE)='','0',Get(API_ATTR_CODEC)),LBR)) and (LBR>BitrateMax) then Result:=False else
          if (Length(NotAllowedInName)>0) and (ContainsIn(Get(API_ATTR_URL),NotAllowedInName)) then Result:=False else
          if (Length(NotAllowedInURL)>0) and (ContainsIn(Get(API_ATTR_URL),NotAllowedInURL)) then Result:=False;
        end;
    end;
end;

function GetRBCache(var ARBObjectsList: TRBObjectsList; ACacheName: string; AAVRConfigIdx, AStart, AHowMany: integer): integer;
var
  LCacheIdx: integer;
  LCacheFileName: string;
begin
  Result:=0;
  LCacheIdx:=RBCache.IndexOf(ACacheName);
  if LCacheIdx>=0 then
    begin
      case RBCacheType of
           catFile: begin
                      LCacheFileName:=CachePath+DirectorySeparator+ACacheName+'-'+AVRConfigArray[AAVRConfigIdx].CRC32.ToString+CACHE_EXT;
                      if FileExists(LCacheFileName) then
                        begin
                          if (RBCacheTTL>0) and (FileDateToDateTime(FileAge(LCacheFileName))+(RBCacheTTL/24)<Now) then
                            begin
                              Logging(ltInfo, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_CACHE,MSG_RBCACHE_TTLEXPRELOAD,ACacheName]));
                              Exit;
                            end;
                          Result:=LoadRBObjects(ARBObjectsList,LCacheFileName,AStart,AHowMany);
                        end
                      else
                        Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,'no file',MSG_CACHE,ACacheName]));
                   end;
        catMemStr: with TRBCacheObject(RBCache.Objects[LCacheIdx]) do
                     begin
                       if (CRBEOL>0) and (CRBEOL<Now) then
                         begin
                           Logging(ltInfo, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_MEMSTRTYPE,MSG_CACHE,MSG_RBCACHE_TTLEXPRELOAD,ACacheName]));
                           Exit;
                         end;
                       Result:=LoadRBObjects(ARBObjectsList,CRBRecords,AStart,AHowMany);
                     end;
      end;
      if Result>0 then
        Logging(ltDebug, string.Join(' ',[MSG_CACHE,MSG_LOADED,ACacheName]))
      else
        begin
          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_CACHE,MSG_EMPTY,ACacheName]));
          RBCache.Delete(LCacheIdx);
        end;
    end;
end;

function SetRBCache(ARBObjectsList: TRBObjectsList; ACacheName: string; AAVRConfigIdx: integer):boolean;
var
  LCacheIdx: integer;
  LCacheFileName: string;
  LRBCacheObject: TRBCacheObject;
begin
  Result:=False;
  if ARBObjectsList.Count>0 then
    begin
      LCacheIdx:=RBCache.IndexOf(ACacheName);
      case RBCacheType of
         catFile: begin
                    LCacheFileName:=CachePath+DirectorySeparator+ACacheName+'-'+AVRConfigArray[AAVRConfigIdx].CRC32.ToString+CACHE_EXT;
                    Result:=SaveRBObjects(ARBObjectsList,LCacheFileName);
                    if Result then
                      begin
                        if LCacheIdx<0 then
                          RBCache.Add(ACacheName);
                      end
                    else
                      if FileExists(LCacheFileName) then
                        Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_CACHE,MSG_SAVE,MSG_ERROR,ACacheName]));
                  end;
       catMemStr: begin
                    LRBCacheObject:=TRBCacheObject.Create;
                    try
                      Result:=SaveRBObjects(ARBObjectsList,LRBCacheObject.CRBRecords);
                      if Result then
                        begin
                          if RBCacheTTL<=0 then
                            LRBCacheObject.CRBEOL:=0
                          else
                            LRBCacheObject.CRBEOL:=Now+RBCacheTTL/24;
                          if LCacheIdx<0 then
                            RBCache.AddObject(ACacheName,LRBCacheObject)
                          else
                            begin
                              if Assigned(TRBCacheObject(RBCache.Objects[LCacheIdx])) then
                                TRBCacheObject(RBCache.Objects[LCacheIdx]).Free;
                              RBCache.Objects[LCacheIdx]:=LRBCacheObject;
                            end;
                        end
                      else
                        LRBCacheObject.Free;
                    except
                      on E: Exception do
                        begin
                          Result:=False;
                          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_MEMSTRTYPE,MSG_CACHE,MSG_ERROR,ACacheName,' ('+E.Message+')']));
                        end;
                    end;
                  end;
      end;
      if Result then
        Logging(ltDebug, string.Join(' ',[MSG_CACHE,MSG_SAVED,ACacheName]))
      else
        if LCacheIdx>=0 then
          RBCache.Delete(LCacheIdx);
    end
  else
    Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_EMPTY,MSG_OBJECTS,ACacheName]));
end;

function GetRBStationCache(var ARBStation: TRBStation; AUUID: string; AAVRConfigIdx: integer):boolean;
var
  LCacheIdx: integer;
  LCacheName, LCacheFileName: string;
  LRBStations: TRBStations;
begin
  Result:=False;
  LCacheIdx:=0;
  if (RBCacheType<>catNone) and (RBCache.Count>0) then
    case RBCacheType of
      catFile: for LCacheName in RBCache do
                 if LCacheName.StartsWith(RBCACHE_STATIONS_PREFIX+AVRMACsArray[AAVRConfigIdx]+'-') then
                   begin
                     LCacheFileName:=CachePath+DirectorySeparator+LCacheName+'-'+AVRConfigArray[AAVRConfigIdx].CRC32.ToString+CACHE_EXT;
                     if (FileExists(LCacheFileName)) and ((RBCacheTTL<=0) or (FileDateToDateTime(FileAge(LCacheFileName))+(RBCacheTTL/24) > Now)) and (string(GetFileAsString(LCacheFileName)).Contains(AUUID)) then
                       begin
                         LRBStations:=TRBStations.Create;
                         try
                           try
                             if LoadRBObjects(LRBStations,LCacheFileName,0,0,AUUID)=1 then
                               begin
                                 ARBStation.Assign(TRBStation(LRBStations.Objects[0]));
                                 Logging(ltDebug, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_CACHE,MSG_LOADED,LCacheName]));
                                 Result:=True;
                                 Exit;
                               end;
                           except
                             on E: Exception do
                               Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_CACHE,MSG_ERROR,LCacheName,' ('+E.Message+')']));
                           end;
                         finally
                           LRBStations.Free;
                         end;
                       end;
                 end;
      catMemStr: for LCacheName in RBCache do
                   begin
                     if (LCacheName.StartsWith(RBCACHE_STATIONS_PREFIX+AVRMACsArray[AAVRConfigIdx]+'-')) and ((TRBCacheObject(RBCache.Objects[LCacheIdx]).CRBEOL <= 0) or (TRBCacheObject(RBCache.Objects[LCacheIdx]).CRBEOL > Now)) and (string(TRBCacheObject(RBCache.Objects[LCacheIdx]).CRBRecords).Contains(AUUID)) then
                       begin
                         LRBStations:=TRBStations.Create;
                         try
                           try
                             if LoadRBObjects(LRBStations,TRBCacheObject(RBCache.Objects[LCacheIdx]).CRBRecords,0,0,AUUID)=1 then
                               begin
                                 ARBStation.Assign(TRBStation(LRBStations.Objects[0]));
                                 Logging(ltDebug, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_MEMSTRTYPE,MSG_CACHE,MSG_LOADED,LCacheName]));
                                 Result:=True;
                                 Exit;
                               end;
                           except
                             on E: Exception do
                               Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_MEMSTRTYPE,MSG_CACHE,MSG_ERROR,LCacheName,' ('+E.Message+')']));
                           end;
                         finally
                           LRBStations.Free;
                         end;
                       end;
                     LCacheIdx:=LCacheIdx+1;
                   end;
    end;
end;

function RemoveEmptyCategory(ARBAllCategoryType: TRBAllCategoryTypes; ACacheName,AName: string; AAVRConfigIdx: integer):boolean;
var
  LCacheIdx: integer;
  LCacheFileName: string;
  LRBCategories: TRBCategories;
  LIdx: integer = 0;
begin
  Result:=False;
  LCacheIdx:=RBCache.IndexOf(ACacheName);
  if LCacheIdx>=0 then
    case RBCacheType of
       catFile: begin
                  LCacheFileName:=CachePath+DirectorySeparator+ACacheName+'-'+AVRConfigArray[AAVRConfigIdx].CRC32.ToString+CACHE_EXT;
                  if FileExists(LCacheFileName) then
                    begin
                      LRBCategories:=TRBCategories.Create;
                      try
                        try
                          if LoadRBObjects(LRBCategories,LCacheFileName,0,0)>0 then
                            begin
                              LIdx:=LRBCategories.IndexOf(AName);
                              if LIdx>0 then
                                begin
                                  LRBCategories.Delete(LIdx);
                                  Result:=SaveRBObjects(LRBCategories,LCacheFileName);
                                end;
                            end;
                        except
                          on E: Exception do
                            Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_CACHE,MSG_ERROR,ACacheName,' ('+E.Message+')']));
                        end;
                      finally
                        LRBCategories.Free;
                      end;
                    end
                  else
                    Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,'no file',MSG_CACHE,ACacheName]));
                end;
     catMemStr: with TRBCacheObject(RBCache.Objects[LCacheIdx]) do
                  begin
                    LRBCategories:=TRBCategories.Create;
                    try
                      try
                        if LoadRBObjects(LRBCategories,CRBRecords,0,0)>0 then
                          begin
                            LIdx:=LRBCategories.IndexOf(AName);
                            if LIdx>0 then
                              begin
                                LRBCategories.Delete(LIdx);
                                Result:=SaveRBObjects(LRBCategories,CRBRecords);
                              end;
                          end;
                      except
                        on E: Exception do
                          Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_CACHE,MSG_ERROR,ACacheName,' ('+E.Message+')']));
                      end;
                    finally
                      LRBCategories.Free;
                    end;
                  end;
    end;
end;

function LoadRBObjects(ARBObjectsList: TRBObjectsList; AFileName: string; AStart, AHowMany: integer; AObjectName: string = ''): integer;
var
  LCacheStream: TRawByteStringStream;
begin
  Result:=0;
  LCacheStream:=TRawByteStringStream.Create;
  try
    try
      LCacheStream.LoadFromFile(AFileName);
      if LCacheStream.Size>0 then
        Result:=DeSerializeRBObjects(ARBObjectsList,LCacheStream,AStart,AHowMany,AObjectName)
      else
        Logging(ltDebug, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_CACHE,MSG_EMPTY,MSG_STREAM]));
    except
      on E: Exception do
        Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_STREAM,MSG_CACHE,MSG_ERROR,' ('+E.Message+')']));
    end;
  finally
    LCacheStream.Free;
  end;
end;

function LoadRBObjects(ARBObjectsList: TRBObjectsList; ACRBObjects: RawByteString; AStart, AHowMany: integer; AObjectName: string = ''): integer;
var
  LCacheStream: TRawByteStringStream;
begin
  Result:=0;
  LCacheStream:=TRawByteStringStream.Create;
  try
    try
      LCacheStream.WriteString(ACRBObjects);
      if LCacheStream.Size>0 then
        Result:=DeSerializeRBObjects(ARBObjectsList,LCacheStream,AStart,AHowMany,AObjectName)
      else
        Logging(ltDebug, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_MEMSTRTYPE,MSG_CACHE,MSG_EMPTY,MSG_STREAM]));
    except
      on E: Exception do
        Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_MEMSTRTYPE,MSG_STREAM,MSG_CACHE,MSG_ERROR,' ('+E.Message+')']));
    end;
  finally
    LCacheStream.Free;
  end;
end;

function SaveRBObjects(ARBObjectsList: TRBObjectsList; AFileName: string): boolean;
var
  LCacheStream: TRawByteStringStream;
begin
  Result:=False;
  LCacheStream:=TRawByteStringStream.Create;
  try
    if SerializeRBObjects(ARBObjectsList,LCacheStream) then
      begin
        LCacheStream.Position:=0;
        if LCacheStream.Size>0 then
          begin
            try
              LCacheStream.SaveToFile(AFileName);
              Result:=True;
            except
              on E: Exception do
                Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_STREAM,MSG_CACHE,MSG_ERROR,' ('+E.Message+')']));
            end;
          end
        else
          Logging(ltDebug, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_FILETYPE,MSG_CACHE,MSG_EMPTY,MSG_STREAM]));
      end;
  finally
    LCacheStream.Free;
  end;
end;

function SaveRBObjects(ARBObjectsList: TRBObjectsList; var ACRBObjects: RawByteString): boolean;
var
  LCacheStream: TRawByteStringStream;
begin
  Result:=False;
  LCacheStream:=TRawByteStringStream.Create;
  try
    if SerializeRBObjects(ARBObjectsList,LCacheStream) then
      begin
        LCacheStream.Position:=0;
        if LCacheStream.Size>0 then
          begin
            try
              ACRBObjects:=LCacheStream.ReadString(LCacheStream.Size);
              Result:=True;
            except
              on E: Exception do
                Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_MEMSTRTYPE,MSG_STREAM,MSG_CACHE,MSG_ERROR,' ('+E.Message+')']));
            end;
          end
        else
          Logging(ltDebug, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_RBCACHE_MEMSTRTYPE,MSG_CACHE,MSG_EMPTY,MSG_STREAM]));
      end;
  finally
    LCacheStream.Free;
  end;
end;

function SerializeRBObjects(ARBObjectsList: TRBObjectsList; var ACacheStream: TRawByteStringStream): boolean;
var
  LListLen, LListIdx: LongWord;

  procedure SetStrValue(AStrValue: string);
  var
    LLen: Word;
  begin
    LLen:=AStrValue.Length;
    ACacheStream.Write(LLen,SizeOf(Word));
    if LLen>0 then
      ACacheStream.WriteString(AStrValue);
  end;

begin
  Result:=False;
  try
    ACacheStream.WriteString(IfThen(ARBObjectsList.Objects[0] is TRBStation,'S','C'));
    LListLen:=ARBObjectsList.Count;
    ACacheStream.Write(LListLen,SizeOf(LongWord));
    if ARBObjectsList.Objects[0] is TRBStation then
      for LListIdx:=0 to LListLen-1 do
        with TRBStation(ARBObjectsList.Objects[LListIdx]) do
          begin
            SetStrValue(RBSID);
            SetStrValue(RBSName);
            SetStrValue(RBSHomePageURL);
            SetStrValue(RBSURL);
            SetStrValue(RBSIcon);
            SetStrValue(RBSTags);
            SetStrValue(RBSCountry);
            SetStrValue(RBSCountryCode);
            SetStrValue(RBSLanguage);
            SetStrValue(RBSVotes);
            SetStrValue(RBSCodec);
            SetStrValue(RBSBitrate);
          end
    else
      for LListIdx:=0 to LListLen-1 do
        with TRBCategory(ARBObjectsList.Objects[LListIdx]) do
          begin
            SetStrValue(RBCName);
            ACacheStream.Write(RBCMaxCount,SizeOf(integer));
          end;
    if ACacheStream.Size>0 then
      Result:=True;
  except
    on E: Exception do
      Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_CACHE,MSG_ERROR,' ('+E.Message+')']));
  end;
end;

function DeSerializeRBObjects(ARBObjectsList: TRBObjectsList; var ACacheStream: TRawByteStringStream; AStart, AHowMany: integer; AObjectName: string = ''): integer;
var
  LListIdx: LongWord = 0;
  LIdx: LongWord;
  LCacheType: AnsiChar;
  LRBStation: TRBStation;
  LRBCategory: TRBCategory;
  LObjectFound: boolean = False;

  procedure GetStrValue(var AStrValue: string);
  var
    LLen: Word;
  begin
    ACacheStream.Read(LLen,SizeOf(Word));
    if LLen>0 then
      AStrValue:=ACacheStream.ReadString(LLen);
  end;

  procedure GetIntValue(var AIntValue: integer);
  begin
    ACacheStream.Read(AIntValue,SizeOf(integer));
  end;

  procedure StrDummyRead;
  var
    LLen: Word;
    LDummyValue: string;
  begin
    ACacheStream.Read(LLen,SizeOf(Word));
    if LLen>0 then
      LDummyValue:=ACacheStream.ReadString(LLen);
  end;

  procedure IntDummyRead;
  var
    LDummyValue: integer;
  begin
    ACacheStream.Read(LDummyValue,SizeOf(integer));
  end;

begin
  Result:=0;
  try
    ACacheStream.Position:=0;
    ACacheStream.Read(LCacheType,SizeOf(Byte));
    ACacheStream.Read(Result,SizeOf(LongWord));
    if (AStart=0) and (AHowMany<=0) then
      AHowMany:=Result
    else
      if AStart+AHowMany>Result then
        AHowMany:=Result-AStart;
    case LCacheType of
      'S' : while (LListIdx<AStart+AHowMany) and (not LObjectFound) do
              begin
                if LListIdx>=AStart then
                  begin
                    LRBStation:=TRBStation.Create;
                    with LRBStation do
                      begin
                        GetStrValue(RBSID);
                        GetStrValue(RBSName);
                        GetStrValue(RBSHomePageURL);
                        GetStrValue(RBSURL);
                        GetStrValue(RBSIcon);
                        GetStrValue(RBSTags);
                        GetStrValue(RBSCountry);
                        GetStrValue(RBSCountryCode);
                        GetStrValue(RBSLanguage);
                        GetStrValue(RBSVotes);
                        GetStrValue(RBSCodec);
                        GetStrValue(RBSBitrate);
                      end;
                    if (AObjectName='') or (AObjectName=LRBStation.RBSID) then
                      begin
                        ARBObjectsList.AddObject(LRBStation.RBSID,LRBStation);
                        if AObjectName<>'' then
                          begin
                            LObjectFound:=True;
                            Result:=1;
                          end;
                      end
                    else
                      FreeAndNil(LRBStation);
                  end
                else
                  for LIdx:=1 to 12 do StrDummyRead;
                LListIdx:=LListIdx+1;
              end;
      'C' : while (LListIdx<AStart+AHowMany) and (not LObjectFound) do
              begin
                if LListIdx>=AStart then
                  begin
                    LRBCategory:=TRBCategory.Create;
                    with LRBCategory do
                      begin
                        GetStrValue(RBCName);
                        GetIntValue(RBCMaxCount);
                      end;
                    if (AObjectName='') or (AObjectName=LRBCategory.RBCName) then
                      begin
                        ARBObjectsList.AddObject(LRBCategory.RBCName,LRBCategory);
                        if AObjectName<>'' then
                          begin
                            LObjectFound:=True;
                            Result:=1;
                          end;
                      end
                    else
                      FreeAndNil(LRBCategory);
                  end
                else
                  begin
                    StrDummyRead;
                    IntDummyRead;
                  end;
                LListIdx:=LListIdx+1;
              end;
    end;
    if (AObjectName<>'') and (not LObjectFound) then
      Result:=0;
  except
    on E: Exception do
      Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_CACHE,MSG_ERROR,' ('+E.Message+')']));
  end;
end;

procedure LoadRBCacheFilesInfo(AAVRConfigIdx: integer);
var
  LCacheFile: string;
  LCacheFiles: TStringList;
begin
  LCacheFiles:=FindAllFiles(CachePath,'RB*-'+AVRMACsArray[AAVRConfigIdx]+'-*-'+AVRConfigArray[AAVRConfigIdx].CRC32.ToString+CACHE_EXT,False);
  try
    if LCacheFiles.Count>0 then
      begin
        Logging(ltInfo, string.Join(' ',[MSG_LOADING,AVRMACsArray[AAVRConfigIdx],MSG_CACHE]) );
        for LCacheFile in LCacheFiles do
          if (RBCacheTTL<=0) or (FileDateToDateTime(FileAge(LCacheFile))+(RBCacheTTL/24)>Now) then
            RBCache.Add(StringReplace(ExtractFileName(LCacheFile),'-'+AVRConfigArray[AAVRConfigIdx].CRC32.ToString+CACHE_EXT,'',[]))
          else
            DeleteFile(LCacheFile);
      end;
  finally
    LCacheFiles.free;
  end;

end;

procedure RemoveOldRBCacheFiles;
var
  LCacheFileRemoved: boolean = False;
  LCacheFile: string;
  LCacheFiles: TStringList;
begin
  LCacheFiles:=FindAllFiles(CachePath,'RB*-*'+CACHE_EXT,False);
  try
    if (RBCacheTTL>0) and (LCacheFiles.Count>0) then
      begin
        for LCacheFile in LCacheFiles do
          if FileDateToDateTime(FileAge(LCacheFile))+(RBCacheTTL/24)<Now then
            begin
              DeleteFile(LCacheFile);
              LCacheFileRemoved:=True
            end;
        if LCacheFileRemoved then
          Logging(ltDebug, string.Join(' ',['old',MSG_CACHE,MSG_REMOVED]));
      end;
  finally
    LCacheFiles.free;
  end;
end;

finalization
  if (RBCacheType<>catNone) and Assigned(RBCache) then
    RBCache.Free;

end.

