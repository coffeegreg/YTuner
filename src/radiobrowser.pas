unit radiobrowser;

// YTuner: Radio-browser.info API feature support unit.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  fpjson, jsonparser,
  fphttpclient, httpprotocol,
  RegExpr, StrUtils, DateUtils,
  common;

type
  TRBStation = record
                 RBSID, RBSName, RBSHomePageURL, RBSURL, RBSIcon, RBSTags, RBSCountry, RBSCountryCode, RBSLanguage, RBSVotes, RBSCodec, RBSBitrate : string;
               end;

  TRBStations = array of TRBStation;

  TRBCategoryItems = array of record
                       RBCName: string;
                       RBCCount: integer;
                     end;

  TRBAllCategoryTypes = (rbctGenre, rbctCountry, rbctLanguage, rtctPopular, rtctSearch);
  TRBCategoryTypes = rbctGenre..rbctLanguage;
  TRBCategories = array[TRBCategoryTypes] of TRBCategoryItems;

const
  RADIOBROWSER_PREFIX = 'RB';
  PATH_RADIOBROWSER = 'radiobrowser';
  API_SEARCH_PATH = 'search';
  API_STATIONS_PATH = 'stations';
  API_OFFSET_PATH = 'offset=';
  API_LIMIT_PATH = 'limit=';

  API_CATEGORY_FILTER_PATH = '?order=name&reverse=false';
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
  API_ATTR_FAVICON ='favicon';
  API_ATTR_TAGS = 'tags';
  API_ATTR_COUNTRY = 'country';
  API_ATTR_COUNTRYCODE = 'countrycode';
  API_ATTR_LANGUAGE = 'language';
  API_ATTR_VOTES = 'votes';
  API_ATTR_CODEC = 'codec';
  API_ATTR_BITRATE = 'bitrate';
  API_ATTR_STATIONCOUNT = 'stationcount';

  MSG_RADIOBROWSER = 'Radio-browser.info';
  RADIOBROWSER_UUIDS_FILE = 'rbuuids.txt';

  HIDE_BROKEN_STATIONS : boolean = True;
  RB_CATEGORIES_PATH : array[TRBCategoryTypes] of string = ('tags','countries','languages');

var
  RadioBrowserEnabled: boolean = True;
  RBStationsUUIDs: string = '';
  RBMinStationsPerCategory: integer = 3;
  RBPopularAndSearchStationsLimit: integer = 100;
  RBUUIDsCacheTTL: integer = -1;
  RBUUIDsCacheAutoRefresh: boolean = False;
  RBAPIURL: string = 'http://all.api.radio-browser.info';

function GetAPIURLRange(AElementNumber,AElementCount: integer): string;
function RadiobrowserAPIRequest(AURL: string): RawByteString;
function RadiobrowserAPIRequestJSON(AURL: string): TJSONData;
function GetCategoryItemsCount(ARBCategoryType: TRBCategoryTypes): integer;
function GetCategoryItems(ARBCategoryType: TRBCategoryTypes): TRBCategoryItems;
function GetStationsBy(ARBAllCategoryType: TRBAllCategoryTypes; AName: string): TRBStations;
function GetRBStationByID(AID: string): TRBStation;
function SetRBStation(ARBStationJSONObject: TJSONObject): TRBStation;
function LoadRBStationsUUIDs: boolean;
procedure GetRBStations;

implementation

function GetAPIURLRange(AElementNumber,AElementCount: integer): string;
begin
  Result:='?'+API_OFFSET_PATH+IntToStr(AElementNumber)+'&'+API_LIMIT_PATH+IntToStr(AElementCount);
end;

function RadiobrowserAPIRequest(AURL: string): RawByteString;
begin
  Logging(ltDebug,MSG_RADIOBROWSER+': GET '+AURL);
  with TFPHttpClient.Create(nil) do
  try
    try
      AddHeader(HTTP_HEADER_ACCEPT,HTTP_RESPONSE_CONTENT_TYPE[ctJSON]);
      AddHeader(HTTP_HEADER_USER_AGENT,YTUNER_USER_AGENT+'/'+APP_VERSION);
      Result:=Get(RBAPIURL+'/json/'+AURL);
    except
      Logging(ltError,MSG_RADIOBROWSER+': '+MSG_GETTING+MSG_ERROR+' '+AURL);
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

function SetRBStation(ARBStationJSONObject: TJSONObject): TRBStation;
begin
  with Result, ARBStationJSONObject do
    begin
      RBSID:=Get(API_ATTR_STATIONUUID);
      RBSName:=Get(API_ATTR_NAME);
      RBSHomePageURL:=Get(API_ATTR_URL);
      RBSURL:=Get(API_ATTR_URL);
      RBSIcon:=Get(API_ATTR_FAVICON);
      RBSTags:=Get(API_ATTR_TAGS);
      RBSCountry:=Get(API_ATTR_COUNTRY);
      RBSCountryCode:=Get(API_ATTR_COUNTRYCODE);
      RBSLanguage:=Get(API_ATTR_LANGUAGE);
      RBSVotes:=Get(API_ATTR_VOTES);
      RBSCodec:=Get(API_ATTR_CODEC);
      RBSBitrate:=Get(API_ATTR_BITRATE);
    end;
end;

function LoadRBStationsUUIDs: boolean;
var
  LFA: Longint;

  function LoadRBStationsFromFile:boolean;
  var
    F: Text;
  begin
    Result:=False;
    if FileExists(MyAppPath+RADIOBROWSER_UUIDS_FILE) then
      begin
        AssignFile(F,MyAppPath+RADIOBROWSER_UUIDS_FILE);
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
  if FileExists(MyAppPath+RADIOBROWSER_UUIDS_FILE) then
    try
      Logging(ltInfo, MSG_LOADING+'RB UUIDs cache file');
// Alternatively:
//      RBStationsUUIDs:=GetFileAsString(MyAppPath+RADIOBROWSER_UUIDS_FILE);
//      if RBStationsUUIDs.Length>0 then
      if LoadRBStationsFromFile and (RBStationsUUIDs.Length>0) then
        begin
          Logging(ltInfo, MSG_SUCCESSFULLY_LOADED+IfThen(RBStationsUUIDs.IsEmpty,'0',IntToStr(RBStationsUUIDs.CountChar(',')+1))+' RB UUIDs from cache file');
          LFA:=FileAge(MyAppPath+RADIOBROWSER_UUIDS_FILE);
          if (LFA<>-1) and ((RBUUIDsCacheTTL<0) or (HoursBetween(Now,FileDateToDateTime(LFA))<RBUUIDsCacheTTL)) then
            Result:=True
          else
            Logging(ltInfo, 'RB UUIDs cache file is older then '+IntToStr(RBUUIDsCacheTTL)+' hours. Refresing RB UUIDs list..');
        end;
    except
      Logging(ltInfo, MSG_ERROR_LOAD+'RB UUIDs cache file');
    end;
end;


procedure GetRBStations;
var
  LLoadError: boolean = False;
  LStations: TStrings;

  function SaveRBStationsToFile:boolean;
  var
    F: Longint;
  begin
    Result:=False;
    if FileExists(MyAppPath+RADIOBROWSER_UUIDS_FILE) then
      begin
        F:=FileOpen(MyAppPath+RADIOBROWSER_UUIDS_FILE,fmOpenReadWrite);
        if not FileTruncate(F,0) then
          if DeleteFile(MyAppPath+RADIOBROWSER_UUIDS_FILE) then
            F:=FileCreate(MyAppPath+RADIOBROWSER_UUIDS_FILE)
          else
            begin
              Logging(ltError, RADIOBROWSER_UUIDS_FILE+' access error');
              Exit;
            end
      end
    else
      F:=FileCreate(MyAppPath+RADIOBROWSER_UUIDS_FILE);
    if F<>-1 then
      begin
        FileWrite(F,RBStationsUUIDs[1],RBStationsUUIDs.Length);
        FileClose(F);
        Result:=True;
      end;
  end;

begin
  Logging(ltInfo, MSG_GETTING+MSG_RADIOBROWSER+' UUIDs..');
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

        Logging(ltInfo, MSG_SUCCESSFULLY_DOWNLOADED+IfThen(RBStationsUUIDs.IsEmpty,'0',IntToStr(RBStationsUUIDs.CountChar(',')+1))+' RB UUIDs');
        if SaveRBStationsToFile then
          Logging(ltInfo, MSG_SUCCESSFULLY_SAVED+IfThen(RBStationsUUIDs.IsEmpty,'0',IntToStr(RBStationsUUIDs.CountChar(',')+1))+' RB UUIDs to cache file');
      end;
    LStations.Free;
  end;
end;

function GetRBStationByID(AID: string): TRBStation;
var
  LMatches: SizeIntArray;
begin
  if FindMatchesBoyerMooreCaseInSensitive(RBStationsUUIDs,AID.Insert(8,'-')+'-',LMatches,False) then
    try
      Result:=SetRBStation(TJSONArray(RadiobrowserAPIRequestJSON(API_STATIONS_PATH+API_BYUUID_PATH+RBStationsUUIDs.Substring(LMatches[0]-1,36))).Objects[0]);
    except
      Result.RBSID:='';
    end
  else
    Result.RBSID:='';
end;

function GetStationsBy(ARBAllCategoryType: TRBAllCategoryTypes; AName: string): TRBStations;
var
  LURL: string;
  LJSONArray: TJSONArray;
  LJSONEnum: TJSONEnum;
begin
  case ARBAllCategoryType of
    rbctGenre: LURL:=API_STATIONS_PATH+API_BYTAGEXACT_PATH+AName+API_CATEGORY_FILTER_PATH;
    rbctCountry: LURL:=API_STATIONS_PATH+API_BYCOUNTRYEXACT_PATH+AName+API_CATEGORY_FILTER_PATH;
    rbctLanguage: LURL:=API_STATIONS_PATH+API_BYLANGUAGEEXACT_PATH+AName+API_CATEGORY_FILTER_PATH;
    rtctPopular: LURL:=API_STATIONS_PATH+API_POPULAR_FILTER_PATH+IntToStr(RBPopularAndSearchStationsLimit);
    rtctSearch: LURL:=API_STATIONS_PATH+'/'+API_SEARCH_PATH+API_SEARCH_FILTER_PATH+IntToStr(RBPopularAndSearchStationsLimit)+'&'+API_ATTR_NAME+'='+AName;
  end;
//  LJSONArray:=TJSONArray(RadiobrowserAPIRequestJSON(LURL+IfThen(HIDE_BROKEN_STATIONS,'&'+API_CHECK_FILTER_PATH,'')));
  LJSONArray:=TJSONArray(RadiobrowserAPIRequestJSON(LURL+IfThen(HIDE_BROKEN_STATIONS,'&'+API_HIDEBROKEN_FILTER_PATH,'')));
  SetLength(Result,LJSONArray.Count);
  for LJSONEnum in LJSONArray do
    Result[LJSONEnum.KeyNum]:=SetRBStation(TJSONObject(LJSONEnum.Value));
  LJSONArray.Free;
end;

function GetCategoryItemsCount(ARBCategoryType: TRBCategoryTypes): integer;
var
  LJSONArray: TJSONArray;
  LJSONEnum: TJSONEnum;
  i: integer = 0;
begin
  LJSONArray:=TJSONArray(RadiobrowserAPIRequestJSON(RB_CATEGORIES_PATH[ARBCategoryType]+IfThen(HIDE_BROKEN_STATIONS,'?'+API_HIDEBROKEN_FILTER_PATH,'')));
  for LJSONEnum in LJSONArray do
    if (TJSONObject(LJSONEnum.Value).Get(API_ATTR_NAME,'')<>'') and (TJSONObject(LJSONEnum.Value).Get(API_ATTR_STATIONCOUNT,0)>=RBMinStationsPerCategory) then
      i:=i+1;
  LJSONArray.Free;
  Result:=i;
end;

function GetCategoryItems(ARBCategoryType: TRBCategoryTypes): TRBCategoryItems;
var
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  LJSONEnum: TJSONEnum;
  i: integer = 0;
begin
  LJSONArray:=TJSONArray(RadiobrowserAPIRequestJSON(RB_CATEGORIES_PATH[ARBCategoryType]+IfThen(HIDE_BROKEN_STATIONS,'?'+API_HIDEBROKEN_FILTER_PATH,'')));
  SetLength(Result,LJSONArray.Count);
  for LJSONEnum in LJSONArray do
    begin
      LJSONObject:=TJSONObject(LJSONEnum.Value);
      if (LJSONObject.Get(API_ATTR_NAME,'')<>'') and (LJSONObject.Get(API_ATTR_STATIONCOUNT,0)>=RBMinStationsPerCategory) then
        with Result[i] do
          begin
            RBCName:=LJSONObject.Get(API_ATTR_NAME);
            RBCCount:=LJSONObject.Get(API_ATTR_STATIONCOUNT);
            i:=i+1;
          end;
    end;
  LJSONArray.Free;
  SetLength(Result,i);
end;

end.

