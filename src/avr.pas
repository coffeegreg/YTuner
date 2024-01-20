unit avr;

// YTuner: AVR unit.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, StrUtils, HTTPDefs, common;

type
  TRBFilter = record
                AllowedTags,AllowedCountries,AllowedLanguages,AllowedCodecs: array of string;
                BitrateMax: integer;
                NotAllowedTags,NotAllowedCodecs,NotAllowedInName,NotAllowedInURL: array of string;
              end;
  TRBSort = record
                Order: string;
                Reverse: boolean;
              end;
  TAVRConfig = record
                 CRC32: Cardinal;
                 Protocol: string;
                 RBFilter: TRBFilter;
                 RBSort: TRBSort;
               end;
  TAVRConfigArray = array of TAVRConfig;

const
  AVR_INI_VERSION = '1.0.0';
  AVR_INI_PROTOCOL = 'Protocol';
  AVR_INI_SECTION_CONFIGURATION = 'Configuration';
  AVR_INI_INIVERSION = 'INIVersion';
  AVR_INI_SECTION_RADIOBROWSER_FILTERING = 'RadioBrowser Filtering';
  AVR_INI_ALLOWED_TAGS = 'AllowedTags';
  AVR_INI_NOTALLOWED_TAGS = 'NotAllowedTags';
  AVR_INI_ALLOWED_COUNTRIES = 'AllowedCountries';
  AVR_INI_ALLOWED_LANGUAGES = 'AllowedLanguages';
  AVR_INI_ALLOWED_CODECS = 'AllowedCodecs';
  AVR_INI_NOTALLOWED_CODECS = 'NotAllowedCodecs';
  AVR_INI_BITRATEMAX = 'BitrateMax';
  AVR_INI_NOTALLOWED_IN_NAME = 'NotAllowedInName';
  AVR_INI_NOTALLOWED_IN_URL = 'NotAllowedInURL';
  AVR_INI_SECTION_RADIOBROWSER_SORTING = 'RadioBrowser Sorting';
  AVR_INI_ORDER = 'Order';
  AVR_INI_REVERSE = 'Reverse';

  AVR_FILTER_EMPTY = '{empty}';

  AVR_AVR = 'avr';
  AVR_MAC = 'mac';
  AVR_PROTOCOL_HTTP = 'http';
  AVR_PROTOCOL_HTTPS = 'https';
  AVR_PROTOCOL_ALL_AS_HTTP = 'all-as-http';

var
  AVRMACsArray: array of string;
  AVRConfigArray: TAVRConfigArray;
  CommonAVRini: boolean = True;

function StripHttps(AURL: string; AReq: TRequest):string;
function GetAVRConfigIdx(AReq: TRequest):integer;
function ReadAVRINIConfiguration(AAVRMAC: string):integer;

implementation

uses radiobrowser, radiobrowserdb;

function StripHttps(AURL: string; AReq: TRequest):string;
var
  LAVRConfigIdx: integer = 0;
  LStripHttps: boolean = False;
begin
   if (not CommonAVRini) and (AReq.QueryFields.Values[AVR_MAC].Trim.Length>0) then
     begin
       LAVRConfigIdx:=IndexStr(AReq.QueryFields.Values[AVR_MAC].Trim,AVRMACsArray);
       if LAVRConfigIdx<0 then
         LAVRConfigIdx:=GetAVRConfigIdx(AReq);
     end;
   if (LAVRConfigIdx>=0) and (AVRConfigArray[LAVRConfigIdx].Protocol=AVR_PROTOCOL_ALL_AS_HTTP) then
     LStripHttps:=True;

   if LStripHttps then
     Result:=StringReplace(AURL,AVR_PROTOCOL_HTTPS+'://',AVR_PROTOCOL_HTTP+'://',[rfIgnoreCase])
   else
     Result:=AURL;
end;

function GetAVRConfigIdx(AReq: TRequest):integer;
var
  LAVRMAC: string;
begin
  Result:=0;
  if not CommonAVRini then
    begin
      LAVRMAC:=AReq.QueryFields.Values[AVR_MAC].Trim;
      if (LAVRMAC<>'') then
        begin
          Result:=IndexStr(LAVRMAC,AVRMACsArray);
          if Result<0 then
            begin
              Logging(ltInfo, 'New AVR connected ('+LAVRMAC+')');
              if not FileExists(ConfigPath+DirectorySeparator+LAVRMAC+'.ini') then
                Logging(ltInfo, 'Preparing new config ini file ('+LAVRMAC+'.ini)..');
              try
                Result:=ReadAVRINIConfiguration(LAVRMAC);
                if Result>0 then
                  case RBCacheType of
                    catFile: LoadRBCacheFilesInfo(Result);
                    catDB, catMemDB, catPermMemDB: DBRBCheckAVRView(Result);
                  end;
              finally
                if Result<0 then                               //INI error.
                  Result:=0;                                   //Default AVR config.
              end;
            end;
        end;
    end;
end;

function ReadAVRINIConfiguration(AAVRMAC: string):integer;
var
  LAVRINIFile: TIniFile;

  procedure RBFilteringReadStrings(var AFilterItem: TStringArray; const AIDENT: string);
  begin
    with LAVRINIFile do
      begin
        if not ValueExists(AVR_INI_SECTION_RADIOBROWSER_FILTERING,AIDENT) then
          WriteString(AVR_INI_SECTION_RADIOBROWSER_FILTERING,AIDENT,'');
        AFilterItem:=StripChars(ReadString(AVR_INI_SECTION_RADIOBROWSER_FILTERING,AIDENT,''),RB_FILTER_STRIP_CHARS).Split([';'],TStringSplitOptions.ExcludeEmpty);
      end;
  end;

begin
  Result:=Length(AVRMACsArray);
  SetLength(AVRMACsArray,Result+1);
  AVRMACsArray[Result]:=AAVRMAC;
  SetLength(AVRConfigArray,Result+1);
  LAVRINIFile:=TIniFile.Create(ConfigPath+DirectorySeparator+AAVRMAC+'.ini');
  with LAVRINIFile,AVRConfigArray[Result] do
    try
      if not ValueExists(AVR_INI_SECTION_CONFIGURATION,AVR_INI_INIVERSION) then
        WriteString(AVR_INI_SECTION_CONFIGURATION,AVR_INI_INIVERSION,AVR_INI_VERSION);
      if ReadString(AVR_INI_SECTION_CONFIGURATION,AVR_INI_INIVERSION,'')<>AVR_INI_VERSION then
        begin
          Logging(ltWarning, 'Your '+AAVRMAC+'.ini file is outdated! Some features may not work properly!');
          Logging(ltWarning, 'Your '+AAVRMAC+'.ini file will be updated with new options right now. Check https://github.com/coffeegreg/YTuner/cfg for the description of appropriate avr.ini file for your version of YTuner');
          WriteString(AVR_INI_SECTION_CONFIGURATION,AVR_INI_INIVERSION,AVR_INI_VERSION);
        end;

      if not ValueExists(AVR_INI_SECTION_CONFIGURATION,AVR_INI_PROTOCOL) then
        WriteString(AVR_INI_SECTION_CONFIGURATION,AVR_INI_PROTOCOL,AVR_PROTOCOL_ALL_AS_HTTP);
      Protocol:=ReadString(AVR_INI_SECTION_CONFIGURATION,AVR_INI_PROTOCOL,AVR_PROTOCOL_ALL_AS_HTTP).ToLower;

      with RBFilter do
        begin
          RBFilteringReadStrings(AllowedTags,AVR_INI_ALLOWED_TAGS);
          RBFilteringReadStrings(NotAllowedTags,AVR_INI_NOTALLOWED_TAGS);
          RBFilteringReadStrings(AllowedCountries,AVR_INI_ALLOWED_COUNTRIES);
          RBFilteringReadStrings(AllowedLanguages,AVR_INI_ALLOWED_LANGUAGES);
          RBFilteringReadStrings(AllowedCodecs,AVR_INI_ALLOWED_CODECS);
          RBFilteringReadStrings(NotAllowedCodecs,AVR_INI_NOTALLOWED_CODECS);
          RBFilteringReadStrings(NotAllowedInName,AVR_INI_NOTALLOWED_IN_NAME);
          RBFilteringReadStrings(NotAllowedInURL,AVR_INI_NOTALLOWED_IN_URL);
          if not ValueExists(AVR_INI_SECTION_RADIOBROWSER_FILTERING,AVR_INI_BITRATEMAX) then
            WriteString(AVR_INI_SECTION_RADIOBROWSER_FILTERING,AVR_INI_BITRATEMAX,'');
          BitrateMax:=ReadInteger(AVR_INI_SECTION_RADIOBROWSER_FILTERING,AVR_INI_BITRATEMAX,0);
        end;
      with RBSort do
        begin
          if not ValueExists(AVR_INI_SECTION_RADIOBROWSER_SORTING,AVR_INI_ORDER) then
            WriteString(AVR_INI_SECTION_RADIOBROWSER_SORTING,AVR_INI_ORDER,'name');
          Order:=ReadString(AVR_INI_SECTION_RADIOBROWSER_SORTING,AVR_INI_ORDER,'name');
          if not ValueExists(AVR_INI_SECTION_RADIOBROWSER_SORTING,AVR_INI_REVERSE) then
            WriteString(AVR_INI_SECTION_RADIOBROWSER_SORTING,AVR_INI_REVERSE,'0');
          Reverse:=ReadBool(AVR_INI_SECTION_RADIOBROWSER_SORTING,AVR_INI_REVERSE,False);
        end;
      CRC32:=CalcFileCRC32(ConfigPath+DirectorySeparator+AAVRMAC+'.ini');
    finally
      Free;
    end;
end;

end.

