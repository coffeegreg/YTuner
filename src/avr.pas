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
  TMainMenuItem = record
                    MIdentifier: string;
                    MLabel: string;
                  end;
  TTranslator = record
                  ReplaceUTF8Latin1Supplement: boolean;
                  ReplaceUTF8Latin1ExtendedA: boolean;
                  ReplaceUTF8Latin1ExtendedB: boolean;
                  Enabled: boolean;
                  AutoTranslate: boolean;
                  ToTranslate: string;
                  LanguageCode: string;
                  TranslatorItems: TStrings;
                  ToTranslateLocked: boolean;
                  TranslatorFileLocked: boolean;
                end;
  TAVRConfig = record
                 CRC32: Cardinal;
                 Protocol: string;
                 Translator: TTranslator;
                 MainMenuItems: array of TMainMenuItem;
                 RBFilter: TRBFilter;
                 RBSort: TRBSort;
               end;
  TAVRConfigArray = array of TAVRConfig;

const
  AVR_INI_VERSION = '1.0.2';
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
  AVR_INI_SECTION_MAINMENU = 'MainMenu Items';
  AVR_INI_MAINMENU_IDENTIFIERS = 'IdentifiersList';
  AVR_INI_MAINMENU_LABELS = 'LabelsList';
  AVR_INI_SECTION_TRANSLATOR = 'Translator';
  AVR_INI_ENABLE = 'Enable';
  AVR_INI_AUTOTRANSLATE = 'Auto';
  AVR_INI_LANGUAGECODE = 'LanguageCode';
  AVR_INI_REPLACEUTF8LATIN1SUPPLEMENT = 'ReplaceUTF8Latin1Supplement';
  AVR_INI_REPLACEUTF8LATIN1EXTENDEDA = 'ReplaceUTF8Latin1ExtendedA';
  AVR_INI_REPLACEUTF8LATIN1EXTENDEDB = 'ReplaceUTF8Latin1ExtendedB';

  AVR_FILTER_EMPTY = '{empty}';

  AVR_AVR = 'avr';
  AVR_MAC = 'mac';
  AVR_PROTOCOL_HTTP = 'http';
  AVR_PROTOCOL_HTTPS = 'https';
  AVR_PROTOCOL_ALL_AS_HTTP = 'all-as-http';
  AVR_MAINMENU_IDENTIFIER_BOOKMARKS = 'bookmarks';
  AVR_MAINMENU_IDENTIFIER_MYSTATIONS = 'mystations';
  AVR_MAINMENU_IDENTIFIER_RADIOBROWSER = 'radiobrowser';
  AVR_MAINMENU_IDENTIFIER_ABOUT = 'about';
  AVR_MAINMENU_IDENTIFIERS = AVR_MAINMENU_IDENTIFIER_ABOUT+';'+AVR_MAINMENU_IDENTIFIER_BOOKMARKS+';'+AVR_MAINMENU_IDENTIFIER_MYSTATIONS+';'+AVR_MAINMENU_IDENTIFIER_RADIOBROWSER;
  AVR_MAINMENU_LABELS = '*** YTuner ***;Bookmarks;My Stations;Radio Browser';

  MSG_AVR_MAINMENU_INCORRECT_VALUES = 'Incorrect values in avr.ini [MainMenu Items] group!';
  MSG_AVR_MAINMENU_RESTORING_VALUES = 'Restoring default values...';

var
  AVRMACsArray: array of string;
  AVRConfigArray: TAVRConfigArray;
  CommonAVRini: boolean = True;

function StripHttps(AURL: string; AReq: TRequest):string;
function GetAVRConfigIdx(AReq: TRequest):integer;
function ReadAVRINIConfiguration(AAVRMAC: string):integer;

implementation

uses radiobrowser, radiobrowserdb, translator;

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
  LStr1, LStr2: string;
  LIdx: integer;

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

      LStr1:=ReadString(AVR_INI_SECTION_MAINMENU,AVR_INI_MAINMENU_IDENTIFIERS,'');
      LStr2:=ReadString(AVR_INI_SECTION_MAINMENU,AVR_INI_MAINMENU_LABELS,'');
      if (LStr1.CountChar(';')<>LStr2.CountChar(';')) or (LStr1.Length=0) or (LStr2.Length=0) then
        begin
          Logging(ltWarning, MSG_AVR_MAINMENU_INCORRECT_VALUES+' '+MSG_AVR_MAINMENU_RESTORING_VALUES);
          WriteString(AVR_INI_SECTION_MAINMENU,AVR_INI_MAINMENU_IDENTIFIERS,AVR_MAINMENU_IDENTIFIERS);
          WriteString(AVR_INI_SECTION_MAINMENU,AVR_INI_MAINMENU_LABELS,AVR_MAINMENU_LABELS);
          LStr1:=AVR_MAINMENU_IDENTIFIERS;
          LStr2:=AVR_MAINMENU_LABELS;
        end;

      SetLength(MainMenuItems,LStr1.CountChar(';')+1);
      for LIdx:=0 to LStr1.CountChar(';') do
        begin
          MainMenuItems[LIdx].MIdentifier:=LStr1.Split([';'])[LIdx];
          MainMenuItems[LIdx].MLabel:=LStr2.Split([';'])[LIdx];
        end;

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

      if not ValueExists(AVR_INI_SECTION_TRANSLATOR,AVR_INI_ENABLE) then
        WriteString(AVR_INI_SECTION_TRANSLATOR,AVR_INI_ENABLE,'0');
      Translator.Enabled:=ReadBool(AVR_INI_SECTION_TRANSLATOR,AVR_INI_ENABLE,False);

      if not ValueExists(AVR_INI_SECTION_TRANSLATOR,AVR_INI_AUTOTRANSLATE) then
        WriteString(AVR_INI_SECTION_TRANSLATOR,AVR_INI_AUTOTRANSLATE,'0');
      Translator.AutoTranslate:=ReadBool(AVR_INI_SECTION_TRANSLATOR,AVR_INI_AUTOTRANSLATE,False);

      if not ValueExists(AVR_INI_SECTION_TRANSLATOR,AVR_INI_LANGUAGECODE) then
        WriteString(AVR_INI_SECTION_TRANSLATOR,AVR_INI_LANGUAGECODE,'');
      Translator.LanguageCode:=ReadString(AVR_INI_SECTION_TRANSLATOR,AVR_INI_LANGUAGECODE,'').ToLower;

      if not ValueExists(AVR_INI_SECTION_TRANSLATOR,AVR_INI_REPLACEUTF8LATIN1SUPPLEMENT) then
        WriteString(AVR_INI_SECTION_TRANSLATOR,AVR_INI_REPLACEUTF8LATIN1SUPPLEMENT,'0');
      Translator.ReplaceUTF8Latin1Supplement:=ReadBool(AVR_INI_SECTION_TRANSLATOR,AVR_INI_REPLACEUTF8LATIN1SUPPLEMENT,False);

      if not ValueExists(AVR_INI_SECTION_TRANSLATOR,AVR_INI_REPLACEUTF8LATIN1EXTENDEDA) then
        WriteString(AVR_INI_SECTION_TRANSLATOR,AVR_INI_REPLACEUTF8LATIN1EXTENDEDA,'0');
      Translator.ReplaceUTF8Latin1ExtendedA:=ReadBool(AVR_INI_SECTION_TRANSLATOR,AVR_INI_REPLACEUTF8LATIN1EXTENDEDA,False);

      if not ValueExists(AVR_INI_SECTION_TRANSLATOR,AVR_INI_REPLACEUTF8LATIN1EXTENDEDB) then
        WriteString(AVR_INI_SECTION_TRANSLATOR,AVR_INI_REPLACEUTF8LATIN1EXTENDEDB,'0');
      Translator.ReplaceUTF8Latin1ExtendedB:=ReadBool(AVR_INI_SECTION_TRANSLATOR,AVR_INI_REPLACEUTF8LATIN1EXTENDEDB,False);

      CRC32:=CalcFileCRC32(ConfigPath+DirectorySeparator+AAVRMAC+'.ini');
    finally
      Free;
    end;
  with AVRConfigArray[Result].Translator do
    begin
      ToTranslate:='';
      TranslatorFileLocked:=False;
      ToTranslateLocked:=False;
      TranslatorItems:=TStringList.Create;
      if Enabled then
        LoadTranslator(Result,AAVRMAC);
    end;
end;

end.

