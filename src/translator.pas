unit translator;

// YTuner: Translator unit.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, LazUTF8, SysUtils, StrUtils,
  fphttpclient, httpdefs, fpjson, opensslsockets, jsonparser,
  avr, common, threadtimer;

const
  UTF8LATIN1SUPPLEMENT_LOW = 160;
  UTF8LATIN1SUPPLEMENT_HIGH = 255;
  UTF8LATIN1EXTENDEDA_LOW = 256;
  UTF8LATIN1EXTENDEDA_HIGH = 383;
  UTF8LATIN1EXTENDEDB_LOW = 384;
  UTF8LATIN1EXTENDEDB_HIGH = 639;

  UTF8Latin1Supplement: array[UTF8LATIN1SUPPLEMENT_LOW..UTF8LATIN1SUPPLEMENT_HIGH] of AnsiString =
    (
    ' ','!','c','L','c','Y','I','S','"','c','a','<<','-','­','R','-','o','+/-','2','3',
    '"','u',' ','.',',','1','0','>>','1/4','1/2','3/4','?','A','A','A','A','A','A',
    'AE','C','E','E','E','E','I','I','I','I','D','N','O','O','O','O','O','x','O',
    'U','U','U','U','Y','TH','ss','a','a','a','a','a','a','ae','c','e','e','e','e',
    'i','i','i','i','d','n','o','o','o','o','o','/','o','u','u','u','u','y','th','y'
    );

  UTF8Latin1ExtendedA: array[UTF8LATIN1EXTENDEDA_LOW..UTF8LATIN1EXTENDEDA_HIGH] of AnsiString =
    (
    'A','a','A','a','A','a','C','c','C','c','C','c','C','c','D','d','D','d',
    'E','e','E','e','E','e','E','e','E','e','G','g','G','g','G','g','G','g',
    'H','h','H','h','I','i','I','i','I','i','I','i','I','i','IJ','ij','J','j',
    'K','k','k','L','l','L','l','L','l','L','l','l','l','N','n','N','n','N',
    'n','n','N','n','O','o','O','o','O','o','OE','oe','R','r','R','r','R','r',
    'S','s','S','s','S','s','S','s','T','t','T','t','T','t','U','u','U','u',
    'U','u','U','u','U','u','U','u','W','w','Y','y','Y','Z','z','Z','z','Z','z','s'
    );

  UTF8Latin1ExtendedB: array[UTF8LATIN1EXTENDEDB_LOW..UTF8LATIN1EXTENDEDB_HIGH] of AnsiString =
    (
    'b','B','B','b','b','b','C','C','c','D','D','D','d','d','E','E','E','F','f',
    'G','Y','h','I','I','K','k','l','l','M','N','n','O','O','o','OI','oi','P','p',
    'R','S','s','E','l','t','T','t','T','U','u','U','U','Y','y','Z','z','E','E','e','e',
    '2','5','?','t','p','I','II','I','!','DZ','Dz','dz','LJ','Lj','lj','NJ','Nj','nj',
    'A','a','I','i','O','o','U','u','U','u','U','u','U','u','U','u','e','A','a','A','a',
    'AE','ae','G','g','G','g','K','k','O','o','O','o','E','e','j','Dz','Dz','dz',
    'G','g','H','P','N','n','A','a','AE','ae','O','o','A','a','A','a','E','e','E','e',
    'I','i','I','i','O','o','O','o','R','r','R','r','U','u','U','u','S','s','T','t',
    '3','3','H','h','N','d','8','8','Z','z','A','a','E','e','O','o','O','o','O','o','O','o',
    'Y','y','l','n','t','j','db','qp','A','C','c','L','T','s','z','?','?','B','U','V',
    'E','e','J','j','Q','q','R','r','Y','y','a','a','a','b','c','c','d','d',
    'e','e','e','e','e','e','e','f','g','g','G','Y','h','h','h','h','i','i','I','l','l','l',
    'B','m','m','m','n','n','N','O','OE','O','o','r','r','r','r','r','r','r'
    );

function ReplaceDiacritics(const AUTF8String: string; ATranslatorIdx: integer): string;
function GoogleTranslateService(AURL,ALanguageCode: String): TJSONStringType;
function GoogleTranslate(AListToTranslate,ALanguageCode : string): string;
function AddNameToTranslatorFile(AName: string; ATranslatorIdx: integer): boolean;
function Translate(AName: string; ATranslatorIdx: integer): string;
procedure LoadTranslator(AAVRMACIdx: integer; AAVRMAC: string);

implementation

function ReplaceDiacritics(const AUTF8String: string; ATranslatorIdx: integer): string;
var
  LPointer: PChar;
  LUnicode: Cardinal;
  LCPLen: integer;
  LIdx: integer;
begin
  Result:='';
  LPointer:=PChar(AUTF8String);
  LIdx:=0;
  try
    repeat
      LUnicode:=UTF8CodepointToUnicode(LPointer,LCPLen);
      if LCPLen=2 then
        with AVRConfigArray[ATranslatorIdx].Translator do
          case Integer(LUnicode.Words[0]) of
            UTF8LATIN1SUPPLEMENT_LOW..UTF8LATIN1SUPPLEMENT_HIGH: Result:=Result+IfThen(ReplaceUTF8Latin1Supplement,UTF8Latin1Supplement[LUnicode],AUTF8String.Substring(LIdx,LCPLen));
            UTF8LATIN1EXTENDEDA_LOW..UTF8LATIN1EXTENDEDA_HIGH: Result:=Result+IfThen(ReplaceUTF8Latin1ExtendedA,UTF8Latin1ExtendedA[LUnicode],AUTF8String.Substring(LIdx,LCPLen));
            UTF8LATIN1EXTENDEDB_LOW..UTF8LATIN1EXTENDEDB_HIGH: Result:=Result+IfThen(ReplaceUTF8Latin1ExtendedB,UTF8Latin1ExtendedB[LUnicode],AUTF8String.Substring(LIdx,LCPLen));
          else
            Result:=Result+AUTF8String.Substring(LIdx,LCPLen)
          end
      else
        Result:=Result+AUTF8String.Substring(LIdx,LCPLen);
      Inc(LPointer,LCPLen);
      Inc(LIdx,LCPLen);
    until (LCPLen=0) or (LUnicode=0);
  except
    Result:=AUTF8String;
  end;
end;

function GoogleTranslateService(AURL,ALanguageCode: String): TJSONStringType;
var
  LResponse: TStringList;
begin
  Result:= '';
  LResponse:=TStringList.Create;
  with TFPHTTPClient.Create(nil) do
    try
      AllowRedirect:=True;
      KeepConnection:=True;
      AddHeader(HTTP_HEADER_ACCEPT,'application/json');
      AddHeader(HTTP_HEADER_ACCEPT_LANGUAGE, 'en-US,'+ALanguageCode.ToLower);
      AddHeader(HTTP_HEADER_CACHE_CONTROL, WEBBROWSER_HTTP_HEADER_CACHE_CONTROL);
      AddHeader(HTTP_HEADER_CONNECTION, WEBBROWSER_HTTP_HEADER_CONNECTION);
      AddHeader(HTTP_HEADER_USER_AGENT,WEBBROWSER_HTTP_HEADER_USER_AGENT);
      try
        Get(AURL,LResponse);
        Result:=LResponse.Text;
      except
        on E: Exception do
          Logging(ltError, string.Join(' ',[MSG_GOOGLE_TRANSLATE,MSG_ERROR,' ('+E.Message+')']));
      end;
    finally
      LResponse.Free;
      Free;
    end;
end;

function GoogleTranslate(AListToTranslate,ALanguageCode : string): string;
var
  LURL: String;
  Index: integer;
  LStringResponse: TJSONStringType;
  LJSONResponse, LJSONTranslation, LJSONTranslationArray: TJSONData;
  LJSONArrayTranslation: TJSONArray;
begin
  Result:='';
  LURL:='https://translate.googleapis.com/translate_a/single?client=gtx'
        +'&q='+HTTPEncode(AListToTranslate)
        +'&sl=auto'
        +'&tl='+ALanguageCode
        +'&dt=t&ie=UTF-8&oe=UTF-8';

  LStringResponse:= GoogleTranslateService(LURL, ALanguageCode);
  if LStringResponse<>'' then
    try
      LJSONResponse:= GetJSON(LStringResponse);
      LJSONTranslation:= LJSONResponse.FindPath('[0]');
      if (LJSONTranslation <> nil) and (LJSONTranslation.JSONType = jtArray) then
        begin
          LJSONArrayTranslation:= TJSONArray(LJSONTranslation);
          for index:=0 to LJSONArrayTranslation.Count-1 do
            begin
              LJSONTranslationArray:= LJSONArrayTranslation[Index];
              if (LJSONTranslationArray <> nil) and (LJSONTranslationArray.JSONType = jtArray) then
                Result:=Trim(TJSONArray(LJSONTranslationArray)[0].AsString);
            end;
        end;
    finally
      LJSONResponse.Free;
    end;
end;

function TranslateThread(AP:Pointer):PtrInt;
var
  LTranslatorIdx: integer;
  LTranslated: string = '';
  LToTranslate, LItemToTranslate: string;
  LIdx, LTIdx, LInStrIdx, LOutStrIdx : integer;
  LKeepTranslate: boolean = False;
  LTranslationFailed: boolean = False;
  LFailureAttempts: integer = 0;

begin
  repeat
    LKeepTranslate:=False;
    with GTThread do
      begin
        Enabled:=False;
        Processing:=True;
        Needed:=False;
      end;
    for LTranslatorIdx:=0 to Length(AVRConfigArray)-1 do
      with AVRConfigArray[LTranslatorIdx].Translator do
        begin
          while (ToTranslate.Length>1) and (not LTranslationFailed) do
            begin
              LFailureAttempts:=0;
              LTranslationFailed:=False;
              try
                if ToTranslate[1]<>';' then
                  begin
                    if ToTranslateLocked then
                      Sleep(300);    // Wait only once.
                    if not ToTranslateLocked then
                      begin
                        ToTranslateLocked:=True;
                        ToTranslate:=';'+ToTranslate;
                        ToTranslateLocked:=False;
                      end
                    else
                      begin
                        ToTranslate:='';
                        GTThread.Processing:=False;
                        Exit;
                      end;
                  end;
                LToTranslate:=ToTranslate;
                LTranslated:=GoogleTranslate(LToTranslate,LanguageCode);
                if LToTranslate.CountChar(';') = LTranslated.CountChar(';') then
                  begin
                    LInStrIdx:=2;
                    LOutStrIdx:=2;
                    for LIdx:=0 to LToTranslate.CountChar(';')-2 do
                      begin
                        TranslatorItems.ValueFromIndex[TranslatorItems.IndexOfName(Copy(LToTranslate,LInStrIdx,Pos(';',LToTranslate,LInStrIdx+1)-LInStrIdx))]:=Copy(LTranslated,LOutStrIdx,Pos(';',LTranslated,LOutStrIdx+1)-LOutStrIdx);
                        LInStrIdx:=Pos(';',LToTranslate,LInStrIdx+1)+1;
                        LOutStrIdx:=Pos(';',LTranslated,LOutStrIdx+1)+1;
                      end;
                    if ToTranslateLocked then
                      Sleep(300);    // Wait only once.
                    if not ToTranslateLocked then
                      begin
                        ToTranslateLocked:=True;
                        ToTranslate:=ToTranslate.Replace(LToTranslate,'');
                        if ToTranslate<>'' then
                          begin
                            if not ToTranslate.StartsWith(';') then
                              ToTranslate:=';'+ToTranslate;
                            if not ToTranslate.EndsWith(';') then
                              ToTranslate:=ToTranslate+';';
                          end;
                        ToTranslateLocked:=False;
                      end
                    else
                      begin
                        ToTranslate:='';
                        GTThread.Processing:=False;
                        Exit;
                      end;
                  end
                else
                  begin
                    LTranslationFailed:=True;
                    Logging(ltError, string.Join(' ',[MSG_GOOGLE_TRANSLATE,'Wrong translation!']));
                  end;
              except
                on E: Exception do
                  begin
                    LTranslationFailed:=True;
                    Logging(ltError, string.Join(' ',[MSG_GOOGLE_TRANSLATE,'Translate process',MSG_ERROR,' ('+E.Message+')']));
                  end;
              end;
              if LTranslationFailed then
                begin
                  if LToTranslate[1]<>';' then
                    LToTranslate:=';'+LToTranslate;
                  LInStrIdx:=2;
                  for LIdx:=0 to LToTranslate.CountChar(';')-2 do
                    begin
                      LItemToTranslate:=Copy(LToTranslate,LInStrIdx,Pos(';',LToTranslate,LInStrIdx+1)-LInStrIdx);
                      LTIdx:=TranslatorItems.IndexOfName(LItemToTranslate);
                      try
                        LFailureAttempts:=StrToInt(TranslatorItems.ValueFromIndex[LTIdx].Replace('!','',[rfReplaceAll]))+1;
                      except
                        LFailureAttempts:=1;
                      end;
                      if LFailureAttempts>4 then
                        begin
                          if ToTranslateLocked then
                            Sleep(300);    // Wait only once.
                          if not ToTranslateLocked then
                            begin
                              ToTranslateLocked:=True;
                              ToTranslate:=ToTranslate.Replace(';'+LItemToTranslate+';',';');
                              while ToTranslate.IndexOf(';;')>=0 do
                                ToTranslate:=ToTranslate.Replace(';;',';');
                              if ToTranslate[1]<>';' then
                                ToTranslate:=';'+ToTranslate;
                              ToTranslateLocked:=False;
                            end
                          else
                            begin
                              ToTranslate:='';
                              GTThread.Processing:=False;
                              Exit;
                            end;
                        end;
                      if LFailureAttempts<=5 then
                        TranslatorItems.ValueFromIndex[LTIdx]:='!'+LFailureAttempts.ToString+'!';
                      LInStrIdx:=Pos(';',LToTranslate,LInStrIdx+1)+1;
                    end;
                  Logging(ltDebug, string.Join(' ',[MSG_GOOGLE_TRANSLATE,'Translation failed.']))
                end
              else
                Logging(ltDebug, string.Join(' ',[MSG_GOOGLE_TRANSLATE,'Translation completed.']));
              if not TranslatorFileLocked then
                begin
                  TranslatorFileLocked:=True;
                  try
                    TranslatorItems.SaveToFile(ConfigPath+DirectorySeparator+'translator-'+AVRMACsArray[LTranslatorIdx]+'.txt');
                  except
                    on E: Exception do
                      Logging(ltError, string.Join(' ',[MSG_TRANSLATOR,MSG_FILE,MSG_SAVE,MSG_ERROR,' ('+E.Message+')']));
                  end;
                  TranslatorFileLocked:=False;
                end;
            end;
        end;
    LTranslatorIdx:=0;
    while (LTranslatorIdx<Length(AVRConfigArray)) and (not LKeepTranslate) do
      begin
        LKeepTranslate:=((AVRConfigArray[LTranslatorIdx].Translator.ToTranslate<>'') or GTThread.Needed);
        LTranslatorIdx:=LTranslatorIdx+1;
      end;
  until (not LKeepTranslate) or LTranslationFailed;
  GTThread.Processing:=False;
end;

procedure TranslatorOnTimer(Sender: TObject);
begin
  BeginThread(@TranslateThread);
end;

function AddNameToTranslatorFile(AName: string; ATranslatorIdx: integer): boolean;
var
  LFileName: string;
  LFileHandle: THandle;
  LLastCharCode: Byte = 0;
begin
  LFileName:=ConfigPath+DirectorySeparator+'translator-'+AVRMACsArray[ATranslatorIdx]+'.txt';
  Result:=True;
  with AVRConfigArray[ATranslatorIdx].Translator do
    if not TranslatorFileLocked then
      begin
        TranslatorFileLocked:=True;
        try
          if not FileExists(LFileName) then
            LFileHandle:=FileCreate(LFileName)
          else
            begin
              LFileHandle:=FileOpen(LFileName,fmOpenReadWrite);
              if FileSeek(LFileHandle,-1,fsFromEnd)>=0 then
                begin
                  FileRead(LFileHandle,LLastCharCode,1);
                  if LLastCharCode<>10 then
                    FileWrite(LFileHandle,String(LineEnding),Length(LineEnding));
                end;
            end;
          FileSeek(LFileHandle,0,fsFromEnd);
          AName:=AName+'='+LineEnding;
          FileWrite(LFileHandle,AName[1],AName.Length);
          FileClose(LFileHandle);
        except
          on E: Exception do
            begin
              Result:=False;
              Logging(ltError, string.Join(' ',[MSG_TRANSLATOR,LFileName,MSG_FILE,MSG_ERROR,' ('+E.Message+')']));
            end;
        end;
        TranslatorFileLocked:=False;
      end
    else
      Logging(ltError, string.Join(' ',[MSG_TRANSLATOR,MSG_FILE,LFileName,'is currently locked!']));
end;

function Translate(AName: string; ATranslatorIdx: integer): string;
var
  LValue: string = '';
  LNameIdx: integer;
  LFailureCount: integer = 0;
begin
  Result:=AName;
  AName:=AName.Replace('=','-',[rfReplaceAll]).Replace(';','-',[rfReplaceAll]).Trim;
  with AVRConfigArray[ATranslatorIdx].Translator do
    begin
      LNameIdx:=TranslatorItems.IndexOfName(AName);
      if LNameIdx=-1 then
        begin
          if AddNameToTranslatorFile(AName,ATranslatorIdx) and (TranslatorItems.IndexOfName(AName)=-1) then
            TranslatorItems.Add(AName+'=')
        end
      else
        begin
          LValue:=TranslatorItems.ValueFromIndex[LNameIdx];
          if AutoTranslate and LValue.StartsWith('!') and LValue.EndsWith('!') then
            begin
              if TryStrToInt(LValue.Replace('!','',[rfReplaceAll]),LFailureCount) then
                begin
                  if LFailureCount>4 then
                    Logging(ltDebug, string.Join(' ',[MSG_GOOGLE_TRANSLATE,'Too many failed attempts to translate:',AName]))
                  else
                    begin
                      LValue:='';
                      if TranslatorItems.IndexOfName(AName)=-1 then
                        TranslatorItems.Add(AName+'=');
                    end;
                end;
            end
          else
            if LValue<>'' then
              Result:=LValue;
        end;
      if (LValue='') and AutoTranslate and (LanguageCode<>'') then
        begin
          if ToTranslate.IndexOf(';'+AName+';')=-1 then
            begin
              if ToTranslateLocked then
                Sleep(300);    // Wait only once.
              if not ToTranslateLocked then
                begin
                  ToTranslateLocked:=True;
                  ToTranslate:=IfThen(ToTranslate='',';',ToTranslate)+AName+';';
                  ToTranslateLocked:=False;
                end
              else
                Exit(AName);
            end;

          if Assigned(GTThread) then
            begin
              if (GTThread.Enabled) then
                GTThread.Needed:=GTThread.Processing
              else
                GTThread.Enabled:=True;
            end
          else
            begin
              GTThread:=TThreadTimer.Create(GT_THREAD);
              with GTThread do
                begin
                  OnTimer:=@TranslatorOnTimer;
                  StartTimer;
                  Logging(ltDebug, string.Join(' ',[MSG_GOOGLE_TRANSLATE,'Starting thread..']))
                end;
            end;
        end;
    end;
end;

procedure LoadTranslator(AAVRMACIdx: integer; AAVRMAC: string);
var
  LTranslatorFile: string;
begin
  LTranslatorFile:=ConfigPath+DirectorySeparator+'translator-'+AAVRMAC+'.txt';
  try
    if FileExists(LTranslatorFile) then
      begin
        AVRConfigArray[AAVRMACIdx].Translator.TranslatorItems.LoadFromFile(LTranslatorFile);
        Logging(ltInfo, 'Translator: loaded');
      end;
  except
    on E: Exception do
      Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%},MSG_TRANSLATOR,MSG_FILE_LOAD_ERROR,' ('+E.Message+')']));
  end;
end;

end.

