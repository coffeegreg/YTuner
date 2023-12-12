unit my_stations;

// Ytuner : Custom stations list files support unit.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  md5, common;

const
  MY_STATIONS_EXT : Array Of AnsiString = ('.ini','.yaml','.yml');

type
  TMyStation = record
                 MSID, MSName, MSURL, MSLogoURL: string;
               end;

  TMyStations = array of TMyStation;

  TMyStationsGroup = array of record
                                MSCategory: string;
                                MSStations: TMyStations;
                              end;

  TMSStation = record
                 Category: string;
                 Station: TMyStation;
               end;

function GetMyStationByID(AID: string): TMSStation;
function ReadMyStationsINIFile(AMyStationsFileName: string): boolean;
function ReadMyStationsYAMLFile(AMyStationsFileName: string): boolean;

var
  MyStationsEnabled: boolean = True;
  MyStationsFileName: string = 'stations.ini';
  MyStationsAutoRefreshPeriod: integer = 0;
  MyStationsFileAge: LongInt = 0;
  MyStationsFileCRC32: LongWord = 0;
  MyStations: TMyStationsGroup;

implementation

function GetMyStationByID(AID: string): TMSStation;
var
  i,j: integer;
begin
  for i:=0 to Length(MyStations)-1 do
    for j:=0 to Length(MyStations[i].MSStations)-1 do
      if MyStations[i].MSStations[j].MSID=AID then
        with Result do
          begin
            Category:=MyStations[i].MSCategory;
            Station:=MyStations[i].MSStations[j];
            Exit;
          end;
end;

function ReadMyStationsINIFile(AMyStationsFileName: string): boolean;
var
  LStr: TStrings;
  i,j: integer;
  LMyStationsCount: integer = 0;
  LMSIniTmpValue: string;
begin
  Result:=False;
  LStr:=TStringList.Create;
  try
    with TIniFile.Create(AMyStationsFileName) do
      try
        try
          ReadSections(LStr);
          SetLength(MyStations,LStr.Count);
          for i:=0 to LStr.Count-1 do
            MyStations[i].MSCategory:=LStr[i];
          for i:=0 to Length(MyStations)-1 do
            begin
              LStr.Clear;
              ReadSectionRaw(MyStations[i].MSCategory,LStr);
              SetLength(MyStations[i].MSStations,LStr.Count);
              for j:=0 to LStr.Count-1 do
                if Length(LStr[j].Split(['=']))>1 then
                  with MyStations[i].MSStations[j] do
                    begin
                      MSName:=LStr[j].Split(['='])[0].Trim;
                      LMSIniTmpValue:=LStr[j].Substring(LStr[j].IndexOf('=')+1);
                      MSURL:=LMSIniTmpValue.Split(['|'])[0].Trim;
                      If Length(LMSIniTmpValue.Split(['|']))>1 then
                        MSLogoURL:=LMSIniTmpValue.Split(['|'])[1].Trim;
// Crash on old Linux (?!)   MSID:=MY_STATIONS_PREFIX+'_'+MD5Print(MD5String(MSName+MSURL)).Substring(0,12).ToUpper;
                      MSID:=MY_STATIONS_PREFIX+'_'+MD5Print(MD5String(MSName+MSURL)).Substring(0,12);
                      MSID:=MSID.ToUpper;
                    end;
              LMyStationsCount:=LMyStationsCount+LStr.Count;
            end;
          Result:=(LMyStationsCount>0);
        except
          Result:=False;
        end;
      finally
        Free;
      end;
  finally
    LStr.Free;
    if Result then
      Logging(ltInfo, MSG_SUCCESSFULLY_LOADED+IntToStr(LMyStationsCount)+' my '+MSG_STATIONS)
    else
      Logging(ltError, 'INI'+MSG_FILE_LOAD_ERROR)
  end;
end;

function ReadMyStationsYAMLFile(AMyStationsFileName: string): boolean;
const
  LSeparators: array of string = (': ','|');
var
  LStationsFileContent: TStrings;
  i: integer = 0;
  j: integer = 0;
  c: integer = 0;
  LMyStationsCount: integer = 0;
begin
  Result:=False;
  LStationsFileContent:=TStringList.Create;
  with LStationsFileContent do
    try
      try
        LStationsFileContent.LoadFromFile(AMyStationsFileName);
        while i<LStationsFileContent.Count do
          begin
            if LStationsFileContent.Strings[i].EndsWith(':') then
              begin
                SetLength(MyStations,c+1);
                MyStations[c].MSCategory:=LStationsFileContent.Strings[i].Trim.TrimRight(':');
                c:=c+1;
                j:=0;
              end
            else
              if (MyStations[c-1].MSCategory <> '') and (LStationsFileContent.Strings[i].Contains(': ')) then
                begin
                   SetLength(MyStations[c-1].MSStations,j+1);
                   MyStations[c-1].MSStations[j].MSName:=LStationsFileContent.Strings[i].Split(LSeparators)[0].Trim;
                   MyStations[c-1].MSStations[j].MSURL:=LStationsFileContent.Strings[i].Split(LSeparators)[1].Trim;
                   if Length(LStationsFileContent.Strings[i].Split(LSeparators))>2 then
                     MyStations[c-1].MSStations[j].MSLogoURL:=LStationsFileContent.Strings[i].Split(LSeparators)[2].Trim;
// Crash on old Linux (?!)   MyStations[c-1].MSStations[j].MSID:= MY_STATIONS_PREFIX+'_'+MD5Print(MD5String(MyStations[c-1].MSStations[j].MSName+MyStations[c-1].MSStations[j].MSURL)).Substring(0,12).ToUpper;
                   MyStations[c-1].MSStations[j].MSID:= MY_STATIONS_PREFIX+'_'+MD5Print(MD5String(MyStations[c-1].MSStations[j].MSName+MyStations[c-1].MSStations[j].MSURL)).Substring(0,12);
                   MyStations[c-1].MSStations[j].MSID:=MyStations[c-1].MSStations[j].MSID.ToUpper;
                   LMyStationsCount:=LMyStationsCount+1;
                   j:=j+1;
                end;
            i:=i+1;
          end;
        Result:=(LMyStationsCount>0);
      except
        Result:=False;
      end;
    finally
      Free;
      if Result then
        Logging(ltInfo, MSG_SUCCESSFULLY_LOADED+IntToStr(LMyStationsCount)+' my '+MSG_STATIONS)
      else
        Logging(ltError, 'YAML'+MSG_FILE_LOAD_ERROR)
    end;
end;

end.

