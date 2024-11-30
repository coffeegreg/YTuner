unit httpserver;

// YTuner: Web serwer unit.

{$mode ObjFPC}{$H+}

// Definitions that support selected image formats for reading external logo stations.
{$DEFINE READERPNG}     //Comment/uncomment this line to disable/enable support PNG images.
{$DEFINE READERGIF}     //Comment/uncomment this line to disable/enable support GIF images.
//{$DEFINE READERTIFF}    //Comment/uncomment this line to disable/enable support TIFF images.

// Definition that supports selected image format supported by your AVR device(s).
// You have to choose one of them. Don't forget to clean cache folder after change.
{$DEFINE WRITERJPG}     //Comment/uncomment this line to disable/enable support JPG images.
{$IFNDEF WRITERJPG}
{$DEFINE WRITERPNG}     //Comment/uncomment this line to disable/enable support PNG images.
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils,
  fpimage, fpimgcanv,
{$IFDEF WRITERJPG}
  fpwritejpeg,
{$ELSE}
  {$IFDEF WRITERPNG}
   fpwritepng,
  {$ENDIF WRITERPNG}
{$ENDIF WRITERJPG}
  fpreadjpeg,           // Default support JPG images (read external logo stations)
{$IFDEF READERPNG}
  fpreadpng,
{$ENDIF}
{$IFDEF READERGIF}
  fpreadgif,
{$ENDIF}
{$IFDEF READERTIFF}
  fpreadtiff,
{$ENDIF}
  fphttpclient, httpdefs, httproute, DOM,
  common, vtuner, my_stations, radiobrowser, bookmark, avr, translator;

const
  WEB_SERVICE = 'Web Service';
  PATH_HTTP = 'http://';
  PATH_SETUPAPP = 'setupapp';
  PATH_STATXML_ASP = 'statxml.asp';
  PATH_LOGINXML_ASP = 'loginxml.asp';
  PATH_FAVXML_ASP = 'favxml.asp';
  PATH_NAVXML_ASP = 'navxml.asp';
  PATH_SEARCH_ASP = 'search.asp';
  PATH_ROOT = 'ytuner';
  PATH_PLAY = 'play';
  PATH_STATION = 'station';
  PATH_SEARCH = 'search';
  PATH_ICON = 'icon';
  PATH_ABOUT = 'about';
  PATH_EMPTY = 'empty';
  PATH_VTUNER_ASP = 'func/dynamOD.asp';

  PATH_RADIOBROWSER_GENRE = 'genre';
  PATH_RADIOBROWSER_COUNTRY = 'country';
  PATH_RADIOBROWSER_LANGUAGE = 'language';
  PATH_RADIOBROWSER_POPULAR = 'popular';
  PATH_CATEGORY_TYPE = 'categorytype';
  PATH_CATEGORY = 'category';

  PATH_RADIOBROWSER_CATEGORIES : array[0..3] of string = (PATH_RADIOBROWSER_GENRE,PATH_RADIOBROWSER_COUNTRY,PATH_RADIOBROWSER_LANGUAGE,PATH_RADIOBROWSER_POPULAR);

  MSG_QUERY_TOO_SHORT = 'Search query too short.';
  MSG_NO_STATIONS_FOUND = 'No station(s) found';
  MSG_FOR_THIS_CATEGORY = ' for this category';
  MSG_UNKNOWN_ROUTE = 'Unknown route';
  MSG_EMPTY_FOLDER = 'This folder is intentionally empty...';

var
  WebServerIPAddress: string;
  WebServerPort: integer = 80;
  IconSize: integer = 200;
  IconCache: boolean = False;
  HTTPCodeRedirect: integer = HTTP_CODE_FOUND;

procedure RegisterServerRoutes;

// Registered routes routines
procedure DefaultPage(AReq: TRequest; ARes: TResponse);
procedure SetupAppLoginXMLPage(AReq: TRequest; ARes: TResponse);
procedure GetStation(AReq: TRequest; ARes: TResponse);
procedure BookmarkService(AReq: TRequest; ARes: TResponse);
procedure GetMyStationsCategories(AReq: TRequest; ARes: TResponse);
procedure GetMyStationsOfCategory(AReq: TRequest; ARes: TResponse);
procedure GetIcon(AReq: TRequest; ARes: TResponse);
procedure GetRadioBrowserRootDirectory(AReq: TRequest; ARes: TResponse);
procedure GetRadioBrowserCategoryType(AReq: TRequest; ARes: TResponse);
procedure GetRadioBrowserCategoryStations(AReq: TRequest; ARes: TResponse);
procedure GetBookmarkStations(AReq: TRequest; ARes: TResponse);
procedure PlayStation(AReq: TRequest; ARes: TResponse);
procedure GetRadioBrowserSearchedStations(AReq: TRequest; ARes: TResponse);
procedure GetAbout(AReq: TRequest; ARes: TResponse);
procedure GetEmpty(AReq: TRequest; ARes: TResponse);
procedure VTunerRedirect(AReq: TRequest; ARes: TResponse);

// Service routines
function GetRange(var AStart, AHowMany: integer; AQueryFields: TStrings): boolean;
procedure GetPageRange(var AFirstElement, ALastElement :integer; AArrayLen: integer; AQueryFields: TStrings);
function  GetStationInfo(AReq: TRequest; AMyPage: TVTunerPage): string;
procedure GetRadioBrowserStations(ARBStations: TRBStations; ATotalCount: integer; AReq: TRequest; ARes: TResponse);

// Server response routines
procedure ServerResponse(AResponseCode: integer; AResponseContentType: TResponseContentType; ARes: TResponse; AData: TStream);
procedure ServerResponse(AResponseCode: integer; AResponseContentType: TResponseContentType; ARes: TResponse; AData: string);
procedure SendPageResponse(AResponseCode: integer; AResponseContentType: TResponseContentType; ARes: TResponse; AMyPage: TVTunerPage);

// vTuner structures routines
function SetVTunerStation(AStation: TMSStation; AReq: TRequest): TVTunerStation;
function SetVTunerStation(ARBStation: TRBStation; AReq: TRequest; ATranslatorIdx: integer): TVTunerStation;
function SetVTunerDirectory(ATitle, ADestination: string; AItemCount: integer; ATranslatorIdx: integer): TVTunerDirectory;
function SetVTunerDisplay(AMessage: string): TVTunerDisplay;

// Display AVR messsages
procedure DisplayMessage(AMessage: string; var ARes: TResponse);

implementation

procedure RegisterServerRoutes;
begin
  HTTPRouter.RegisterRoute('/', @DefaultPage, true);
  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_LOGINXML_ASP, @SetupAppLoginXMLPage, false);
  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_STATXML_ASP, @GetStation, false);
  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/'+PATH_FAVXML_ASP, @BookmarkService, false);
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_MY_STATIONS, @GetMyStationsCategories, false);
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_MY_STATIONS+'/:'+PATH_CATEGORY, @GetMyStationsOfCategory, false);
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_ICON, @GetIcon, false);
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_RADIOBROWSER, @GetRadioBrowserRootDirectory, false);
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_RADIOBROWSER+'/:'+PATH_CATEGORY_TYPE, @GetRadioBrowserCategoryType, false);
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_RADIOBROWSER+'/:'+PATH_CATEGORY_TYPE+'/:'+PATH_CATEGORY, @GetRadioBrowserCategoryStations, false);
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_BOOKMARK, @GetBookmarkStations, false);
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_ABOUT, @GetAbout, false);
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_EMPTY, @GetEmpty, false);

// This endpoint is for redirect for vTuner radio station link
  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_VTUNER_ASP, @VTunerRedirect, false);

///////////////////////////////////////////////
// Group of routes NOT tested with real AVR. //
///////////////////////////////////////////////

//Play station ?
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_PLAY, @PlayStation, false);

//Search stations?
  HTTPRouter.RegisterRoute('/'+PATH_ROOT+'/'+PATH_SEARCH, @GetRadioBrowserSearchedStations, false);

//Some AVR use "NavXML.asp" and "gofile" query parameter with values like "S-ByLocation" or "LocationLevelTwo".
//We need to get some documentation and/or investigate some logs from users.
//Until then, we will route all such queries to browse Radiobrowser directory.
  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_NAVXML_ASP, @GetRadioBrowserRootDirectory, false);

//Some AVR use "Search.asp" and "sSearchtype" query parameter with values like "1" or "2".
//We need to get some documentation and/or investigate some logs from users.
//Until then, we will route all such queries to search Radiobrowser directory.
//UPDATE 06.01.2024 :
//Due to this discussion: https://github.com/coffeegreg/YTuner/discussions/17 about "Grundig Sonoclock 940A"
//we know that some devices use the "search.asp" query to "play" a station.
//We'll recognize such a requests with "sSearchtype=3" as a play requests. We'll also skip the specific parameter
//"ven=grn6" because other devices may also use "sSearchtype=3" to play but identify themselves with a different value for the "ven" parameter
//It seems that "sSearchtype=2" is used to real stations search with "search" query parameter and "sSearchtype=1" for search podcast.
//For now, we consider any sSearchtype values other than "3" to be searching for a station.

  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_SEARCH_ASP, @GetRadioBrowserSearchedStations, false);

// Some AVRs use this endpoint without any parameters for the requested operation (add/del).
// We can assume this is a bookmark query but how these AVR requests for operations like "add" & "del"?
//  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_FAVXML_ASP, @GetBookmarkStations, false);
end;

// BEGIN - Registered routes routines
procedure DefaultPage(AReq: TRequest; ARes: TResponse);
begin
  Logging(ltDebug, MSG_UNKNOWN_ROUTE+' : '+AReq.Method+' '+AReq.URI);
  ServerResponse(HTTP_CODE_NOT_FOUND,ctNone,ARes,'');
end;

procedure SetupAppLoginXMLPage(AReq: TRequest; ARes: TResponse);
var
  LMyPage: TVTunerPage;
  LIdx: integer=0;

  procedure LSetMainMenuItems(AAAVRConfigIdx: integer);
  var
    LLIdx: integer = 0;
  begin
    while LLIdx<Length(AVRConfigArray[AAAVRConfigIdx].MainMenuItems) do
      begin
        case AVRConfigArray[AAAVRConfigIdx].MainMenuItems[LLIdx].MIdentifier of
          AVR_MAINMENU_IDENTIFIER_MYSTATIONS:
            if MyStationsEnabled then
              begin
                LMyPage.Add(SetVTunerDirectory(AVRConfigArray[AAAVRConfigIdx].MainMenuItems[LLIdx].MLabel,PATH_ROOT+'/'+PATH_MY_STATIONS,Length(MyStations),-1));
                LIdx:=LIdx+1;
              end;
          AVR_MAINMENU_IDENTIFIER_RADIOBROWSER:
            if RadiobrowserEnabled then
              begin
                LMyPage.Add(SetVTunerDirectory(AVRConfigArray[AAAVRConfigIdx].MainMenuItems[LLIdx].MLabel,PATH_ROOT+'/'+PATH_RADIOBROWSER,4,-1));
                LIdx:=LIdx+1;
              end;
          AVR_MAINMENU_IDENTIFIER_BOOKMARKS:
            if BookmarkEnabled then
              begin
                LMyPage.Add(SetVTunerDirectory(AVRConfigArray[AAAVRConfigIdx].MainMenuItems[LLIdx].MLabel,PATH_ROOT+'/'+PATH_BOOKMARK,GetBookmarkItemsCount(AReq.QueryFields.Values[PATH_PARAM_MAC]),-1));
                LIdx:=LIdx+1;
              end;
          AVR_MAINMENU_IDENTIFIER_ABOUT:
            begin
              LMyPage.Add(SetVTunerDirectory(AVRConfigArray[AAAVRConfigIdx].MainMenuItems[LLIdx].MLabel,PATH_ROOT+'/'+PATH_ABOUT,2,-1));
              LIdx:=LIdx+1;
            end;
        end;
        LLIdx:=LLIdx+1;
      end;
  end;

begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  if AReq.QueryFields.Values[PATH_PARAM_TOKEN]='0' then
    ServerResponse(HTTP_CODE_OK,ctXML,ARes,VT_XML_ENCRYPTEDTOKEN.Replace('><','>'+MyToken+'<'))
  else
    begin
      LMyPage:=TVTunerPage.Create;
      try
        LSetMainMenuItems(GetAVRConfigIdx(AReq));
        LMyPage.TotalItemsCount:=LIdx;
        SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
      finally
        LMyPage.Free;
      end;
    end;
end;

procedure GetStation(AReq: TRequest; ARes: TResponse);
var
  LMyPage: TVTunerPage;
  LStream: TStream;
  LCode: integer;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  case AReq.QueryFields.Values[PATH_PARAM_ID].Substring(2,1) of
    '_': begin
           LMyPage:=TVTunerPage.Create;
           try
             if GetStationInfo(AReq,LMyPage).IsEmpty then
               begin
                 Logging(ltError, MSG_NO_STATIONS_FOUND+' with ID: '+AReq.QueryFields.Values[PATH_PARAM_ID]+'');
                 DisplayMessage(MSG_NO_STATIONS_FOUND,ARes);
               end
             else
               SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
           finally
             LMyPage.Free;
           end;
         end;
    'B': begin
           LStream:=TMemoryStream.Create;
           try
             if GetBookmarkStationInfo(AReq.QueryFields.Values[PATH_PARAM_MAC],AReq.QueryFields.Values[PATH_PARAM_ID],LStream) then
               LCode:=HTTP_CODE_OK
             else
               LCode:=HTTP_CODE_NOT_FOUND;
             ServerResponse(LCode,ctXML,ARes,LStream);
           finally
             FreeAndNil(LStream);
           end;
         end;
  end;
end;

procedure BookmarkService(AReq: TRequest; ARes: TResponse);
var
  LMyPage: TVTunerPage;
  LXMLDoc: TXMLDocument;
  LStationID: string = '';
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LMyPage:=TVTunerPage.Create;
  try
    try
      LStationID:=GetStationInfo(AReq,LMyPage);
    except
      LStationID:='';
    end;
    if LStationID.IsEmpty then
      begin
        Logging(ltError, MSG_NO_STATIONS_FOUND+' with ID: '+AReq.QueryFields.Values[PATH_PARAM_ID]+'');
        DisplayMessage(MSG_NO_STATIONS_FOUND,ARes);
      end
    else
      begin
        TVTunerStation(LMyPage.Items[0]).UID:=StringReplace(TVTunerStation(LMyPage.Items[0]).UID,'_','B',[rfIgnoreCase]);
        LXMLDoc:=TXMLDocument.Create;
        try
          LMyPage.MakeXML(LXMLDoc);
          SetBookmark(AReq.QueryFields.Values[PATH_PARAM_MAC],AReq.QueryFields.Values[PATH_PARAM_FAV],LXMLDoc.FirstChild.ChildNodes[1]);
        finally
          LXMLDoc.Free;
        end;
      end;
  finally
    LMyPage.Free;
  end;
end;

procedure GetMyStationsCategories(AReq: TRequest; ARes: TResponse);
var
  LMyPage: TVTunerPage;
  i: integer;
  LFirstElement: integer = 0;
  LLastElement: integer = 0;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  GetPageRange(LFirstElement,LLastElement,Length(MyStations),AReq.QueryFields);
  LMyPage:=TVTunerPage.Create;
  LMyPage.TotalItemsCount:=Length(MyStations);
  try
    if Length(MyStations)>0 then
      begin
        for i:=LFirstElement to LLastElement do
          with MyStations[i] do
            LMyPage.Add(SetVTunerDirectory(MSCategory,PATH_ROOT+'/'+PATH_MY_STATIONS+'/'+URLEncode(MSCategory),Length(MSStations),-1));
        SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
      end
    else
      begin
        Logging(ltError, MSG_NO_STATIONS_FOUND);
        DisplayMessage(MSG_NO_STATIONS_FOUND,ARes);
      end;
  finally
    LMyPage.Free;
  end;
end;

procedure GetMyStationsOfCategory(AReq: TRequest; ARes: TResponse);
var
  i: integer;
  LMyPage: TVTunerPage;
  LMSStation: TMSStation;
  LCategoryIdx: integer = 0;
  LFirstElement: integer = 0;
  LLastElement: integer = 0;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LMSStation.Category:=HTTPDecode(AReq.RouteParams[PATH_CATEGORY]);
  while (LCategoryIdx<Length(MyStations)-1) and (MyStations[LCategoryIdx].MSCategory.ToLower<>LMSStation.Category.ToLower) do
    Inc(LCategoryIdx);
  if MyStations[LCategoryIdx].MSCategory.ToLower=LMSStation.Category.ToLower then
    begin
      GetPageRange(LFirstElement,LLastElement,Length(MyStations[LCategoryIdx].MSStations),AReq.QueryFields);
      LMyPage:=TVTunerPage.Create;
      LMyPage.TotalItemsCount:=Length(MyStations[LCategoryIdx].MSStations);
      try
        if Length(MyStations[LCategoryIdx].MSStations)>0 then
          begin
            for i:=LFirstElement to LLastElement do
              begin
                LMSStation.Station:=MyStations[LCategoryIdx].MSStations[i];
                LMyPage.Add(SetVTunerStation(LMSStation,AReq));
              end;
            SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
          end
        else
          begin
            Logging(ltError, MSG_NO_STATIONS_FOUND+' : '+LMSStation.Category);
            DisplayMessage(MSG_NO_STATIONS_FOUND+' : '+LMSStation.Category,ARes);
          end;
      finally
        LMyPage.Free;
      end;
    end;
end;

procedure GetIcon(AReq: TRequest; ARes: TResponse);
var
  LStream: TMemoryStream;
  LURL: string = '';
  LHeaderAcceptStr: string;
  LScaleFactor: Real;
  LImageIn: TFPMemoryImage = nil;
  LImageReader: TFPCustomImageReader = nil;
  LImageWriter: TFPCustomImageWriter = nil;
  LImageFile: string;
  LGetImageFromURL: boolean = True;
  LRBStation: TRBStation;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LImageFile:=AReq.QueryFields.Values[PATH_PARAM_ID];
  LStream:=TMemoryStream.Create;
  try
    if IconCache and FileExists(CachePath+DirectorySeparator+LImageFile) then
      begin
        try
          LStream.LoadFromFile(CachePath+DirectorySeparator+LImageFile);
          if LStream.Size>0 then
            begin
              LGetImageFromURL:=False;
              {$IFDEF WRITERJPG}
              ServerResponse(HTTP_CODE_OK,ctJPG,ARes,LStream);
              {$ELSE}
              {$IFDEF WRITERPNG}
              ServerResponse(HTTP_CODE_OK,ctPNG,Res,LStream);
              {$ENDIF}
              {$ENDIF}
            end;
        except
          on E : Exception do
            Logging(ltError, LImageFile+MSG_FILE_LOAD_ERROR+' ('+E.Message+')');
        end;
      end;
    if LGetImageFromURL then
      begin
        case LImageFile.Substring(0,2) of
          MY_STATIONS_PREFIX: LURL:=GetMyStationByID(LImageFile).Station.MSLogoURL;
          RADIOBROWSER_PREFIX: begin
                                 LRBStation:=TRBStation.Create;
                                 try
                                   GetRBStationByID(LRBStation,LImageFile.Substring(3,12),GetAVRConfigIdx(AReq));
                                   LURL:=LRBStation.RBSIcon;
                                 finally
                                   LRBStation.Free;
                                 end;
                               end;
        end;
        if LURL <> '' then
          begin
            with TFPHttpClient.Create(nil) do
              try
                AllowRedirect:=True;
                LHeaderAcceptStr:=HTTP_RESPONSE_CONTENT_TYPE[ctJPG];
                {$IFDEF READERPNG}
                LHeaderAcceptStr:=LHeaderAcceptStr+','+HTTP_RESPONSE_CONTENT_TYPE[ctPNG];
                {$ENDIF}
                {$IFDEF READERGIF}
                LHeaderAcceptStr:=LHeaderAcceptStr+','+HTTP_RESPONSE_CONTENT_TYPE[ctGIF];
                {$ENDIF}
                {$IFDEF READERTIFF}
                LHeaderAcceptStr:=LHeaderAcceptStr+','+HTTP_RESPONSE_CONTENT_TYPE[ctTIFF];
                {$ENDIF}
                AddHeader(HTTP_HEADER_ACCEPT,LHeaderAcceptStr);
                AddHeader(HTTP_HEADER_USER_AGENT,YTUNER_USER_AGENT+'/'+APP_VERSION);
                try
                  Logging(ltDebug, MSG_GETTING+' '+LURL);
                  Get(LURL,LStream);
                except
                  on E : Exception do
                    begin
                      LStream.Size:=0;
                      Logging(ltError, 'Error getting image from: "'+LURL+'"? ('+E.Message+')');
                    end;
                end;
                if LStream.Size>0 then
                  begin
                    LStream.Position:=0;
                    LImageIn:=TFPMemoryImage.Create(0,0);
                    LImageIn.UsePalette:=False;
                    if LImageIn.FindReaderFromStream(LStream)<>nil then
                      LImageReader:=LImageIn.FindReaderFromStream(LStream).Create;
                  end;
              finally
                Free;
              end;
            try
              if Assigned(LImageReader) then
                begin
                  LStream.Position := 0;
                  {$IFDEF WRITERJPG}
                  LImageWriter:=TFPWriterJPEG.Create;
                  {$ELSE}
                  {$IFDEF WRITERPNG}
                  LImageWriter:=TFPWriterPNG.Create;
                  {$ENDIF}
                  {$ENDIF}
                  try
                    with LImageIn do
                      begin
                        try
                          LoadFromStream(LStream,LImageReader);
                          if (Width>IconSize) or (Height>IconSize) then
                            begin
                              if Width>=Height then
                                LScaleFactor:=IconSize/Width
                              else
                                LScaleFactor:=IconSize/Height;

                              with TFPImageCanvas.Create(TFPMemoryImage.Create(Round(LImageIn.Width*LScaleFactor),Round(LImageIn.Height*LScaleFactor))) do
                                try
                                  Image.UsePalette:=false;
                                  StretchDraw(0,0,Round(LImageIn.Width*LScaleFactor),Round(LImageIn.Height*LScaleFactor),LImageIn);
                                  Image.SaveToStream(LStream,LImageWriter);
                                finally
                                  Image.Free;
                                  Free;
                                end;
                            end
                          else
                            SaveToStream(LStream,LImageWriter);
                        except
                          on E : Exception do
                          Logging(ltError, 'Unsupported stream with image type: "'+LURL+'"? /'+E.Message+'/');
                        end;
                      end;
                    {$IFDEF WRITERJPG}
                    ServerResponse(HTTP_CODE_OK,ctJPG,ARes,LStream);
                    {$ELSE}
                    {$IFDEF WRITERPNG}
                    ServerResponse(HTTP_CODE_OK,ctPNG,ARes,LStream);
                    {$ENDIF}
                    {$ENDIF}
                    if IconCache then
                      begin
                        if not DirectoryExists(CachePath) then CreateDir(CachePath);
                        try
                          LStream.SaveToFile(CachePath+DirectorySeparator+LImageFile);
                        except
                          on E : Exception do
                            Logging(ltError, LImageFile+MSG_FILE_SAVE_ERROR+' ('+E.Message+')');
                        end;
                      end;
                  finally
                    FreeAndNil(LImageWriter);
                  end;
                end
              else
                Logging(ltError, 'Unsupported image type: "'+LURL+'"');
            finally
              if Assigned(LImageIn) then
                FreeAndNil(LImageIn);
              if Assigned(LImageReader) then
                FreeAndNil(LImageReader);
            end;
          end;
      end;
  finally
    FreeAndNil(LStream);
  end;
  if not ARes.ContentSent then
    begin
      if LURL <> '' then
        Logging(ltWarning, 'Cannot process image from "'+LURL+'"');
      ServerResponse(HTTP_CODE_NOT_FOUND,ctNone,ARes,'');
    end;
end;

procedure GetRadioBrowserRootDirectory(AReq: TRequest; ARes: TResponse);
var
  LRootCategoryTypesCount: TRBRootCategoryTypesCount;
  LMyPage: TVTunerPage;
  LAVRConfigIdx: integer;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LAVRConfigIdx:=GetAVRConfigIdx(AReq);
  GetRootItems(LRootCategoryTypesCount,LAVRConfigIdx);
  LMyPage:=TVTunerPage.Create;
  try
    LMyPage.TotalItemsCount:=4;
    LMyPage.Add(SetVTunerDirectory('Genres',PATH_ROOT+'/'+PATH_RADIOBROWSER+'/'+PATH_RADIOBROWSER_GENRE,LRootCategoryTypesCount[rbctGenre],LAVRConfigIdx));
    LMyPage.Add(SetVTunerDirectory('Countries',PATH_ROOT+'/'+PATH_RADIOBROWSER+'/'+PATH_RADIOBROWSER_COUNTRY,LRootCategoryTypesCount[rbctCountry],LAVRConfigIdx));
    LMyPage.Add(SetVTunerDirectory('Languages',PATH_ROOT+'/'+PATH_RADIOBROWSER+'/'+PATH_RADIOBROWSER_LANGUAGE,LRootCategoryTypesCount[rbctLanguage],LAVRConfigIdx));
    LMyPage.Add(SetVTunerDirectory('Most popular',PATH_ROOT+'/'+PATH_RADIOBROWSER+'/'+PATH_RADIOBROWSER_POPULAR,LRootCategoryTypesCount[rbctPopular],LAVRConfigIdx));
    SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
  finally
    LMyPage.Free;
  end;
end;

procedure GetRadioBrowserCategoryType(AReq: TRequest; ARes: TResponse);
var
  LMyPage: TVTunerPage;
  LTotalCount: integer = 0;
  i, LCategoryIdx: integer;
  LRBCategories: TRBCategories;
begin
  LCategoryIdx:=IndexStr(AReq.RouteParams[PATH_CATEGORY_TYPE],PATH_RADIOBROWSER_CATEGORIES);
  case LCategoryIdx of
    0..2: begin
            Logging(ltDebug, AReq.Method+' '+AReq.URI);
            LRBCategories:=TRBCategories.Create;
            try
              LTotalCount:=GetCategoryItems(LRBCategories,TRBAllCategoryTypes(LCategoryIdx),AReq);
              if LTotalCount>0 then
                begin
                  LMyPage:=TVTunerPage.Create;
                  try
                    LMyPage.TotalItemsCount:=LTotalCount;
                    for i:=0 to LRBCategories.Count-1 do
                      with TRBCategory(LRBCategories.Objects[i]) do
                        LMyPage.Add(SetVTunerDirectory(RBCName,PATH_ROOT+'/'+PATH_RADIOBROWSER+'/'+PATH_RADIOBROWSER_CATEGORIES[LCategoryIdx]+'/'+URLEncode(RBCName),RBCMaxCount,GetAVRConfigIdx(AReq)));
                    SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
                  finally
                    LMyPage.Free;
                  end;
                end
              else
                begin
                  Logging(ltError, MSG_NO_STATIONS_FOUND+MSG_FOR_THIS_CATEGORY);
                  DisplayMessage(MSG_NO_STATIONS_FOUND+MSG_FOR_THIS_CATEGORY,ARes);
                end;
            finally
              LRBCategories.Free;
            end;
          end;
    3: GetRadioBrowserCategoryStations(AReq,ARes);
  else
    Logging(ltError, 'Radiobrowser category type not supported ?!');
  end;
end;

procedure GetRadioBrowserCategoryStations(AReq: TRequest; ARes: TResponse);
var
  LRBCategoryTypeIdx: integer;
  LRBStations: TRBStations;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LRBCategoryTypeIdx:=IndexStr(AReq.RouteParams[PATH_CATEGORY_TYPE],PATH_RADIOBROWSER_CATEGORIES);
  case LRBCategoryTypeIdx of
    0..3: begin
            LRBStations:=TRBStations.Create;
            try
              GetRadioBrowserStations(LRBStations,GetStationsByCategory(LRBStations,TRBAllCategoryTypes(LRBCategoryTypeIdx),AReq.RouteParams[PATH_CATEGORY],AReq),AReq,ARes);
            finally
              LRBStations.Free;
            end;
          end
  else
    ServerResponse(HTTP_CODE_NOT_FOUND,ctNone,ARes,'');
  end;
end;

procedure GetBookmarkStations(AReq: TRequest; ARes: TResponse);
var
  LStream: TStream;
  LCode: integer;
  FirstElement: integer = 0;
  LastElement: integer = 0;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LStream:=TMemoryStream.Create;
  try
    GetPageRange(FirstElement,LastElement,GetBookmarkItemsCount(AReq.QueryFields.Values[PATH_PARAM_MAC]),AReq.QueryFields);
    if GetBookmark(AReq,FirstElement,LastElement,LStream) then
      LCode:=HTTP_CODE_OK
    else
      LCode:=HTTP_CODE_NOT_FOUND;
    ServerResponse(LCode,ctXML,ARes,LStream);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure PlayStation(AReq: TRequest; ARes: TResponse);
var
  LURL: string = '';
  LCode: integer;
  LRBStation: TRBStation;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  case AReq.QueryFields.Values[PATH_PARAM_ID].Substring(0,2) of
    MY_STATIONS_PREFIX:
      with GetMyStationByID(AReq.QueryFields.Values[PATH_PARAM_ID]) do
        if Station.MSID<>'' then
          LURL:=StripHttps(Station.MSURL,AReq);
    RADIOBROWSER_PREFIX:
      begin
        LRBStation:=TRBStation.Create;
        try
          GetRBStationByID(LRBStation,AReq.QueryFields.Values[PATH_PARAM_ID].Substring(3,12),GetAVRConfigIdx(AReq));
          LURL:=LRBStation.RBSURL;
        finally
          LRBStation.Free;
        end;
      end;
  end;
  if LURL<>'' then
    begin
      ARes.SetCustomHeader(HTTP_HEADER_LOCATION,LURL);
      LCode:=HTTPCodeRedirect;
    end
  else
    LCode:=HTTP_CODE_NOT_FOUND;
  ServerResponse(LCode,ctNone,ARes,'');
end;

//In some cases used to play station.
procedure GetRadioBrowserSearchedStations(AReq: TRequest; ARes: TResponse);
var
  LRBStations: TRBStations;
begin
  try
    if AReq.QueryFields.Values[PATH_PARAM_SSEARCH_TYPE]='3' then    // In some cases "3" mean a "Play" option.
      begin
        AReq.QueryFields.AddPair(PATH_PARAM_ID,AReq.QueryFields.Values[PATH_PARAM_SEARCH]);
        GetStation(AReq,ARes);
      end
    else                                                            // Probably valid for "2" only.
      begin
        Logging(ltDebug, AReq.Method+' '+AReq.URI);
        if AReq.QueryFields.Values[PATH_PARAM_SEARCH].Length<3 then
          DisplayMessage(MSG_QUERY_TOO_SHORT,ARes)
        else
          begin
            LRBStations:=TRBStations.Create;
            try
              GetRadioBrowserStations(LRBStations,GetStationsByCategory(LRBStations,rbctSearch,AReq.QueryFields.Values[PATH_PARAM_SEARCH],AReq),AReq,ARes);
            finally
              LRBStations.Free;
            end;
          end;
      end;
  except
    on E: Exception do
      Logging(ltError, 'Search error: '+E.Message);
  end;
end;

procedure GetAbout(AReq: TRequest; ARes: TResponse);
var
  LMyPage: TVTunerPage;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LMyPage:=TVTunerPage.Create;
  try
    LMyPage.TotalItemsCount:=2;
    LMyPage.Add(SetVTunerDirectory('Welcome to '+APP_NAME+' v'+APP_VERSION,PATH_ROOT+'/'+PATH_EMPTY,1,-1));
    LMyPage.Add(SetVTunerDirectory(APP_COPYRIGHT,PATH_ROOT+'/'+PATH_EMPTY,1,-1));
    SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
  finally
    LMyPage.Free;
  end;
end;

procedure GetEmpty(AReq: TRequest; ARes: TResponse);
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  DisplayMessage(MSG_EMPTY_FOLDER,ARes);
end;

procedure VTunerRedirect(AReq: TRequest; ARes: TResponse);
var
  LHTTP_Code: integer = HTTP_CODE_NOT_FOUND;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  with TLocalHttpClient.Create(1024) do
    try
      AllowRedirect:=False;
      KeepConnection:=True;
      AddHeader(HTTP_HEADER_ACCEPT,WEBBROWSER_HTTP_HEADER_ACCEPT);
      AddHeader(HTTP_HEADER_ACCEPT_ENCODING, WEBBROWSER_HTTP_HEADER_ACCEPT_ENCODING);
      AddHeader(HTTP_HEADER_ACCEPT_LANGUAGE, WEBBROWSER_HTTP_HEADER_ACCEPT_LANGUAGE);
      AddHeader(HTTP_HEADER_CACHE_CONTROL, WEBBROWSER_HTTP_HEADER_CACHE_CONTROL);
      AddHeader(HTTP_HEADER_CONNECTION, WEBBROWSER_HTTP_HEADER_CONNECTION);
      AddHeader(HTTP_HEADER_PRAGMA, WEBBROWSER_HTTP_HEADER_PRAGMA);
      AddHeader(HTTP_HEADER_UPGRADE_INSECURE_REQUESTS, '1');
      AddHeader(HTTP_HEADER_USER_AGENT,WEBBROWSER_HTTP_HEADER_USER_AGENT);
      try
        Get('http://'+AReq.Host+AReq.URL);
      except end;
      case ResponseStatusCode of
        301,302,303,307,308 : if (ResponseHeaders.IndexOfName('Location')>=0) then
                                begin
                                  LHTTP_Code:=302;
                                  ARes.SetCustomHeader(HTTP_HEADER_LOCATION,Trim(ResponseHeaders.Values['Location']));
                                  Logging(ltDebug, 'vTuner radio station link - redirect to:'+ResponseHeaders.Values['Location']);
                                end;
      end;
    finally
      Free;
    end;
  ServerResponse(LHTTP_Code,ctNone,ARes,'');
end;

// END - Registered routes routines

// BEGIN - Service routines
function GetRange(var AStart, AHowMany: integer; AQueryFields: TStrings): boolean;
begin
  Result:=False;
  AStart:=0;
  AHowMany:=0;
  if TryStrToInt(AQueryFields.Values['start'],AStart) and TryStrToInt(AQueryFields.Values['howmany'],AHowMany) then
    Result:=True
  else
    begin
      if TryStrToInt(AQueryFields.Values['startitems'],AStart) and TryStrToInt(AQueryFields.Values['enditems'],AHowMany) then
        begin
          Result:=True;
          if AHowMany<AStart then
            begin
              Logging(ltDebug, 'Index of first element ('+IntToStr(AStart)+') greater than the index of the last one ('+IntToStr(AHowMany)+')?');
              AHowMany:=1;
            end
          else
            AHowMany:=AHowMany-AStart+1;
        end
      else
        AHowMany:=-1;
    end;
  Dec(AStart);
  if AStart<0 then AStart:=0;
end;

procedure GetPageRange(var AFirstElement, ALastElement: integer; AArrayLen: integer; AQueryFields: TStrings);
begin
  if TryStrToInt(AQueryFields.Values['start'],AFirstElement) and TryStrToInt(AQueryFields.Values['howmany'],ALastElement) then
    ALastElement:=AFirstElement+ALastElement-1
  else
    begin
      TryStrToInt(AQueryFields.Values['startitems'],AFirstElement);
      TryStrToInt(AQueryFields.Values['enditems'],ALastElement);
      if ALastElement<AFirstElement then
        begin
          Logging(ltDebug, 'Index of first element ('+IntToStr(AFirstElement)+') greater than the index of the last one ('+IntToStr(ALastElement)+')?');
          ALastElement:=AFirstElement;
        end;
    end;
  Dec(AFirstElement);
  Dec(ALastElement);
  if AFirstElement<0 then
    begin
      Logging(ltDebug, 'First element of page with index: "'+IntToStr(AFirstElement)+'"?');
      AFirstElement:=0;
    end;
  if ALastElement>AArrayLen-1 then
    ALastElement:=AArrayLen-1
  else
    if ALastElement<0 then
      begin
        Logging(ltDebug, 'Last element of page with index: "'+IntToStr(ALastElement)+'"');
        ALastElement:=AArrayLen-1;  // Changed from 0 value due to this issue : https://github.com/coffeegreg/YTuner/issues/28
      end;
end;

function GetStationInfo(AReq: TRequest; AMyPage: TVTunerPage): string;
var
  LMSStation: TMSStation;
  LRBStation: TRBStation;
  LAVRConfigIdx: integer;
begin
  Result:='';
  case AReq.QueryFields.Values[PATH_PARAM_ID].Substring(0,2) of
    MY_STATIONS_PREFIX:
      begin
        LMSStation:=GetMyStationByID(AReq.QueryFields.Values[PATH_PARAM_ID]);
        if LMSStation.Station.MSID<>'' then
          begin
            AMyPage.TotalItemsCount:=1;
            AMyPage.Add(SetVTunerStation(LMSStation,AReq));
            Result:=LMSStation.Station.MSID;
          end;
      end;
    RADIOBROWSER_PREFIX:
      begin
        LRBStation:=TRBStation.Create;
        try
          LAVRConfigIdx:=GetAVRConfigIdx(AReq);
          GetRBStationByID(LRBStation,AReq.QueryFields.Values[PATH_PARAM_ID].Substring(3,12),LAVRConfigIdx);
          if LRBStation.RBSID<>'' then
            begin
              AMyPage.TotalItemsCount:=1;
              AMyPage.Add(SetVTunerStation(LRBStation,AReq,LAVRConfigIdx));
              Result:=LRBStation.RBSID;
            end;
        finally
          LRBStation.Free;
        end;
      end;
  end;
end;

procedure GetRadioBrowserStations(ARBStations: TRBStations; ATotalCount: integer; AReq: TRequest; ARes: TResponse);
var
  LMyPage: TVTunerPage;
  LAVRConfigIdx, i: integer;
begin
  if ATotalCount>0 then
    begin
      LAVRConfigIdx:=GetAVRConfigIdx(AReq);
      LMyPage:=TVTunerPage.Create;
      try
        LMyPage.TotalItemsCount:=ATotalCount;
        for i:=0 to ARBStations.Count-1 do
          LMyPage.Add(SetVTunerStation(TRBStation(ARBStations.Objects[i]),AReq,LAVRConfigIdx));
        SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
      finally
        LMyPage.Free;
      end;
    end
  else
    begin
      Logging(ltWarning, MSG_NO_STATIONS_FOUND+ifThen(AReq.RouteParams[PATH_CATEGORY]='','',' : '+AReq.RouteParams[PATH_CATEGORY]) );
      DisplayMessage(MSG_NO_STATIONS_FOUND,ARes);
    end;
end;
// END - Service routines

// BEGIN - Server response routines
procedure ServerResponse(AResponseCode: integer; AResponseContentType: TResponseContentType; ARes: TResponse; AData: TStream);
begin
  with ARes do
    begin
      SetCustomHeader(HTTP_HEADER_SERVER,YTUNER_USER_AGENT+'/'+APP_VERSION);
      Code:=AResponseCode;
      ContentType:=HTTP_RESPONSE_CONTENT_TYPE[AResponseContentType];
      ContentStream:=AData;
      ContentLength:=ContentStream.Size;
      SendContent;
    end;
end;

procedure ServerResponse(AResponseCode: integer; AResponseContentType: TResponseContentType; ARes: TResponse; AData: string);
begin
  with ARes do
    begin
      SetCustomHeader(HTTP_HEADER_SERVER,YTUNER_USER_AGENT+'/'+APP_VERSION);
      Code:=AResponseCode;
      ContentType:=HTTP_RESPONSE_CONTENT_TYPE[AResponseContentType];
      Content:=AData;
      ContentLength:=Length(Content);
      SendContent;
    end;
end;

procedure SendPageResponse(AResponseCode: integer; AResponseContentType: TResponseContentType; ARes: TResponse; AMyPage: TVTunerPage);
var
  LStream: TStream;
begin
  LStream:=TMemoryStream.Create;
  try
    AMyPage.ToXMLStream(LStream);
    ServerResponse(AResponseCode,AResponseContentType,ARes,LStream);
  finally
    FreeAndNil(LStream);
  end;
end;
// END - Server response routines

// BEGIN - vTuner structures routines
function SetVTunerStation(AStation: TMSStation; AReq: TRequest): TVTunerStation;
begin
  Result:=TVTunerStation.Create;
  with Result, AStation do
    begin
      UID:=Station.MSID;
      Name:=Station.MSName;
      Description:='My favorite "'+Station.MSName+'"';
      URL:=StripHttps(Station.MSURL,AReq);
      Icon:=PATH_HTTP+URLHost+'/'+PATH_ROOT+'/'+PATH_ICON+'?'+PATH_PARAM_ID+'='+Station.MSID;
      Genre:=Category;
      Bookmark:=PATH_HTTP+URLHost+'/'+PATH_SETUPAPP+'/'+PATH_FAVXML_ASP+'?'+PATH_PARAM_ID+'='+Station.MSID+'&'+PATH_FAVACTION+'='+PATH_FAVACTION_ADD;
    end;
end;

function SetVTunerStation(ARBStation: TRBStation; AReq: TRequest; ATranslatorIdx: integer): TVTunerStation;
begin
  Result:=TVTunerStation.Create;
  with Result, ARBStation, AVRConfigArray[ATranslatorIdx].Translator do
    begin
      UID:=RADIOBROWSER_PREFIX+'_'+RBSID.Replace('-','').Substring(0,12).ToUpper;
      Name:=RBSName;
      Location:=RBSCountry;
      if Enabled then
        Location:=Translate(Location, ATranslatorIdx);
      if ReplaceUTF8Latin1Supplement or ReplaceUTF8Latin1ExtendedA or ReplaceUTF8Latin1ExtendedB then
        begin
          Name:=ReplaceDiacritics(Name, ATranslatorIdx);
          Location:=ReplaceDiacritics(Location, ATranslatorIdx);
        end;
      Description:=Name+' : '+RBSHomePageURL;
      URL:=StripHttps(RBSURL,AReq);
      if RBSTags='' then
        Genre:=AReq.RouteParams[PATH_CATEGORY]
      else
        Genre:=RBSTags;
      Mime:=RBSCodec.ToUpper;
      Bitrate:=RBSBitrate;
      Icon:=PATH_HTTP+URLHost+'/'+PATH_ROOT+'/'+PATH_ICON+'?'+PATH_PARAM_ID+'='+UID;
      Bookmark:=PATH_HTTP+URLHost+'/'+PATH_SETUPAPP+'/'+PATH_FAVXML_ASP+'?'+PATH_PARAM_ID+'='+UID+'&'+PATH_FAVACTION+'='+PATH_FAVACTION_ADD;
    end;
end;

function SetVTunerDirectory(ATitle, ADestination: string; AItemCount: integer; ATranslatorIdx: integer): TVTunerDirectory;
begin
  Result:=TVTunerDirectory.Create;
  with Result do
    begin
      Title:=ATitle;
      if ATranslatorIdx>=0 then
        with AVRConfigArray[ATranslatorIdx].Translator do
          begin
            if Enabled then
              Title:=Translate(Title, ATranslatorIdx);
            if ReplaceUTF8Latin1Supplement or ReplaceUTF8Latin1ExtendedA or ReplaceUTF8Latin1ExtendedB then
              Title:=ReplaceDiacritics(Title, ATranslatorIdx);
          end;
      Destination:=PATH_HTTP+URLHost+'/'+ADestination;
      ItemCount:=AItemCount;
    end;
end;

function SetVTunerDisplay(AMessage: string): TVTunerDisplay;
begin
  Result:=TVTunerDisplay.Create;
  Result.Message:=AMessage;
end;
// END - vTuner structures routines

// BEGIN - Display AVR messsages
procedure DisplayMessage(AMessage: string; var ARes: TResponse);
var
  LMyPage: TVTunerPage;
begin
  LMyPage:=TVTunerPage.Create;
  try
    LMyPage.Add(SetVTunerDisplay(AMessage));
    LMyPage.TotalItemsCount:=1;
    SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
  finally
    LMyPage.Free;
  end;
end;
// END - Display AVR messsages

end.

