unit httpserver;

// Retuner: Web serwer unit.

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
  Classes, SysUtils, StrUtils, Math, base64, syncobjs,
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
  common, vtuner, my_stations, podcasts, radiobrowser, bookmark, avr, translator, relayserver, dnsserver;

type
  EIconTooLarge = class(Exception);

// Every image reader announces the size it is about to decode by calling
// SetSize, and TFPMemoryImage allocates there and then - before a single pixel
// of the file has been read, and from numbers that came out of the file's
// header. A header is a claim, not a measurement, and this is the one place
// every format's reader passes through. See script/fuzz-test.sh, where an
// 8000x8000 PNG of a few hundred bytes took the process to half a gigabyte.
  TCappedImage = class(TFPMemoryImage)
  public
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

const
  WEB_SERVICE = 'Web Service';
  PATH_HTTP = 'http://';
  PATH_SETUPAPP = 'setupapp';
  PATH_STATXML_ASP = 'statxml.asp';
  PATH_LOGINXML_ASP = 'loginxml.asp';
  PATH_FAVXML_ASP = 'favxml.asp';
  PATH_NAVXML_ASP = 'navxml.asp';
  PATH_SEARCH_ASP = 'search.asp';
  PATH_ROOT = 'viritin';
// Bookmarks an AVR saved before the renames contain absolute /retuner/ or /ytuner/ URLs,
// and the receiver replays them verbatim. Every root route is therefore registered
// under these prefixes as well -- see RegisterRootRoutes. Served, never generated.
  PATH_ROOT_LEGACY = 'retuner';
  PATH_ROOT_LEGACY_YTUNER = 'ytuner';
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
  MSG_PODCASTS_NONE = 'No podcast feeds configured';
  MSG_PODCASTS_NO_FEED = 'Podcast feed not found';
  MSG_PODCASTS_NO_EPISODES = 'No episodes found';
  MSG_FOR_THIS_CATEGORY = ' for this category';
  MSG_UNKNOWN_ROUTE = 'Unknown route';
  MSG_EMPTY_FOLDER = 'This folder is intentionally empty...';

  VTUNER_HOST = 'radio567.vtuner.com';

  WEBSERVER_PORT = 80;

{$I placeholder-icon.inc}

var
  WebServerIPAddress: string;
  WebServerPort: integer = WEBSERVER_PORT;
  IconSize: integer = ICON_SIZE;
  IconCache: boolean = ICON_CACHE;
  IconPlaceholder: boolean = ICON_PLACEHOLDER;
  IconExtension: string = '';
  HTTPCodeRedirect: integer = HTTP_CODE_REDIRECT;

procedure RegisterServerRoutes;

// Registered routes routines
procedure DefaultPage(AReq: TRequest; ARes: TResponse);
procedure SetupAppLoginXMLPage(AReq: TRequest; ARes: TResponse);
procedure GetStation(AReq: TRequest; ARes: TResponse);
procedure BookmarkService(AReq: TRequest; ARes: TResponse);
procedure GetMyStationsCategories(AReq: TRequest; ARes: TResponse);
procedure GetMyStationsOfCategory(AReq: TRequest; ARes: TResponse);
procedure GetPodcastFeedList(AReq: TRequest; ARes: TResponse);
procedure GetPodcastFeedEpisodes(AReq: TRequest; ARes: TResponse);
procedure GetIcon(AReq: TRequest; ARes: TResponse);
procedure GetRadioBrowserRootDirectory(AReq: TRequest; ARes: TResponse);
procedure GetRadioBrowserCategoryType(AReq: TRequest; ARes: TResponse);
procedure GetRadioBrowserCategoryStations(AReq: TRequest; ARes: TResponse);
procedure GetBookmarkStations(AReq: TRequest; ARes: TResponse);
procedure PlayStation(AReq: TRequest; ARes: TResponse);
procedure GetSearchedStations(AReq: TRequest; ARes: TResponse);
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
function SetVTunerStation(AEpisode: TPodcastEpisode; const AFeedName: string; AReq: TRequest): TVTunerStation;
function SetVTunerDirectory(ATitle, ADestination: string; AItemCount: integer; ATranslatorIdx: integer): TVTunerDirectory;
function SetVTunerDisplay(AMessage: string): TVTunerDisplay;

// Display AVR messsages
procedure DisplayMessage(AMessage: string; var ARes: TResponse);

// Station lookup for the relay listener, which has no access to these units.
function ResolveRelayStationURL(const AID: string): string;

implementation

procedure RegisterServerRoutes;

// Every route under the application root, registered under a given prefix.
// Called once for PATH_ROOT and once for PATH_ROOT_LEGACY: a bookmark stored by
// an AVR before the rename holds an absolute /ytuner/ URL and is replayed as-is,
// so dropping the old prefix would break every bookmark already saved.
  procedure RegisterRootRoutes(const APrefix: string);
  begin
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_MY_STATIONS, @GetMyStationsCategories, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_MY_STATIONS+'/:'+PATH_CATEGORY, @GetMyStationsOfCategory, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_PODCASTS, @GetPodcastFeedList, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_PODCASTS+'/:'+PATH_CATEGORY, @GetPodcastFeedEpisodes, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_ICON+IconExtension, @GetIcon, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_RADIOBROWSER, @GetRadioBrowserRootDirectory, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_RADIOBROWSER+'/:'+PATH_CATEGORY_TYPE, @GetRadioBrowserCategoryType, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_RADIOBROWSER+'/:'+PATH_CATEGORY_TYPE+'/:'+PATH_CATEGORY, @GetRadioBrowserCategoryStations, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_BOOKMARK, @GetBookmarkStations, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_ABOUT, @GetAbout, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_EMPTY, @GetEmpty, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_PLAY, @PlayStation, false);
    HTTPRouter.RegisterRoute('/'+APrefix+'/'+PATH_SEARCH, @GetSearchedStations, false);
  end;

begin
  HTTPRouter.RegisterRoute('/', @DefaultPage, true);
  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_LOGINXML_ASP, @SetupAppLoginXMLPage, false);
  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_STATXML_ASP, @GetStation, false);
  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/'+PATH_FAVXML_ASP, @BookmarkService, false);
  RegisterRootRoutes(PATH_ROOT);
  RegisterRootRoutes(PATH_ROOT_LEGACY);
  RegisterRootRoutes(PATH_ROOT_LEGACY_YTUNER);

// This endpoint is for redirect for vTuner radio station link
  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_VTUNER_ASP, @VTunerRedirect, false);

///////////////////////////////////////////////
// Group of routes NOT tested with real AVR. //
///////////////////////////////////////////////

//Play station ?

//Search stations?

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

  HTTPRouter.RegisterRoute('/'+PATH_SETUPAPP+'/*/'+PATH_SEARCH_ASP, @GetSearchedStations, false);

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
                LMyPage.Add(SetVTunerDirectory(AVRConfigArray[AAAVRConfigIdx].MainMenuItems[LLIdx].MLabel,PATH_ROOT+'/'+PATH_MY_STATIONS,Length(GetMyStationsSnapshot),-1));
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
// Skips the country level of Radio Browser and lands directly on the stations
// of the configured country. Nothing to show if no country is set.
          AVR_MAINMENU_IDENTIFIER_LOCAL:
            if RadiobrowserEnabled and (not LocalCountry.IsEmpty) then
              begin
                LMyPage.Add(SetVTunerDirectory(AVRConfigArray[AAAVRConfigIdx].MainMenuItems[LLIdx].MLabel,
                            PATH_ROOT+'/'+PATH_RADIOBROWSER+'/'+PATH_RADIOBROWSER_COUNTRY+'/'+URLEncode(LocalCountry),
                            RBPopularAndSearchStationsLimit,-1));
                LIdx:=LIdx+1;
              end;
// Hidden unless podcasts are switched on and at least one feed loaded, so the
// entry never leads to an empty folder.
          AVR_MAINMENU_IDENTIFIER_PODCASTS:
            if PodcastsEnabled and (Length(GetPodcastFeeds)>0) then
              begin
                LMyPage.Add(SetVTunerDirectory(AVRConfigArray[AAAVRConfigIdx].MainMenuItems[LLIdx].MLabel,
                            PATH_ROOT+'/'+PATH_PODCASTS,Length(GetPodcastFeeds),-1));
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
// This is the AVR's entry point, so it is where we learn the address a device
// actually reaches us from. On a public host that address is then allowed to
// have its station lookups forwarded, which is what makes pointing an AVR's DNS
// setting at this server enough on its own.
  DNSNoteWebClient(AReq.RemoteAddress);
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
// An id that is missing or shorter than three characters matched no branch and
// the handler returned without ever writing a response, leaving the AVR to sit
// on the request until it timed out.
  else
    begin
      Logging(ltError, MSG_NO_STATIONS_FOUND+' with ID: '+AReq.QueryFields.Values[PATH_PARAM_ID]+'');
      DisplayMessage(MSG_NO_STATIONS_FOUND,ARes);
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
  LStations: TMyStationsGroup;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LStations:=GetMyStationsSnapshot;
  GetPageRange(LFirstElement,LLastElement,Length(LStations),AReq.QueryFields);
  LMyPage:=TVTunerPage.Create;
  LMyPage.TotalItemsCount:=Length(LStations);
  try
    if Length(LStations)>0 then
      begin
        for i:=LFirstElement to LLastElement do
          with LStations[i] do
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
  LStations: TMyStationsGroup;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LStations:=GetMyStationsSnapshot;
  LMSStation.Category:=HTTPDecode(AReq.RouteParams[PATH_CATEGORY]);
// An empty station list used to reach MyStations[0] below and raise a range
// error; an unknown category fell through without answering at all, leaving
// the AVR to time out.
  if Length(LStations)=0 then
    begin
      Logging(ltError, MSG_NO_STATIONS_FOUND);
      DisplayMessage(MSG_NO_STATIONS_FOUND,ARes);
      Exit;
    end;
  while (LCategoryIdx<Length(LStations)-1) and (LStations[LCategoryIdx].MSCategory.ToLower<>LMSStation.Category.ToLower) do
    Inc(LCategoryIdx);
  if LStations[LCategoryIdx].MSCategory.ToLower=LMSStation.Category.ToLower then
    begin
      GetPageRange(LFirstElement,LLastElement,Length(LStations[LCategoryIdx].MSStations),AReq.QueryFields);
      LMyPage:=TVTunerPage.Create;
      LMyPage.TotalItemsCount:=Length(LStations[LCategoryIdx].MSStations);
      try
        if Length(LStations[LCategoryIdx].MSStations)>0 then
          begin
            for i:=LFirstElement to LLastElement do
              begin
                LMSStation.Station:=LStations[LCategoryIdx].MSStations[i];
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
    end
  else
    begin
      Logging(ltError, MSG_NO_STATIONS_FOUND+' : '+LMSStation.Category);
      DisplayMessage(MSG_NO_STATIONS_FOUND+' : '+LMSStation.Category,ARes);
    end;
end;

// The feed list. Each feed is a directory; its episodes are one level down,
// addressed by the feed's id rather than its name so a title with a slash or a
// non-Latin character cannot break the path.
procedure GetPodcastFeedList(AReq: TRequest; ARes: TResponse);
var
  i: integer;
  LMyPage: TVTunerPage;
  LFeeds: TPodcastFeeds;
  LFirstElement: integer = 0;
  LLastElement: integer = 0;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LFeeds:=GetPodcastFeeds;
  if Length(LFeeds)=0 then
    begin
      Logging(ltWarning, MSG_PODCASTS_NONE);
      DisplayMessage(MSG_PODCASTS_NONE,ARes);
      Exit;
    end;
  GetPageRange(LFirstElement,LLastElement,Length(LFeeds),AReq.QueryFields);
  LMyPage:=TVTunerPage.Create;
  LMyPage.TotalItemsCount:=Length(LFeeds);
  try
    for i:=LFirstElement to LLastElement do
      LMyPage.Add(SetVTunerDirectory(LFeeds[i].PFName,
                  PATH_ROOT+'/'+PATH_PODCASTS+'/'+LFeeds[i].PFID,PodcastEpisodesLimit,-1));
    SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
  finally
    LMyPage.Free;
  end;
end;

// The episodes of one feed. Fetching happens here rather than while the feed
// list is drawn: listing every feed would otherwise mean downloading every one
// of them before the first screen could be sent.
procedure GetPodcastFeedEpisodes(AReq: TRequest; ARes: TResponse);
var
  i: integer;
  LMyPage: TVTunerPage;
  LFeed: TPodcastFeed;
  LEpisodes: TPodcastEpisodes;
  LFirstElement: integer = 0;
  LLastElement: integer = 0;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  if not GetPodcastFeedByID(HTTPDecode(AReq.RouteParams[PATH_CATEGORY]),LFeed) then
    begin
      Logging(ltError, string.Join(' ',[MSG_PODCASTS_NO_FEED,AReq.RouteParams[PATH_CATEGORY]]));
      DisplayMessage(MSG_PODCASTS_NO_FEED,ARes);
      Exit;
    end;
  if not GetPodcastEpisodes(LFeed,LEpisodes) then
    begin
      Logging(ltError, string.Join(' ',[MSG_PODCASTS_NO_EPISODES,LFeed.PFName]));
      DisplayMessage(MSG_PODCASTS_NO_EPISODES,ARes);
      Exit;
    end;
  GetPageRange(LFirstElement,LLastElement,Length(LEpisodes),AReq.QueryFields);
  LMyPage:=TVTunerPage.Create;
  LMyPage.TotalItemsCount:=Length(LEpisodes);
  try
    for i:=LFirstElement to LLastElement do
      LMyPage.Add(SetVTunerStation(LEpisodes[i],LFeed.PFName,AReq));
    SendPageResponse(HTTP_CODE_OK,ctXML,ARes,LMyPage);
  finally
    LMyPage.Free;
  end;
end;

procedure TCappedImage.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth<0) or (AHeight<0)
    or (AWidth>ICON_MAX_EDGE) or (AHeight>ICON_MAX_EDGE)
    or (Int64(AWidth)*Int64(AHeight)>ICON_MAX_PIXELS) then
    raise EIconTooLarge.Create('image header claims '+AWidth.ToString+'x'+AHeight.ToString
                              +', over the icon limit of '+ICON_MAX_EDGE.ToString+' per side and '
                              +ICON_MAX_PIXELS.ToString+' pixels');
  inherited SetSize(AWidth,AHeight);
end;

// The icon id becomes a file name under the cache directory, so it decides both
// which file is read and which is written. Ids this server issues are a two
// letter prefix, an underscore and hex; anything else was never an id.
// Refused, not sanitised: '../secret.txt' is not a request to be repaired into
// a different one. Until this existed, GET /retuner/icon?id=../secret.txt
// returned that file's bytes with an image content type, to anyone who asked.
function IsSafeIconKey(const AKey: string): boolean;
var
  LCh: char;
begin
  Result:=False;
  if (AKey='') or (Length(AKey)>ICON_KEY_MAX_LENGTH) then
    Exit;
  for LCh in AKey do
    if not (LCh in ['A'..'Z','a'..'z','0'..'9','_','-']) then
      Exit;
  Result:=True;
end;

// A receiver asks for a station logo with HEAD before it will GET one, so a 404
// here is the end of it: no art, and nothing in the log that looks like a
// fault. Most radio-browser stations carry no favicon at all, which makes that
// the common case rather than the exceptional one - a real Denon issued exactly
// one icon request in a whole session of browsing, took the 404, and never
// asked again.
//
// Serving the Retuner mark instead answers the HEAD with a length, so the
// receiver goes on to fetch it. It is rendered once, at the configured icon
// size, because it is the same bytes for every station that has no logo.
var
  PlaceholderLock: TCriticalSection;
  PlaceholderBytes: string = '';
  PlaceholderTried: boolean = False;

function PlaceholderIcon: string;
var
  LPng, LOut: TMemoryStream;
  LImage: TFPMemoryImage;
  LReader: TFPReaderPNG;
  LWriter: TFPCustomImageWriter;
  LRaw: string;
begin
  PlaceholderLock.Enter;
  try
    Result:=PlaceholderBytes;
    if PlaceholderTried then
      Exit;
    PlaceholderTried:=True;
    try
      LRaw:=DecodeStringBase64(PLACEHOLDER_ICON_PNG_BASE64);
      LPng:=TMemoryStream.Create;
      try
        LPng.Write(LRaw[1],Length(LRaw));
        LPng.Position:=0;
        LImage:=TFPMemoryImage.Create(0,0);
        try
          LImage.UsePalette:=False;
          LReader:=TFPReaderPNG.Create;
          try
            LImage.LoadFromStream(LPng,LReader);
          finally
            LReader.Free;
          end;
          {$IFDEF WRITERJPG}
          LWriter:=TFPWriterJPEG.Create;
          {$ELSE}
          LWriter:=TFPWriterPNG.Create;
          {$ENDIF}
          try
            LOut:=TMemoryStream.Create;
            try
// Scaled the same way every other logo is, so a receiver that was given 75 in
// IconSize gets 75 here too.
              with TFPImageCanvas.Create(TFPMemoryImage.Create(IconSize,IconSize)) do
                try
                  Image.UsePalette:=False;
                  StretchDraw(0,0,IconSize,IconSize,LImage);
                  Image.SaveToStream(LOut,LWriter);
                finally
                  Image.Free;
                  Free;
                end;
              SetLength(PlaceholderBytes,LOut.Size);
              LOut.Position:=0;
              if LOut.Size>0 then
                LOut.Read(PlaceholderBytes[1],LOut.Size);
            finally
              LOut.Free;
            end;
          finally
            LWriter.Free;
          end;
        finally
          LImage.Free;
        end;
      finally
        LPng.Free;
      end;
      Logging(ltDebug, 'Placeholder icon rendered at '+IconSize.ToString+'px, '
        +Length(PlaceholderBytes).ToString+' bytes');
    except
      on E : Exception do
        begin
          PlaceholderBytes:='';
          Logging(ltError, 'Cannot render the placeholder icon ('+E.Message+')');
        end;
    end;
    Result:=PlaceholderBytes;
  finally
    PlaceholderLock.Leave;
  end;
end;

// Answer with the mark rather than a 404, when there is one to answer with.
function ServedPlaceholder(ARes: TResponse): boolean;
var
  LBytes: string;
begin
  Result:=False;
  if not IconPlaceholder then
    Exit;
  LBytes:=PlaceholderIcon;
  if LBytes='' then
    Exit;
  {$IFDEF WRITERJPG}
  ServerResponse(HTTP_CODE_OK,ctJPG,ARes,LBytes);
  {$ELSE}
  ServerResponse(HTTP_CODE_OK,ctPNG,ARes,LBytes);
  {$ENDIF}
  Result:=True;
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
  LNewWidth, LNewHeight: integer;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LImageFile:=AReq.QueryFields.Values[PATH_PARAM_ID];
  if not IsSafeIconKey(LImageFile) then
    begin
      Logging(ltWarning, 'Icon id refused, it is not an id');
      ServerResponse(HTTP_CODE_NOT_FOUND,ctNone,ARes,'');
      Exit;
    end;
// An image decoder handed a hostile file is the one place in this server where
// a stranger picks the input to a C-shaped parser. A failure there must cost
// the request, not the process: before this, a logo whose scaled height rounded
// to zero raised out of the handler thread and ended the program.
  try
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
            PODCASTS_PREFIX: LURL:=GetPodcastLogoForEpisode(LImageFile);
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
  // TLocalHttpClient hangs up once the body passes its ceiling. Without one, the
  // only limit on what a logo URL could stream into memory was the sender's
  // patience.
              with TLocalHttpClient.Create(ICON_MAX_BYTES) do
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
                  AddHeader(HTTP_HEADER_USER_AGENT,RETUNER_USER_AGENT+'/'+APP_VERSION);
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
                      LImageIn:=TCappedImage.Create(0,0);
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
  // The image is decoded now, so drop the downloaded bytes before re-encoding.
  // Writing at the stream's end would otherwise leave source and converted image
  // concatenated, which is what reaches the AVR and the icon cache.
                            LStream.Size:=0;
                            if (Width>IconSize) or (Height>IconSize) then
                              begin
                                if Width>=Height then
                                  LScaleFactor:=IconSize/Width
                                else
                                  LScaleFactor:=IconSize/Height;
  // A 3000x2 banner scaled to fit 200 pixels rounds its height to zero, and the
  // JPEG writer given an image with no height printed "Empty JPEG image (DNL not
  // supported)" and took the process down with an access violation - from a
  // station logo, over the network. A side is at least one pixel.
                                LNewWidth:=Max(1,Round(LImageIn.Width*LScaleFactor));
                                LNewHeight:=Max(1,Round(LImageIn.Height*LScaleFactor));

                                with TFPImageCanvas.Create(TFPMemoryImage.Create(LNewWidth,LNewHeight)) do
                                  try
                                    Image.UsePalette:=false;
                                    StretchDraw(0,0,LNewWidth,LNewHeight,LImageIn);
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
  // A failed conversion leaves the stream empty; serving or caching that would
  // hand the AVR a 0-byte icon and poison the cache with it.
                      if LStream.Size>0 then
                        begin
                          LStream.Position:=0;
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
  except
    on E: Exception do
      Logging(ltError, 'Icon request aborted: '+E.ClassName+' ('+E.Message+')');
  end;
  if not ARes.ContentSent then
    begin
      if LURL <> '' then
        Logging(ltWarning, 'Cannot process image from "'+LURL+'"');
      if not ServedPlaceholder(ARes) then
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
  LEpisode: TPodcastEpisode;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  case AReq.QueryFields.Values[PATH_PARAM_ID].Substring(0,2) of
    MY_STATIONS_PREFIX:
      with GetMyStationByID(AReq.QueryFields.Values[PATH_PARAM_ID]) do
        if Station.MSID<>'' then
          LURL:=StripHttps(Station.MSURL,AReq);
    PODCASTS_PREFIX:
      if GetPodcastEpisodeByID(AReq.QueryFields.Values[PATH_PARAM_ID],LEpisode) then
        LURL:=StripHttps(LEpisode.PEURL,AReq);
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
// Unwrap here rather than when the listing is built: a category page would
// otherwise have to fetch a playlist per station before it could answer.
      if ResolvePlaylists and LooksLikePlaylist(LURL) then
        LURL:=StripHttps(ResolveStreamURL(LURL),AReq);
      ARes.SetCustomHeader(HTTP_HEADER_LOCATION,LURL);
      LCode:=HTTPCodeRedirect;
    end
  else
    LCode:=HTTP_CODE_NOT_FOUND;
  ServerResponse(LCode,ctNone,ARes,'');
end;

//In some cases used to play station.
procedure GetSearchedStations(AReq: TRequest; ARes: TResponse);
var
  LRBStations: TRBStations;
  LDeviceID: string;
  LStations: TMyStationsGroup;
begin
  LStations:=GetMyStationsSnapshot;
  try
    if AReq.QueryFields.Values[PATH_PARAM_SSEARCH_TYPE]='3' then    // In some cases "3" mean a "Play" option.
      begin
        case AReq.QueryFields.Values[PATH_PARAM_SEARCH].Substring(0,2) of
          MY_STATIONS_PREFIX, RADIOBROWSER_PREFIX: AReq.QueryFields.AddPair(PATH_PARAM_ID,AReq.QueryFields.Values[PATH_PARAM_SEARCH]);
        else
          begin
            LDeviceID:=IfThen(AReq.QueryFields.IndexOfName(PATH_PARAM_MAC)>=0,AReq.QueryFields.Values[PATH_PARAM_MAC],'?');
            Logging(ltDebug, 'Device : '+LDeviceID+' -> '+AReq.Host);
            if  LDeviceID='?' then
              LDeviceID:=AReq.Host;
            if FileExists(ConfigPath+DirectorySeparator+LDeviceID+'.xml') then
              begin
                if AReq.QueryFields.IndexOfName(PATH_PARAM_MAC)<0 then
                  AReq.QueryFields.AddPair(PATH_PARAM_MAC,LDeviceID);
                AReq.QueryFields.AddPair(PATH_PARAM_ID,'UNB'+AReq.QueryFields.Values[PATH_PARAM_SEARCH]);
              end
            else
              if MyStationsEnabled and (Length(LStations)>0) and (Length(LStations[0].MSStations)>0) then
                AReq.QueryFields.AddPair(PATH_PARAM_ID,LStations[0].MSStations[0].MSID)
              else
                Logging(ltDebug, MSG_FIRST_STATION_NEEDED);
          end
        end;
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
  LStations: TMyStationsGroup;
begin
  Logging(ltDebug, AReq.Method+' '+AReq.URI);
  LStations:=GetMyStationsSnapshot;
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
        Get('http://'+VTUNER_HOST+AReq.URI);
      except end;
      case ResponseStatusCode of
        301,302,303,307,308 : if (ResponseHeaders.IndexOfName('Location')>=0) then
                                begin
                                  LHTTP_Code:=HTTPCodeRedirect;
                                  ARes.SetCustomHeader(HTTP_HEADER_LOCATION,Trim(ResponseHeaders.Values['Location']));
                                  Logging(ltDebug, MSG_VTUNER_ERROR_LINK1+ResponseHeaders.Values['Location']);
                                end;
        else
          begin
            Logging(ltDebug, MSG_VTUNER_ERROR_LINK2);
            Logging(ltDebug, MSG_FIRST_STATION_NEEDED);
            if MyStationsEnabled and (Length(LStations)>0) and (Length(LStations[0].MSStations)>0) then
              begin
                LHTTP_Code:=HTTPCodeRedirect;
                ARes.SetCustomHeader(HTTP_HEADER_LOCATION,StripHttps(LStations[0].MSStations[0].MSURL,AReq));
              end
            else
              Logging(ltDebug, MSG_FIRST_STATION_NEEDED);
          end;
      end;
    finally
      Free;
    end;
  Logging(ltDebug, MSG_REDIRECT_SUPPORT_NEEDED);
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
  LEpisode: TPodcastEpisode;
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
    PODCASTS_PREFIX:
      if GetPodcastEpisodeByID(AReq.QueryFields.Values[PATH_PARAM_ID],LEpisode) then
        begin
          AMyPage.TotalItemsCount:=1;
          AMyPage.Add(SetVTunerStation(LEpisode,LEpisode.PEDescription,AReq));
          Result:=LEpisode.PEID;
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
      SetCustomHeader(HTTP_HEADER_SERVER,RETUNER_USER_AGENT+'/'+APP_VERSION);
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
      SetCustomHeader(HTTP_HEADER_SERVER,RETUNER_USER_AGENT+'/'+APP_VERSION);
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
      if NeedsRelay(Station.MSURL) then
        URL:=RelayURLFor(UID)
      else
        begin
          URL:=StripHttps(Station.MSURL,AReq);
          if ResolvePlaylists and LooksLikePlaylist(URL) then
            URL:=PATH_HTTP+URLHost+'/'+PATH_ROOT+'/'+PATH_PLAY+'?'+PATH_PARAM_ID+'='+UID;
        end;

      Icon:=PATH_HTTP+URLHost+'/'+PATH_ROOT+'/'+PATH_ICON+IconExtension+'?'+PATH_PARAM_ID+'='+Station.MSID;
      Genre:=Category;
      Bookmark:=PATH_HTTP+URLHost+'/'+PATH_SETUPAPP+'/'+PATH_FAVXML_ASP+'?'+PATH_PARAM_ID+'='+Station.MSID+'&'+PATH_FAVACTION+'='+PATH_FAVACTION_ADD;
    end;
end;

// An episode is a station whose URL is the enclosure. Podcast enclosures are
// very often HTTPS-only, which is precisely what these receivers cannot fetch,
// so this is the listing where the relay earns its keep.
function SetVTunerStation(AEpisode: TPodcastEpisode; const AFeedName: string; AReq: TRequest): TVTunerStation;
begin
  Result:=TVTunerStation.Create;
  with Result, AEpisode do
    begin
      UID:=PEID;
      Name:=PETitle;
      Description:=AFeedName;
      if not PEDate.IsEmpty then
        Description:=AFeedName+' - '+PEDate;
      if NeedsRelay(PEURL) then
        URL:=RelayURLFor(UID)
      else
        URL:=StripHttps(PEURL,AReq);
      Icon:=PATH_HTTP+URLHost+'/'+PATH_ROOT+'/'+PATH_ICON+IconExtension+'?'+PATH_PARAM_ID+'='+UID;
      Genre:=AFeedName;
      Bookmark:=PATH_HTTP+URLHost+'/'+PATH_SETUPAPP+'/'+PATH_FAVXML_ASP+'?'+PATH_PARAM_ID+'='+UID+'&'+PATH_FAVACTION+'='+PATH_FAVACTION_ADD;
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
// An HTTPS-only station goes through the relay: rewriting the scheme, as
// all-as-http does, only works while the station still answers on port 80.
      if NeedsRelay(RBSURL) then
        URL:=RelayURLFor(UID)
      else
        begin
          URL:=StripHttps(RBSURL,AReq);
// Hand the AVR our own play endpoint so the playlist is unwrapped when the
// station is actually selected, not while the listing is being rendered.
          if ResolvePlaylists and LooksLikePlaylist(URL) then
            URL:=PATH_HTTP+URLHost+'/'+PATH_ROOT+'/'+PATH_PLAY+'?'+PATH_PARAM_ID+'='+UID;
        end;
      if RBSTags='' then
        Genre:=AReq.RouteParams[PATH_CATEGORY]
      else
        Genre:=RBSTags;
      Mime:=RBSCodec.ToUpper;
      Bitrate:=RBSBitrate;
      Icon:=PATH_HTTP+URLHost+'/'+PATH_ROOT+'/'+PATH_ICON+IconExtension+'?'+PATH_PARAM_ID+'='+UID;
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

// BEGIN - Relay support
function ResolveRelayStationURL(const AID: string): string;
var
  LRBStation: TRBStation;
  LEpisode: TPodcastEpisode;
begin
  Result:='';
  case AID.Substring(0,2) of
    MY_STATIONS_PREFIX:
      with GetMyStationByID(AID) do
        if Station.MSID<>'' then
          Result:=Station.MSURL;
    PODCASTS_PREFIX:
      if GetPodcastEpisodeByID(AID,LEpisode) then
        Result:=LEpisode.PEURL;
    RADIOBROWSER_PREFIX:
      begin
        LRBStation:=TRBStation.Create;
        try
          GetRBStationByID(LRBStation,AID.Substring(3,12),0);
          if LRBStation.RBSID<>'' then
            Result:=LRBStation.RBSURL;
        finally
          LRBStation.Free;
        end;
      end;
  end;
end;
// END - Relay support


initialization
  PlaceholderLock:=TCriticalSection.Create;

finalization
  PlaceholderLock.Free;

end.
