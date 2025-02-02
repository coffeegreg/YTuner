unit bookmark;

//Ytuner: AVR bookmark feature support unit.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  DOM, XMLRead, XMLWrite, httpdefs,
  vtuner, common, avr, translator;

const
  PATH_BOOKMARK = 'bookmark';
  PATH_FAVACTION = 'fav';
  PATH_FAVACTION_ADD = 'add';
  PATH_FAVACTION_DEL = 'del';
  BOOKMARK_STATIONS_LIMIT = 100;

function GetBookmarkFileName(AMAC: string): string;
function GetBookmarkItemsCount(AMAC: string): integer;
function GetBookmark(AReq: TRequest; AFirstElement, ALastElement: integer; AXMLStream: TStream): boolean;
function GetBookmarkStationInfo(AMAC, ASID: string; AXMLStream: TStream): boolean;
procedure SetBookmark(AMAC, AAction: string; ANode: TDOMNode);

var
  BookmarkEnabled: boolean = True;
  CommonBookmark: boolean = False;
  BookmarkStationsLimit: integer = BOOKMARK_STATIONS_LIMIT;

implementation

function GetBookmarkFileName(AMAC: string): string;
begin
  if CommonBookmark then AMAC:=PATH_BOOKMARK;
  Result:=ConfigPath+DirectorySeparator+AMAC+'.xml';
end;

function GetBookmarkItemsCount(AMAC: string): integer;
var
  LXMLBookmark: TXMLDocument = nil;
  LNode: TDOMNode;
  LItemCount: integer = 0;
begin
  AMAC:=GetBookmarkFileName(AMAC);
  try
    try
      if FileExists(AMAC) then
        begin
          ReadXMLFile(LXMLBookmark,AMAC);
          LNode:=LXMLBookmark.FirstChild;
          if (LNode.NodeName=VT_XML_LISTOFITEMS) and (LNode.FirstChild.NodeName=VT_XML_ITEMCOUNT) then
            TryStrToInt(LNode.FirstChild.FirstChild.NodeValue,LItemCount);
        end;
    except
      on E:Exception do
        Logging(ltError, string.Join(' ',[MSG_BOOKMARK,MSG_ERROR,'['+{$I %CURRENTROUTINE%}+']','('+E.Message+') ']));
    end;
  finally
    if Assigned(LXMLBookmark) then
      LXMLBookmark.Free;
    Result:=LItemCount;
  end;
end;

function GetBookmark(AReq: TRequest; AFirstElement, ALastElement: integer; AXMLStream: TStream): boolean;
var
  LXMLBookmark: TXMLDocument = nil;
  LXMLBookmarkPart: TXMLDocument = nil;
  LAVRConfigIdx, i: integer;
  LBookmarkFileName: string;
begin
  LBookmarkFileName:=GetBookmarkFileName(AReq.QueryFields.Values[PATH_PARAM_MAC]);
  LAVRConfigIdx:=GetAVRConfigIdx(AReq);
  LXMLBookmarkPart:=TXMLDocument.Create;
  try
    if FileExists(LBookmarkFileName) then
      begin
        try
          ReadXMLFile(LXMLBookmark,LBookmarkFileName);
          LXMLBookmarkPart.AppendChild(LXMLBookmarkPart.CreateElement(VT_XML_LISTOFITEMS));
          with LXMLBookmarkPart.DocumentElement do
            begin
              AppendChild(LXMLBookmarkPart.ImportNode(LXMLBookmark.FirstChild.ChildNodes[0],true));
              for i:=AFirstElement to ALastElement do
                begin
                  AppendChild(LXMLBookmarkPart.ImportNode(LXMLBookmark.FirstChild.ChildNodes[i+1],true));
                  LastChild.FindNode(VT_XML_LOGO).FirstChild.NodeValue:=StringReplace(LastChild.FindNode(VT_XML_LOGO).FirstChild.NodeValue,YTUNER_HOST,URLHost,[rfIgnoreCase]);
                  LastChild.FindNode(VT_XML_BOOKMARK).FirstChild.NodeValue:=StringReplace(LastChild.FindNode(VT_XML_BOOKMARK).FirstChild.NodeValue,YTUNER_HOST,URLHost,[rfIgnoreCase]);
                  if LAVRConfigIdx>=0 then
                    with AVRConfigArray[LAVRConfigIdx].Translator do
                      if ReplaceUTF8Latin1Supplement or ReplaceUTF8Latin1ExtendedA or ReplaceUTF8Latin1ExtendedB then
                        LastChild.FindNode(VT_XML_STATIONNAME).FirstChild.NodeValue:=ReplaceDiacritics(LastChild.FindNode(VT_XML_STATIONNAME).FirstChild.NodeValue,LAVRConfigIdx);
                end;
            end;
          WriteXML(LXMLBookmarkPart,AXMLStream);
        except
          on E:Exception do
            begin
              Logging(ltError, string.Join(' ',[MSG_BOOKMARK,MSG_ERROR,'['+{$I %CURRENTROUTINE%}+']','('+E.Message+') ']));
              Result:=False;
              Exit;
            end;
        end;
        Result:=True;
      end
    else
      Result:=False;
  finally
    if Assigned(LXMLBookmark) then
      LXMLBookmark.Free;
    LXMLBookmarkPart.Free;
  end;
end;

function GetBookmarkStationInfo(AMAC, ASID: string; AXMLStream: TStream): boolean;
var
  LXMLBookmark: TXMLDocument = nil;
  LXMLBookmarkPart: TXMLDocument = nil;
  LNode: TDOMNode;
  LStationIdx: integer = -1;
  i: integer;
begin
  AMAC:=GetBookmarkFileName(AMAC);
  LXMLBookmarkPart:=TXMLDocument.Create;
  try
    if FileExists(AMAC) then
      begin
        try
          ReadXMLFile(LXMLBookmark,AMAC);
          LXMLBookmarkPart.AppendChild(LXMLBookmarkPart.CreateElement(VT_XML_LISTOFITEMS));
          LNode:=LXMLBookmarkPart.DocumentElement;
          LNode.AppendChild(LXMLBookmarkPart.CreateElement(VT_XML_ITEMCOUNT)).AppendChild(LXMLBookmarkPart.CreateTextNode('1'));
          LNode:=LXMLBookmark.FirstChild;
          i:=1;
          while (i<=LNode.ChildNodes.Count-1) and (LStationIdx<0) do
            begin
              if (LNode.ChildNodes[i].NodeName=VT_XML_ITEM) and (LNode.ChildNodes[i].FindNode(VT_XML_STATIONID).FirstChild.NodeValue=ASID) then
                LStationIdx:=i;
              i:=i+1;
            end;
          if LStationIdx>=0 then
            with LXMLBookmarkPart.DocumentElement do
              begin
                AppendChild(LXMLBookmarkPart.ImportNode(LXMLBookmark.FirstChild.ChildNodes[LStationIdx],true));
                if LastChild.FindNode(VT_XML_LOGO).HasChildNodes then
                  LastChild.FindNode(VT_XML_LOGO).FirstChild.NodeValue:=StringReplace(LastChild.FindNode(VT_XML_LOGO).FirstChild.NodeValue,YTUNER_HOST,URLHost,[rfIgnoreCase]);
                if LastChild.FindNode(VT_XML_BOOKMARK).HasChildNodes then
                  LastChild.FindNode(VT_XML_BOOKMARK).FirstChild.NodeValue:=StringReplace(LastChild.FindNode(VT_XML_BOOKMARK).FirstChild.NodeValue,YTUNER_HOST,URLHost,[rfIgnoreCase]);
                WriteXML(LXMLBookmarkPart,AXMLStream);
              end;
        except
          on E:Exception do
            begin
              Logging(ltError, string.Join(' ',[MSG_BOOKMARK,MSG_ERROR,'['+{$I %CURRENTROUTINE%}+']','('+E.Message+') ']));
              Result:=False;
              Exit;
            end;
        end;
        Result:=True;
      end
    else
      Result:=False;
  finally
    if Assigned(LXMLBookmark) then
      LXMLBookmark.Free;
    LXMLBookmarkPart.Free;
  end;
end;

procedure SetBookmark(AMAC, AAction: string; ANode: TDOMNode);
var
  LXMLBookmark: TXMLDocument = nil;
  LNode: TDOMNode;
  LItemCount: integer = 0;
  i: integer;
  LStationIdx: integer = -1;
  LXMLOK: boolean = False;
begin
  AMAC:=GetBookmarkFileName(AMAC);
  try
    if FileExists(AMAC) then
      ReadXMLFile(LXMLBookmark,AMAC)
    else
      case AAction of
        PATH_FAVACTION_ADD:
          begin
            LXMLBookmark:=TXMLDocument.Create;
            LXMLBookmark.AppendChild(LXMLBookmark.CreateElement(VT_XML_LISTOFITEMS));
            LNode:=LXMLBookmark.DocumentElement;
            LNode.AppendChild(LXMLBookmark.CreateElement(VT_XML_ITEMCOUNT)).AppendChild(LXMLBookmark.CreateTextNode('0'));
          end;
        PATH_FAVACTION_DEL: Exit;
      end;
    try
      LNode:=LXMLBookmark.FirstChild;
      if LNode.NodeName=VT_XML_LISTOFITEMS then
        if (LNode.FirstChild.NodeName=VT_XML_ITEMCOUNT) then
          if TryStrToInt(LNode.FirstChild.FirstChild.NodeValue,LItemCount) then
            if LItemCount<BookmarkStationsLimit then
              begin
                i:=1;
                while (i<=LNode.ChildNodes.Count-1) and (LStationIdx<0) do
                  begin
                    if (LNode.ChildNodes[i].NodeName=VT_XML_ITEM) and (LNode.ChildNodes[i].FindNode(VT_XML_STATIONID).FirstChild.NodeValue=ANode.FindNode(VT_XML_STATIONID).FirstChild.NodeValue) then
                      LStationIdx:=i;
                    i:=i+1;
                  end;
                LXMLOK:=True;
              end;
      if LXMLOK then
        case AAction of
          PATH_FAVACTION_ADD:
            if LStationIdx<0 then
              begin
                LNode.FirstChild.FirstChild.NodeValue:=IntToStr(LItemCount+1);
                LNode:=LXMLBookmark.ImportNode(ANode,true);
                LNode.FindNode(VT_XML_BOOKMARK).FirstChild.NodeValue:=StringReplace(LNode.FindNode(VT_XML_BOOKMARK).FirstChild.NodeValue,PATH_FAVACTION+'='+PATH_FAVACTION_ADD,PATH_FAVACTION+'='+PATH_FAVACTION_DEL,[rfIgnoreCase]);
                LNode.FindNode(VT_XML_LOGO).FirstChild.NodeValue:=StringReplace(LNode.FindNode(VT_XML_LOGO).FirstChild.NodeValue,URLHost,YTUNER_HOST,[rfIgnoreCase]);
                LNode.FindNode(VT_XML_BOOKMARK).FirstChild.NodeValue:=StringReplace(LNode.FindNode(VT_XML_BOOKMARK).FirstChild.NodeValue,URLHost,YTUNER_HOST,[rfIgnoreCase]);
                LXMLBookmark.DocumentElement.AppendChild(LNode);
                WriteXMLFile(LXMLBookmark,AMAC);
              end;
          PATH_FAVACTION_DEL:
            if LStationIdx>=0 then
              begin
                if LItemCount>1 then
                  begin
                    LNode.FirstChild.FirstChild.NodeValue:=IntToStr(LItemCount-1);
                    LXMLBookmark.DocumentElement.RemoveChild(LXMLBookmark.FirstChild.ChildNodes[LStationIdx]);
                    WriteXMLFile(LXMLBookmark,AMAC);
                  end
                else
                  DeleteFile(AMAC);
              end;
        end;
    finally
      if Assigned(LXMLBookmark) then
        LXMLBookmark.Free;
    end;
  except
    on E:Exception do
      Logging(ltError, string.Join(' ',[MSG_BOOKMARK,MSG_ERROR,'['+{$I %CURRENTROUTINE%}+']','('+E.Message+') ']));
  end;
end;

end.

