unit vtuner;

// YTuner: vTuner classes unit.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, Contnrs, StrUtils;

type
  TVTunerPage = class(TObject)
  public
    TotalItemsCount: integer;
    Items: TFPObjectlist;
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: TObject);
    procedure MakeXML(var AXMLDoc: TXMLDocument);
    function ToXMLString: string;
    procedure ToXMLStream(var AXMLStream: TStream);
  end;

  TVTunerInfo = class(TObject)
  public
    procedure Add2XML(var AXMLDoc: TXMLDocument); Virtual; Abstract;
  end;

  TVTunerDirectory = class(TVTunerInfo)
  public
    Title: string;
    Destination: string;
    ItemCount: integer;
    procedure Add2XML(var AXMLDoc: TXMLDocument); override;
  end;

  TVTunerStation = class(TVTunerInfo)
  public
    UID: string;
    Name: string;
    Description: string;
    URL: string;
    Icon: string;
    Genre: string;
    Location: string;
    Mime: string;
    Bitrate: string;
    Bookmark: string;
    procedure Add2XML(var AXMLDoc: TXMLDocument); override;
  end;

  TVTunerDisplay = class(TVTunerInfo)
  public
    Message: string;
    procedure Add2XML(var AXMLDoc: TXMLDocument); override;
  end;

const
  VTUNER_MIME : array of string = ('MP3','AAC','WMA');
  VT_XML_LISTOFITEMS = 'ListOfItems';
  VT_XML_ITEMCOUNT = 'ItemCount';
  VT_XML_ITEMTYPE = 'ItemType';
  VT_XML_ITEM = 'Item';
  VT_XML_DIR = 'Dir';
  VT_XML_STATION = 'Station';
  VT_XML_DISPLAY = 'Display';
  VT_XML_TITLE = 'Title';
  VT_XML_URLDIR = 'UrlDir';
  VT_XML_URLDIRBACKUP = 'UrlDirBackUp';
  VT_XML_DIRCOUNT = 'DirCount';
  VT_XML_STATIONID = 'StationId';
  VT_XML_STATIONNAME = 'StationName';
  VT_XML_STATIONURL = 'StationUrl';
  VT_XML_STATIONDESC = 'StationDesc';
  VT_XML_LOGO = 'Logo';
  VT_XML_STATIONFORMAT = 'StationFormat';
  VT_XML_STATIONLOCATION = 'StationLocation';
  VT_XML_STATIONBANDWIDTH = 'StationBandWidth';
  VT_XML_STATIONMIME = 'StationMime';
  VT_XML_RELIA = 'Relia';
  VT_XML_BOOKMARK = 'Bookmark';
  VT_XML_ENCRYPTEDTOKEN = '<EncryptedToken></EncryptedToken>';
  PATH_PARAM_DUMMY = '?ytuner=true';

var
  MyToken: string = '0123456789ABCDEF';

function AddYTunerParameter(URL: string): string;

implementation

constructor TVTunerPage.Create;
begin
  inherited Create;
  Items:= TFPObjectlist.Create(True);
  Items.OwnsObjects:=True;
end;

destructor TVTunerPage.Destroy;
begin
  Items.Clear;
  FreeAndNil(Items);
end;

procedure TVTunerPage.Add(Item: TObject);
begin
  Items.Add(Item);
end;

procedure TVTunerPage.MakeXML(var AXMLDoc: TXMLDocument);
var
  LRootNode: TDOMNode;
  LItemIndex: integer;
begin
  LRootNode:=AXMLDoc.CreateElement(VT_XML_LISTOFITEMS);
  AXMLDoc.AppendChild(LRootNode);
  LRootNode:= AXMLDoc.DocumentElement;
  if Assigned(Items) then
    begin
      LRootNode.AppendChild(AXMLDoc.CreateElement(VT_XML_ITEMCOUNT)).AppendChild(AXMLDoc.CreateTextNode(IntToStr(TotalItemsCount)));
      for LItemIndex:=0 to Items.Count-1 do
        TVTunerInfo(Items[LItemIndex]).Add2XML(AXMLDoc);
    end;
end;

function TVTunerPage.ToXMLString: string;
var
  LXMLDoc: TXMLDocument;
  LXMLDocStream: TStringStream;
begin
  LXMLDoc:=TXMLDocument.Create;
  try
    MakeXML(LXMLDoc);
    try
      LXMLDocStream:=TStringStream.Create;
      WriteXML(LXMLDoc,LXMLDocStream);
      ToXMLString:=LXMLDocStream.UnicodeDataString;
    finally
      LXMLDocStream.Free;
    end;
  finally
    LXMLDoc.Free;
  end;
end;

procedure TVTunerPage.ToXMLStream(var AXMLStream: TStream);
var
  LXMLDoc: TXMLDocument;
begin
  LXMLDoc:=TXMLDocument.Create;
  try
    MakeXML(LXMLDoc);
    WriteXML(LXMLDoc,AXMLStream);
  finally
    LXMLDoc.Free;
  end;
end;

procedure TVTunerDirectory.Add2XML(var AXMLDoc: TXMLDocument);
var
  LElementNode: TDOMNode;
begin
  LElementNode:=AXMLDoc.CreateElement(VT_XML_ITEM);
  AXMLDoc.DocumentElement.AppendChild(LElementNode);
  with LElementNode do
    begin
      AppendChild(AXMLDoc.CreateElement(VT_XML_ITEMTYPE)).AppendChild(AXMLDoc.CreateTextNode(VT_XML_DIR));
      AppendChild(AXMLDoc.CreateElement(VT_XML_TITLE)).AppendChild(AXMLDoc.CreateTextNode(Title));
      AppendChild(AXMLDoc.CreateElement(VT_XML_URLDIR)).AppendChild(AXMLDoc.CreateTextNode(AddYTunerParameter(Destination)));
      AppendChild(AXMLDoc.CreateElement(VT_XML_URLDIRBACKUP)).AppendChild(AXMLDoc.CreateTextNode(AddYTunerParameter(Destination)));
      AppendChild(AXMLDoc.CreateElement(VT_XML_DIRCOUNT)).AppendChild(AXMLDoc.CreateTextNode(IntToStr(ItemCount)));
    end;
end;

procedure TVTunerStation.Add2XML(var AXMLDoc: TXMLDocument);
var
  LElementNode: TDOMNode;
  LCodec: string='';
begin
  LElementNode:=AXMLDoc.CreateElement(VT_XML_ITEM);
  AXMLDoc.DocumentElement.AppendChild(LElementNode);
  with LElementNode do
    begin
      AppendChild(AXMLDoc.CreateElement(VT_XML_ITEMTYPE)).AppendChild(AXMLDoc.CreateTextNode(VT_XML_STATION));
      AppendChild(AXMLDoc.CreateElement(VT_XML_STATIONID)).AppendChild(AXMLDoc.CreateTextNode(UID));
      AppendChild(AXMLDoc.CreateElement(VT_XML_STATIONNAME)).AppendChild(AXMLDoc.CreateTextNode(Name));
      AppendChild(AXMLDoc.CreateElement(VT_XML_STATIONURL)).AppendChild(AXMLDoc.CreateTextNode(URL));
      AppendChild(AXMLDoc.CreateElement(VT_XML_STATIONDESC)).AppendChild(AXMLDoc.CreateTextNode(Description));
      AppendChild(AXMLDoc.CreateElement(VT_XML_LOGO)).AppendChild(AXMLDoc.CreateTextNode(Icon));
      AppendChild(AXMLDoc.CreateElement(VT_XML_STATIONFORMAT)).AppendChild(AXMLDoc.CreateTextNode(Genre));
      AppendChild(AXMLDoc.CreateElement(VT_XML_STATIONLOCATION)).AppendChild(AXMLDoc.CreateTextNode(Location));
      AppendChild(AXMLDoc.CreateElement(VT_XML_STATIONBANDWIDTH)).AppendChild(AXMLDoc.CreateTextNode(Bitrate));
      if IndexText(Mime,VTUNER_MIME)>=0 then LCodec:=Mime;
      AppendChild(AXMLDoc.CreateElement(VT_XML_STATIONMIME)).AppendChild(AXMLDoc.CreateTextNode(LCodec));
      AppendChild(AXMLDoc.CreateElement(VT_XML_RELIA)).AppendChild(AXMLDoc.CreateTextNode('3'));
      AppendChild(AXMLDoc.CreateElement(VT_XML_BOOKMARK)).AppendChild(AXMLDoc.CreateTextNode(Bookmark));
    end;
end;

procedure TVTunerDisplay.Add2XML(var AXMLDoc: TXMLDocument);
var
  LElementNode: TDOMNode;
begin
  LElementNode:=AXMLDoc.CreateElement(VT_XML_ITEM);
  AXMLDoc.DocumentElement.AppendChild(LElementNode);
  with LElementNode do
    begin
      AppendChild(AXMLDoc.CreateElement(VT_XML_ITEMTYPE)).AppendChild(AXMLDoc.CreateTextNode(VT_XML_DISPLAY));
      AppendChild(AXMLDoc.CreateElement(VT_XML_DISPLAY)).AppendChild(AXMLDoc.CreateTextNode(Message));
    end;
end;

function AddYTunerParameter(URL: string): string;
begin
  AddYTunerParameter:=URL+PATH_PARAM_DUMMY;
end;

end.

