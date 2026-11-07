unit webgui;

// Viritin: browser-based editor for the "My Stations" list.
//
// Runs on its own listener rather than as routes on the AVR-facing web server,
// for two reasons: that server is reachable by anything on the network (and by
// the internet, if Retuner is hosted remotely), and this one writes to config
// files. Keeping it separate means it can be bound and firewalled on its own.
//
// It is off by default, binds to loopback unless told otherwise, and refuses to
// start without a password.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, base64, fpjson, jsonparser,
  fphttpserver, httpdefs,
  common, my_stations, avrremote;

const
  WEBGUI_SERVICE = 'Web GUI';
  WEBGUI_PORT = 8090;
  WEBGUI_IPADDRESS = '127.0.0.1';
  WEBGUI_USER = 'admin';
  WEBGUI_REALM = 'Viritin';
// Long enough to make guessing impractical, short enough that a typo does not
// feel like a hang.
  WEBGUI_FAILED_AUTH_DELAY_MS = 1000;
// A cross-origin HTML form can only send urlencoded, multipart or plain text.
// Requiring a JSON content type therefore forces a CORS preflight, which this
// API never answers, so another site cannot drive it with the browser's stored
// credentials.
  WEBGUI_REQUIRED_CONTENT_TYPE = 'application/json';

type
  TWebGUIServer = Class
  Private
    FServer: TFPHTTPServer;
  Public
    constructor Create;
    destructor Destroy; override;
    procedure DoHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    property Server: TFPHTTPServer Read FServer Write FServer;
  end;

var
  WebGUIEnabled: boolean = False;
  WebGUIIPAddress: string = WEBGUI_IPADDRESS;
  WebGUIPort: integer = WEBGUI_PORT;
  WebGUIUser: string = WEBGUI_USER;
  WebGUIPassword: string = '';

function StartWebGUIServer: boolean;
procedure StopWebGUIServer;

implementation

var
  WebGUIServer: TWebGUIServer = nil;
  WebGUIThreadID: TThreadID;

// --- station file I/O --------------------------------------------------------

function MyStationsIsYAML: boolean;
begin
  Result:=not SameText(ExtractFileExt(MyStationsFilePath),'.ini');
end;

function StationsAsJSON: TJSONStringType;
var
  LStations: TMyStationsGroup;
  LRoot, LCategory, LStation: TJSONObject;
  LCategories, LList: TJSONArray;
  i, j: integer;
begin
  LStations:=GetMyStationsSnapshot;
  LRoot:=TJSONObject.Create;
  try
    LCategories:=TJSONArray.Create;
    for i:=0 to High(LStations) do
      begin
        LCategory:=TJSONObject.Create;
        LCategory.Add('name',LStations[i].MSCategory);
        LList:=TJSONArray.Create;
        for j:=0 to High(LStations[i].MSStations) do
          begin
            LStation:=TJSONObject.Create;
            LStation.Add('name',LStations[i].MSStations[j].MSName);
            LStation.Add('url',LStations[i].MSStations[j].MSURL);
            LStation.Add('logo',LStations[i].MSStations[j].MSLogoURL);
            LList.Add(LStation);
          end;
        LCategory.Add('stations',LList);
        LCategories.Add(LCategory);
      end;
    LRoot.Add('categories',LCategories);
    LRoot.Add('file',MyStationsFileName);
    LRoot.Add('readonly',MyStationsIsYAML);
    Result:=LRoot.AsJSON;
  finally
    LRoot.Free;
  end;
end;

function StatusAsJSON: TJSONStringType;
var
  LRoot: TJSONObject;
  LStations: TMyStationsGroup;
  LCount, i: integer;
begin
  LStations:=GetMyStationsSnapshot;
  LCount:=0;
  for i:=0 to High(LStations) do
    LCount:=LCount+Length(LStations[i].MSStations);
  LRoot:=TJSONObject.Create;
  try
    LRoot.Add('app',APP_NAME);
    LRoot.Add('version',APP_VERSION);
    LRoot.Add('categories',Length(LStations));
    LRoot.Add('stations',LCount);
    LRoot.Add('file',MyStationsFileName);
    Result:=LRoot.AsJSON;
  finally
    LRoot.Free;
  end;
end;

// Builds an error document, letting fpjson handle escaping rather than pasting
// the message into a hand-written string.
function ErrorAsJSON(const AMessage: string): TJSONStringType;
var
  LRoot: TJSONObject;
begin
  LRoot:=TJSONObject.Create;
  try
    LRoot.Add('error',AMessage);
    Result:=LRoot.AsJSON;
  finally
    LRoot.Free;
  end;
end;

// Rewrites the stations file from the posted document. Returns '' on success or
// a message to show the user.
function SaveStationsFromJSON(const ABody: TJSONStringType): string;
var
  LData: TJSONData = nil;
  LCategories, LList: TJSONArray;
  LCategory, LStation: TJSONObject;
  LOut: TStringList;
  LName, LURL, LLogo, LRaw: string;
  i, j: integer;
begin
  Result:='';
  if MyStationsIsYAML then
    Exit('Editing is only supported for .ini station files.');
  LOut:=TStringList.Create;
  try
    try
      LData:=GetJSON(ABody);
      if not (LData is TJSONObject) then
        Exit('Malformed request.');
      LCategories:=TJSONObject(LData).Get('categories',TJSONArray(nil));
      if LCategories=nil then
        Exit('Malformed request.');
      for i:=0 to LCategories.Count-1 do
        begin
          if not (LCategories[i] is TJSONObject) then
            Continue;
          LCategory:=TJSONObject(LCategories[i]);
          LRaw:=LCategory.Get('name','');
          LName:=LRaw.Trim;
// A category with no name would produce "[]", which the parser would then read
// back as a nameless section.
          if LName.IsEmpty then
            Continue;
          LOut.Add('['+LName+']');
          LList:=LCategory.Get('stations',TJSONArray(nil));
          if LList<>nil then
            for j:=0 to LList.Count-1 do
              begin
                if not (LList[j] is TJSONObject) then
                  Continue;
                LStation:=TJSONObject(LList[j]);
                LRaw:=LStation.Get('name','');   LName:=LRaw.Trim;
                LRaw:=LStation.Get('url','');    LURL:=LRaw.Trim;
                LRaw:=LStation.Get('logo','');   LLogo:=LRaw.Trim;
// '=' separates name from value and '|' separates URL from logo, so a value
// containing either would not survive a round trip.
                LName:=LName.Replace('=','-').Replace('|','-');
                LURL:=LURL.Replace('|','-');
                LLogo:=LLogo.Replace('|','-');
                if LName.IsEmpty or LURL.IsEmpty then
                  Continue;
                if LLogo.IsEmpty then
                  LOut.Add(LName+'='+LURL)
                else
                  LOut.Add(LName+'='+LURL+'|'+LLogo);
              end;
          LOut.Add('');
        end;
      LOut.SaveToFile(MyStationsFilePath);
    except
      on E: Exception do
        Exit('Could not save: '+E.Message);
    end;
  finally
    LOut.Free;
    if Assigned(LData) then
      LData.Free;
  end;
// Publish the new list immediately rather than waiting for the refresh timer,
// which may not even be enabled. This rebuilds from every source, not just the
// file that was saved -- reloading the user's file alone would drop every
// preset station until the next restart.
  if not ReloadStations then
    Result:='Saved, but the file could not be re-read. Check the log.';
end;

// --- page --------------------------------------------------------------------

function GUIPage: string;
var
  P: TStringList;
begin
  P:=TStringList.Create;
  try
    P.Add('<!doctype html><html lang="en"><head><meta charset="utf-8">');
    P.Add('<meta name="viewport" content="width=device-width,initial-scale=1">');
    P.Add('<title>Viritin stations</title><style>');
    P.Add(':root{--bg:#f6f5f2;--card:#fff;--ink:#1c2530;--soft:#5b6875;--line:#dcd8d0;--accent:#0e7d86;--danger:#b3261e}');
    P.Add('@media(prefers-color-scheme:dark){:root{--bg:#151b21;--card:#1d252d;--ink:#e6ebef;--soft:#a3aeb8;--line:#2d3742;--accent:#3fc1cb;--danger:#ff8a80}}');
    P.Add('*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--ink);font:16px/1.5 system-ui,-apple-system,Segoe UI,sans-serif}');
    P.Add('.wrap{max-width:900px;margin:0 auto;padding:32px 20px 80px}');
    P.Add('h1{font-size:26px;margin:0 0 4px}.sub{color:var(--soft);margin:0 0 24px;font-size:14px}');
    P.Add('.cat{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:16px;margin-bottom:14px}');
    P.Add('.cathead{display:flex;gap:10px;align-items:center;margin-bottom:10px}');
    P.Add('input{font:inherit;padding:7px 10px;border:1px solid var(--line);border-radius:6px;background:var(--bg);color:var(--ink);width:100%}');
    P.Add('.catname{font-weight:600}');
    P.Add('table{width:100%;border-collapse:collapse}td{padding:3px 4px;vertical-align:top}');
    P.Add('th{text-align:left;font-size:12px;letter-spacing:.08em;text-transform:uppercase;color:var(--soft);padding:4px}');
    P.Add('button{font:inherit;padding:7px 14px;border-radius:6px;border:1px solid var(--line);background:var(--card);color:var(--ink);cursor:pointer}');
    P.Add('button:hover{border-color:var(--accent)}');
    P.Add('.primary{background:var(--accent);border-color:var(--accent);color:#fff;font-weight:600}');
    P.Add('.x{border:none;background:none;color:var(--danger);font-size:18px;line-height:1;padding:4px 8px}');
    P.Add('.bar{position:sticky;bottom:0;background:var(--bg);border-top:1px solid var(--line);padding:14px 0;display:flex;gap:10px;align-items:center;flex-wrap:wrap}');
    P.Add('.msg{font-size:14px;color:var(--soft)}.err{color:var(--danger)}');
    P.Add('.ro{background:#f9f0dd;border:1px solid #e0c98a;color:#6b4f14;padding:10px 14px;border-radius:6px;margin-bottom:18px;font-size:14px}');
    P.Add('@media(max-width:640px){');
    P.Add('.wrap{padding:20px 14px 80px}h1{font-size:22px}');
    P.Add('.cat{padding:12px}');
    P.Add('table,tbody,tr,td{display:block;width:100%}');
    P.Add('tr.hd{display:none}');
    P.Add('tr{border:1px solid var(--line);border-radius:6px;padding:10px;margin-bottom:10px;position:relative}');
    P.Add('td{padding:4px 0}');
    P.Add('td[data-l]::before{content:attr(data-l);display:block;font-size:11px;letter-spacing:.08em;');
    P.Add('text-transform:uppercase;color:var(--soft);margin-bottom:3px}');
// The delete button is the one cell with no field under it, so it is pinned to
// the card's top-right corner rather than left to sit on a row of its own.
    P.Add('td:last-child{position:absolute;top:6px;right:6px;width:auto;padding:0}');
    P.Add('}');
    P.Add('</style></head><body><div class="wrap">');
    P.Add('<h1>Viritin</h1>');
    P.Add('<p class="sub">My Stations &mdash; the list your receiver shows '
        + 'under that menu. Changes are written to your stations file when you '
        + 'press Save.</p>');
    P.Add('<p class="sub" id="status">Loading&hellip;</p>');
    P.Add('<div id="ro" class="ro" hidden>This station file is YAML. Editing here supports .ini files only, so saving is disabled.</div>');
    P.Add('<div id="cats"></div>');
    P.Add('<button id="addcat">+ Add category</button>');
    P.Add('<div class="bar"><button class="primary" id="save">Save</button>');
    P.Add('<button id="reload">Discard changes</button><span class="msg" id="msg"></span></div>');
    P.Add('</div><script>');
    P.Add('const H={"Content-Type":"application/json"};');
    P.Add('let data={categories:[]},readonly=false;');
    P.Add('const el=(t,a={})=>Object.assign(document.createElement(t),a);');
    P.Add('function draw(){const c=document.getElementById("cats");c.textContent="";');
    P.Add(' data.categories.forEach((cat,ci)=>{const d=el("div",{className:"cat"});');
    P.Add('  const h=el("div",{className:"cathead"});');
    P.Add('  const n=el("input",{value:cat.name,className:"catname"});n.oninput=e=>cat.name=e.target.value;');
    P.Add('  const rm=el("button",{className:"x",textContent:"\u00d7",title:"Remove category"});');
    P.Add('  rm.onclick=()=>{data.categories.splice(ci,1);draw()};');
    P.Add('  h.append(n,rm);d.append(h);');
    P.Add('  const t=el("table");const hr=el("tr",{className:"hd"});');
    P.Add('  ["Name","Stream URL","Logo URL (optional)",""].forEach(x=>hr.append(el("th",{textContent:x})));');
    P.Add('  t.append(hr);');
    P.Add('  cat.stations.forEach((s,si)=>{const r=el("tr");');
    P.Add('   [["name","Name"],["url","Stream URL"],["logo","Logo URL"]].forEach(([k,lab])=>{');
    P.Add('    const td=el("td");td.setAttribute("data-l",lab);');
    P.Add('    const i=el("input",{value:s[k]||""});i.oninput=e=>s[k]=e.target.value;td.append(i);r.append(td)});');
    P.Add('   const td=el("td");const b=el("button",{className:"x",textContent:"\u00d7",title:"Remove station"});');
    P.Add('   b.onclick=()=>{cat.stations.splice(si,1);draw()};td.append(b);r.append(td);t.append(r)});');
    P.Add('  d.append(t);');
    P.Add('  const add=el("button",{textContent:"+ Add station"});');
    P.Add('  add.onclick=()=>{cat.stations.push({name:"",url:"",logo:""});draw()};');
    P.Add('  d.append(add);c.append(d)})}');
    P.Add('async function load(){const r=await fetch("api/stations",{headers:H});const j=await r.json();');
    P.Add(' data={categories:j.categories||[]};readonly=!!j.readonly;');
    P.Add(' document.getElementById("ro").hidden=!readonly;');
    P.Add(' document.getElementById("save").disabled=readonly;');
    P.Add(' const s=await(await fetch("api/status",{headers:H})).json();');
    P.Add(' document.getElementById("status").textContent=');
    P.Add('  s.app+" "+s.version+" \u2014 "+s.stations+" stations in "+s.categories+" categories \u2014 "+s.file;');
    P.Add(' draw()}');
    P.Add('document.getElementById("addcat").onclick=()=>{data.categories.push({name:"New category",stations:[]});draw()};');
    P.Add('document.getElementById("reload").onclick=()=>{msg("");load()};');
    P.Add('function msg(t,bad){const m=document.getElementById("msg");m.textContent=t;m.className="msg"+(bad?" err":"")}');
    P.Add('document.getElementById("save").onclick=async()=>{msg("Saving\u2026");');
    P.Add(' const r=await fetch("api/stations",{method:"POST",headers:H,body:JSON.stringify(data)});');
    P.Add(' const j=await r.json();');
    P.Add(' if(j.error){msg(j.error,true)}else{msg("Saved.");load()}};');
    P.Add('load();');
    P.Add('</script></body></html>');
    Result:=P.Text;
  finally
    P.Free;
  end;
end;

// --- server ------------------------------------------------------------------

// String comparison returns at the first differing byte, so how long it takes
// says how many leading characters were right. Over a LAN that is a slow oracle
// but a real one, and the fix costs nothing. The length is still visible; that
// is the standard trade and it does not narrow the search usefully.
function ConstantTimeEquals(const A, B: string): boolean;
var
  LIdx, LDiff: integer;
begin
  Result:=False;
  if Length(A)<>Length(B) then
    Exit;
  LDiff:=0;
  for LIdx:=1 to Length(A) do
    LDiff:=LDiff or (Ord(A[LIdx]) xor Ord(B[LIdx]));
  Result:=LDiff=0;
end;

function Authorised(ARequest: TFPHTTPConnectionRequest): boolean;
var
  LHeader, LDecoded: string;
  LUserOK, LPassOK: boolean;
begin
  Result:=False;
  LHeader:=ARequest.Authorization;
  if not LHeader.ToLower.StartsWith('basic ') then
    Exit;
  try
    LDecoded:=DecodeStringBase64(LHeader.Substring(6).Trim);
  except
    Exit;
  end;
  if not LDecoded.Contains(':') then
    Exit;
// Both halves are always compared: 'and' would short-circuit on a wrong user
// name and hand back a different timing profile than a wrong password.
  LUserOK:=ConstantTimeEquals(LDecoded.Substring(0,LDecoded.IndexOf(':')),WebGUIUser);
  LPassOK:=ConstantTimeEquals(LDecoded.Substring(LDecoded.IndexOf(':')+1),WebGUIPassword);
  Result:=LUserOK and LPassOK;
end;

procedure SendJSON(var AResponse: TFPHTTPConnectionResponse; ACode: integer; const ABody: string);
begin
  with AResponse do
    begin
      Code:=ACode;
      ContentType:=HTTP_RESPONSE_CONTENT_TYPE[ctJSON];
      Content:=ABody;
      ContentLength:=Length(Content);
      SendContent;
    end;
end;

function RemotePage: string;
var
  P: TStringList;
begin
  P:=TStringList.Create;
  try
    P.Add('<!doctype html><html lang="en"><head><meta charset="utf-8">');
    P.Add('<meta name="viewport" content="width=device-width,initial-scale=1">');
    P.Add('<title>Viritin remote</title><style>');
    P.Add(':root{--bg:#f6f5f2;--card:#fff;--ink:#1c2530;--soft:#5b6875;--line:#dcd8d0;--accent:#0e7d86;--danger:#b3261e}');
    P.Add('@media(prefers-color-scheme:dark){:root{--bg:#151b21;--card:#1d252d;--ink:#e6ebef;--soft:#a3aeb8;--line:#2d3742;--accent:#3fc1cb;--danger:#ff8a80}}');
    P.Add('*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--ink);font:16px/1.5 system-ui,-apple-system,Segoe UI,sans-serif}');
    P.Add('.wrap{max-width:640px;margin:0 auto;padding:24px 16px 60px}');
    P.Add('h1{font-size:22px;margin:0 0 2px}.sub{color:var(--soft);margin:0 0 18px;font-size:13px}');
    P.Add('.card{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:8px;margin-bottom:14px}');
    P.Add('.row{padding:9px 12px;border-radius:6px;font-variant-numeric:tabular-nums;cursor:pointer}');
    P.Add('.row:hover{background:var(--line)}');
    P.Add('.row.cur:hover{background:var(--accent)}');
    P.Add('.row.busy{opacity:.5}');
    P.Add('.row.cur{background:var(--accent);color:#fff;font-weight:600}');
    P.Add('.page{color:var(--soft);font-size:13px;text-align:right;padding:4px 12px}');
    P.Add('.pad{display:grid;grid-template-columns:repeat(3,1fr);gap:8px;max-width:280px;margin:0 auto}');
    P.Add('button{font:inherit;padding:12px;border:1px solid var(--line);border-radius:8px;background:var(--card);color:var(--ink);cursor:pointer}');
    P.Add('button:active{background:var(--accent);color:#fff}');
    P.Add('.wide{grid-column:1/4}');
    P.Add('form{display:flex;gap:8px;margin:0}input{flex:1;font:inherit;padding:10px;border:1px solid var(--line);border-radius:8px;background:var(--bg);color:var(--ink)}');
    P.Add('.err{color:var(--danger);font-size:14px;padding:8px 12px}');
    P.Add('</style></head><body id="viritin-remote"><div class="wrap">');
    P.Add('<h1>Viritin remote</h1>');
    P.Add('<p class="sub">The list your receiver is showing. Click a line to go to it; arrow keys and Enter work too.</p>');
    P.Add('<div class="card"><div id="list"></div><div class="page" id="page"></div></div>');
    P.Add('<div class="card"><form id="search" autocomplete="off">');
    P.Add('<input id="q" maxlength="30" placeholder="Search stations by keyword">');
    P.Add('<button type="submit">Search</button></form></div>');
    P.Add('<div class="card"><div class="pad">');
    P.Add('<button data-c="CmdPageUp">Page up</button><button data-c="CurUp">Up</button><button data-c="CmdPageDown">Page down</button>');
    P.Add('<button data-c="CurLeft">Back</button><button data-c="CurEnter">Enter</button><button data-c="CurRight">Fwd</button>');
    P.Add('<button class="wide" data-c="CurDown">Down</button>');
    P.Add('</div></div>');
    P.Add('<div class="err" id="err"></div>');
    P.Add('</div><script>');
    P.Add('var listEl=document.getElementById("list"),pageEl=document.getElementById("page"),errEl=document.getElementById("err");');
    P.Add('function show(e){errEl.textContent=e||"";}');
    P.Add('function draw(d){');
    P.Add(' if(!d.ok){show(d.error||"The receiver did not answer.");return;}');
    P.Add(' show("");');
    P.Add(' listEl.innerHTML="";');
    P.Add(' d.lines.forEach(function(l){');
    P.Add('  var r=document.createElement("div");');
    P.Add('  r.className="row"+(l.cursor?" cur":"");');
    P.Add('  r.textContent=l.text;');
    P.Add('  r.addEventListener("click",function(){select(l.index,r);});');
    P.Add('  listEl.appendChild(r);});');
    P.Add(' pageEl.textContent=d.page||"";}');
    P.Add('function poll(){fetch("api/remote/status",{credentials:"same-origin"})');
    P.Add(' .then(function(r){return r.json();}).then(draw)');
    P.Add(' .catch(function(){show("Cannot reach Viritin.");});}');
    P.Add('function select(i,row){');
    P.Add(' row.classList.add("busy");');
    P.Add(' fetch("api/remote/select",{method:"POST",credentials:"same-origin",');
    P.Add('  headers:{"Content-Type":"application/json"},body:JSON.stringify({index:i})})');
    P.Add(' .then(function(r){return r.json();})');
    P.Add(' .then(function(d){if(!d.ok)show(d.error||"Refused.");setTimeout(poll,500);})');
    P.Add(' .catch(function(){show("Cannot reach Viritin.");});}');
    P.Add('function send(c){fetch("api/remote/cmd",{method:"POST",credentials:"same-origin",');
    P.Add(' headers:{"Content-Type":"application/json"},body:JSON.stringify({cmd:c})})');
    P.Add(' .then(function(r){return r.json();})');
    P.Add(' .then(function(d){if(!d.ok)show(d.error||"Refused.");setTimeout(poll,350);});}');
    P.Add('document.querySelectorAll("button[data-c]").forEach(function(b){');
    P.Add(' b.addEventListener("click",function(){send(b.dataset.c);});});');
    P.Add('document.getElementById("search").addEventListener("submit",function(e){');
    P.Add(' e.preventDefault();var q=document.getElementById("q").value;');
    P.Add(' fetch("api/remote/search",{method:"POST",credentials:"same-origin",');
    P.Add('  headers:{"Content-Type":"application/json"},body:JSON.stringify({q:q})})');
    P.Add(' .then(function(r){return r.json();})');
    P.Add(' .then(function(d){if(!d.ok)show(d.error||"Refused.");setTimeout(poll,600);});});');
    P.Add('var KEYS={ArrowUp:"CurUp",ArrowDown:"CurDown",ArrowLeft:"CurLeft",ArrowRight:"CurRight",Enter:"CurEnter",PageUp:"CmdPageUp",PageDown:"CmdPageDown"};');
    P.Add('document.addEventListener("keydown",function(e){');
    P.Add(' if(e.target.tagName==="INPUT")return;');
    P.Add(' var c=KEYS[e.key];if(c){e.preventDefault();send(c);}});');
    P.Add('poll();setInterval(poll,2000);');
    P.Add('</script></body></html>');
    Result:=P.Text;
  finally
    P.Free;
  end;
end;

// The page sends one string per request; anything else is a malformed body.
function StringFieldOf(const ABody, AField: string; out AValue: string): boolean;
var
  LData: TJSONData = nil;
begin
  Result:=False;
  AValue:='';
  try
    LData:=GetJSON(ABody);
    if LData is TJSONObject then
      begin
        AValue:=TJSONObject(LData).Get(AField,'');
        Result:=True;
      end;
  except
    on E: Exception do
      Result:=False;
  end;
  LData.Free;
end;

// The page sends one number per request here; a body that is not that is
// malformed, and a float or a string is not "close enough" to a line index.
function IntegerFieldOf(const ABody, AField: string; out AValue: integer): boolean;
var
  LData: TJSONData = nil;
  LObject: TJSONObject;
  LField: TJSONData;
begin
  Result:=False;
  AValue:=0;
  try
    LData:=GetJSON(ABody);
    if LData is TJSONObject then
      begin
        LObject:=TJSONObject(LData);
        LField:=LObject.Find(AField);
        if (LField<>nil) and (LField.JSONType=jtNumber) then
          begin
            AValue:=LField.AsInteger;
            Result:=True;
          end;
      end;
  except
    on E: Exception do
      Result:=False;
  end;
  LData.Free;
end;

constructor TWebGUIServer.Create;
begin
  FServer:=TFPHTTPServer.Create(Nil);
  with FServer do
    begin
      Active:=False;
{$IF FPC_FULLVERSION >= 30300}
      Address:=WebGUIIPAddress;
      ThreadMode:=tmThread;
{$ELSE}
      Threaded:=True;
{$ENDIF}
      Port:=WebGUIPort;
      OnRequest:=@DoHandleRequest;
      Active:=True;
    end;
end;

destructor TWebGUIServer.Destroy;
begin
  if Assigned(FServer) then
    begin
      FServer.Active:=False;
      FreeAndNil(FServer);
    end;
  inherited Destroy;
end;

function IPv4ToCardinal(const AAddr: string; out AValue: Cardinal): boolean;
var
  LParts: TStringArray;
  LIdx, LOctet: integer;
begin
  Result:=False;
  AValue:=0;
  LParts:=AAddr.Split(['.']);
  if Length(LParts)<>4 then
    Exit;
  for LIdx:=0 to 3 do
    begin
      if (LParts[LIdx]='') or (Length(LParts[LIdx])>3) then
        Exit;
      if not TryStrToInt(LParts[LIdx],LOctet) then
        Exit;
      if (LOctet<0) or (LOctet>255) then
        Exit;
      AValue:=(AValue shl 8) or Cardinal(LOctet);
    end;
  Result:=True;
end;

// '192.168.10.0/24'. IPv4 only: an IPv6 client simply does not match a range,
// and has to be named exactly if it is wanted.
function InIPv4Range(const ARemote, AEntry: string): boolean;
var
  LSlash, LBits, LErr: integer;
  LNet, LAddr, LMask: Cardinal;
begin
  Result:=False;
  LSlash:=AEntry.IndexOf('/');
  if LSlash<0 then
    Exit;
  Val(AEntry.Substring(LSlash+1),LBits,LErr);
  if (LErr<>0) or (LBits<0) or (LBits>32) then
    Exit;
  if not IPv4ToCardinal(AEntry.Substring(0,LSlash),LNet) then
    Exit;
  if not IPv4ToCardinal(ARemote,LAddr) then
    Exit;
// A shift of 32 is undefined, so the "match everything" case is its own branch
// rather than a mask computed from it.
  if LBits=0 then
    LMask:=0
  else
    LMask:=not Cardinal((Cardinal(1) shl (32-LBits))-1);
  Result:=(LNet and LMask)=(LAddr and LMask);
end;

// TFPHTTPServer only grew an Address property in FPC 3.3, so on the compiler
// this project is built with the listening socket is open on every interface
// whatever WebGUIIPAddress says. A service that writes configuration files must
// not be quietly wider than its setting claims, so the address is enforced here
// on the connection instead: the port answers, but only to the clients the
// setting names.
//
// It takes a comma-separated list, and an entry may be a range. That is what
// makes "reachable from my LAN and nowhere else" expressible -
// '192.168.10.0/24' - which before this was not: the setting matched one exact
// address, so anybody wanting a phone on it had to open the editor to every
// client with '0.0.0.0'. On a service that writes configuration files, the
// difference between "my LAN" and "anyone" is the whole point of the setting.
//
// "default", "0.0.0.0" and an empty value still mean any client, which is what
// a container or a remote host needs.
function ClientAllowed(const ARemote: string): boolean;
var
  LEntry: string;
begin
  Result:=True;
  if WebGUIIPAddress.IsEmpty
     or (WebGUIIPAddress=DEFAULT_STRING)
     or (WebGUIIPAddress='0.0.0.0') then
    Exit;
  Result:=False;
  for LEntry in WebGUIIPAddress.Split([',']) do
    begin
      if LEntry.Trim.IsEmpty then
        Continue;
      if LEntry.Trim.Contains('/') then
        begin
          if InIPv4Range(ARemote,LEntry.Trim) then
            Exit(True);
        end
      else
        if SameText(ARemote,LEntry.Trim) then
          Exit(True);
    end;
end;

procedure TWebGUIServer.DoHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  LError, LPath, LValue: string;
  LIndex: integer;
begin
  if not ClientAllowed(ARequest.RemoteAddress) then
    begin
      Logging(ltWarning, string.Join(' ',[WEBGUI_SERVICE+':','refused',ARequest.URI,'from',ARequest.RemoteAddress,'(WebGUIIPAddress='+WebGUIIPAddress+')']));
      with AResponse do
        begin
          Code:=HTTP_CODE_UNAVAILABLE;
          Content:='Service unavailable.';
          ContentLength:=Length(Content);
          SendContent;
        end;
      Exit;
    end;

  if not Authorised(ARequest) then
    begin
// A wrong password costs a second. Online guessing against Basic auth is
// otherwise limited only by how fast the attacker can open connections, and
// this is a service that writes configuration files. It delays nobody who
// knows the password.
      Sleep(WEBGUI_FAILED_AUTH_DELAY_MS);
      Logging(ltWarning, string.Join(' ',[WEBGUI_SERVICE+':','unauthorised',ARequest.URI,'from',ARequest.RemoteAddress]));
      with AResponse do
        begin
          Code:=401;
          SetCustomHeader('WWW-Authenticate','Basic realm="'+WEBGUI_REALM+'"');
          Content:='Authentication required.';
          ContentLength:=Length(Content);
          SendContent;
        end;
      Exit;
    end;

// Anything that changes state must arrive as JSON, which a cross-origin form
// cannot produce, so a hostile site cannot ride the browser's stored credentials.
  if (ARequest.Method='POST') and (not ARequest.ContentType.ToLower.StartsWith(WEBGUI_REQUIRED_CONTENT_TYPE)) then
    begin
      SendJSON(AResponse,400,ErrorAsJSON('Expected an '+WEBGUI_REQUIRED_CONTENT_TYPE+' request.'));
      Exit;
    end;

  LPath:=ARequest.URI.ToLower;
  LPath:=LPath.Split(['?'])[0];
  case LPath of
    '/','/index.html':
      with AResponse do
        begin
          Code:=HTTP_CODE_OK;
          ContentType:=HTTP_RESPONSE_CONTENT_TYPE[ctNone];
          Content:=GUIPage;
          ContentLength:=Length(Content);
          SendContent;
        end;
    '/api/status':
      SendJSON(AResponse,HTTP_CODE_OK,StatusAsJSON);
    '/remote','/remote/':
      with AResponse do
        begin
          Code:=HTTP_CODE_OK;
          ContentType:=HTTP_RESPONSE_CONTENT_TYPE[ctNone];
          Content:=RemotePage;
          ContentLength:=Length(Content);
          SendContent;
        end;
    '/api/remote/status':
      SendJSON(AResponse,HTTP_CODE_OK,RemoteStatusAsJSON);
    '/api/remote/cmd':
      if ARequest.Method<>'POST' then
        SendJSON(AResponse,HTTP_CODE_OK,RemoteErrorAsJSON('POST required.'))
      else if not StringFieldOf(ARequest.Content,'cmd',LValue) then
        SendJSON(AResponse,HTTP_CODE_OK,RemoteErrorAsJSON('Malformed request.'))
      else
        begin
          LError:=RemoteSendCommand(LValue);
          if LError.IsEmpty then
            SendJSON(AResponse,HTTP_CODE_OK,'{"ok":true}')
          else
            begin
              Logging(ltWarning, string.Join(' ',[WEBGUI_SERVICE+': remote refused',LValue,'from',ARequest.RemoteAddress]));
              SendJSON(AResponse,HTTP_CODE_OK,RemoteErrorAsJSON(LError));
            end;
        end;
    '/api/remote/select':
      if ARequest.Method<>'POST' then
        SendJSON(AResponse,HTTP_CODE_OK,RemoteErrorAsJSON('POST required.'))
      else if not IntegerFieldOf(ARequest.Content,'index',LIndex) then
        SendJSON(AResponse,HTTP_CODE_OK,RemoteErrorAsJSON('Malformed request.'))
      else
        begin
          LError:=RemoteSelect(LIndex);
          if LError.IsEmpty then
            SendJSON(AResponse,HTTP_CODE_OK,'{"ok":true}')
          else
            SendJSON(AResponse,HTTP_CODE_OK,RemoteErrorAsJSON(LError));
        end;
    '/api/remote/search':
      if ARequest.Method<>'POST' then
        SendJSON(AResponse,HTTP_CODE_OK,RemoteErrorAsJSON('POST required.'))
      else if not StringFieldOf(ARequest.Content,'q',LValue) then
        SendJSON(AResponse,HTTP_CODE_OK,RemoteErrorAsJSON('Malformed request.'))
      else
        begin
          LError:=RemoteSearch(LValue);
          if LError.IsEmpty then
            SendJSON(AResponse,HTTP_CODE_OK,'{"ok":true}')
          else
            SendJSON(AResponse,HTTP_CODE_OK,RemoteErrorAsJSON(LError));
        end;
    '/api/stations':
      if ARequest.Method='POST' then
        begin
          LError:=SaveStationsFromJSON(ARequest.Content);
          if LError.IsEmpty then
            SendJSON(AResponse,HTTP_CODE_OK,'{"ok":true}')
          else
            SendJSON(AResponse,HTTP_CODE_OK,ErrorAsJSON(LError));
        end
      else
        SendJSON(AResponse,HTTP_CODE_OK,StationsAsJSON);
  else
    SendJSON(AResponse,HTTP_CODE_NOT_FOUND,'{"error":"Not found."}');
  end;
end;

function WebGUIServerThread(AP: Pointer): PtrInt;
begin
  Result:=0;
  try
    WebGUIServer:=TWebGUIServer.Create;
  except
    on E: Exception do
      Logging(ltError, string.Join(' ',[WEBGUI_SERVICE+':',MSG_ERROR,'('+E.Message+')']));
  end;
end;

function StartWebGUIServer: boolean;
begin
  Result:=False;
// Without this the editor would be an unauthenticated way to rewrite config
// files, which is not something to leave to whoever can reach the port.
  if WebGUIPassword.IsEmpty then
    begin
      Logging(ltError, WEBGUI_SERVICE+': no password set. Set WebGUIPassword in retuner.ini; not starting.');
      Exit;
    end;
  WebGUIThreadID:=BeginThread(@WebGUIServerThread);
  Result:=True;
end;

procedure StopWebGUIServer;
begin
  if Assigned(WebGUIServer) then
    FreeAndNil(WebGUIServer);
end;

end.
