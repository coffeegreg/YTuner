unit maintenance;

// YTuner: Maintenance service(s). Diagnostic, maintenance and future goals.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  fphttpapp, fphttpclient, fphttpserver, avr;

type
  TMaintenaceHTTPServer = Class
  Private
    FServer : TFPHTTPServer;
  Public
    constructor Create;
    procedure DoHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    property Server : TFPHTTPServer Read FServer Write FServer;
  end;

const
  MAINTENANCE_SERVICE = 'Maintenance Service';
  MSG_SERVICE = 'service';
  MSG_SHUTTING_DOWN = 'Shutting down..';
  MSG_DONE = 'Done';

var
  MaintenanceServiceEnabled: boolean = False;
  MaintenanceServerIPAddress: string = '127.0.0.1';
  MaintenanceServerPort: integer = 8080;

function MaintenaceHTTPServerThread(AP:Pointer):PtrInt;
procedure StartMaintenaceHTTPServer;
procedure YTunerServiceDown(Sender: TObject);

implementation

uses httpserver, dnsserver, threadtimer, radiobrowser, radiobrowserdb, my_stations, common;

procedure StartMaintenaceHTTPServer;
begin
  with TMaintenaceHTTPServer.Create do
    try
      FServer.KeepConnectionTimeout:=0;
      FServer.KeepConnections:=False;
      Server.Free;
      Sleep(3000);
    finally
      Free;
    end;
end;

function MaintenaceHTTPServerThread(AP:Pointer):PtrInt;
begin
  Logging(ltInfo, MAINTENANCE_SERVICE+': listening on: '+MaintenanceServerIPAddress+':'+MaintenanceServerPort.ToString);
  StartMaintenaceHTTPServer;
end;

constructor TMaintenaceHTTPServer.Create;
begin
  FServer:=TFPHTTPServer.Create(Nil);
  with FServer do
    begin
      Active:=False;
//      Address:=MaintenanceServerIPAddress;   //New Trunk FPC only!
      Port:=MaintenanceServerPort;
      ThreadMode:=tmNone;
      OnRequest:=@DoHandleRequest;
      Active:=True;
    end;
end;

procedure TMaintenaceHTTPServer.DoHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
begin
  if ARequest.Host.StartsWith(MaintenanceServerIPAddress) then    //Unnecessary with TFPHTTPServer.Address setting. (New Trunk FPC only!)
    begin
      ServerResponse(HTTP_CODE_OK,ctNone,AResponse,string.Join(': ',[APP_NAME+' '+MSG_SERVICE,MSG_SHUTTING_DOWN+'...']));
      case ARequest.URI.ToLower of
        '/'+PATH_ROOT+'/down' : YTunerServiceDown(Sender);
      end;
    end
  else
    ServerResponse(HTTP_CODE_UNAVAILABLE,ctNone,AResponse,'Service unavailable.');
end;

procedure DNSServiceDown;
begin
  if Assigned(IdDNSServerProxy) then
    begin
      if Assigned(IdDNSServerProxy.IdDNSServer) then
        begin
          IdDNSServerProxy.IdDNSServer.Active:=False;
          IdDNSServerProxy.IdDNSServer.UDPTunnel.Active:=False;
          IdDNSServerProxy.IdDNSServer.UDPTunnel.Bindings.Clear;
          IdDNSServerProxy.IdDNSServer.Free;
        end;
      IdDNSServerProxy.Free;
    end;
end;

procedure MSThreadTimerDown;
begin
  if Assigned(MSThread) then
    with MSThread do
      begin
        if Suspended then
          Start;
        TerminateTimer;
      end;
end;

procedure RBThreadTimerDown;
begin
  if Assigned(RBThread) then
    with RBThread do
      begin
        if Suspended then
          Start;
        TerminateTimer;
      end;
end;

procedure GTThreadTimerDown;
begin
  if Assigned(GTThread) then
    with GTThread do
      begin
        if Suspended then
          Start;
        TerminateTimer;
      end;
end;

procedure YTunerServiceDown(Sender: TObject);
var
  i: integer;
begin
  Logging(ltInfo,string.Join(': ',[APP_NAME,MSG_SHUTTING_DOWN+'. Please wait..']));
  Logging(ltInfo,string.Join(': ',[APP_NAME,'Freeing up resources..']));
  for i:=0 to Length(AVRConfigArray)-1 do
     if AVRConfigArray[i].Translator.TranslatorItems <> nil then
       AVRConfigArray[i].Translator.TranslatorItems.Free;
  if RadioBrowserEnabled and (RBCacheType in [catDB, catMemDB, catPermMemDB]) and (Assigned(DBRBMainConnection))then
    begin
      Logging(ltInfo,string.Join(': ',[MSG_RBDB_CLOSING,MSG_RBDB_DB,MSG_RBDB_CONNECTION]));
      if DBRBMainConnection.Connected then;
        DBRBMainConnection.Connected:=False;
      FreeAndNil(DBRBMainConnection);
    end;
  if RadioBrowserEnabled and ((RBCacheTTL>0) or (RBUUIDsCacheAutoRefresh and (RBUUIDsCacheTTL>0))) then
    begin
      Logging(ltInfo,string.Join(': ',[RB_THREAD,MSG_SHUTTING_DOWN]));
      try
        RBThreadTimerDown;
      except end;
      Logging(ltInfo,string.Join(': ',[RB_THREAD,MSG_DONE]));
    end;
  if MyStationsEnabled and (MyStationsAutoRefreshPeriod>0) then
    begin
      Logging(ltInfo,string.Join(': ',[MS_THREAD,MSG_SHUTTING_DOWN]));
      try
        MSThreadTimerDown;
      except end;
      Logging(ltInfo,string.Join(': ',[MS_THREAD,MSG_DONE]));
    end;
  if Assigned(GTThread) then
    begin
      Logging(ltInfo,string.Join(': ',[GT_THREAD,MSG_SHUTTING_DOWN]));
      try
        GTThreadTimerDown;
      except end;
      Logging(ltInfo,string.Join(': ',[GT_THREAD,MSG_DONE]));
    end;
  if DNSServerEnabled then
    begin
      Logging(ltInfo,string.Join(': ',[DNS_SERVICE,MSG_SHUTTING_DOWN]));
      try
        DNSServiceDown;
      except end;
      Logging(ltInfo,string.Join(': ',[DNS_SERVICE,MSG_DONE]))
    end;

  if (Sender is TFPHTTPServer) then
    begin
      Logging(ltInfo,string.Join(': ',[MAINTENANCE_SERVICE,MSG_SHUTTING_DOWN]));
      TFPHTTPServer(Sender).Active:=False;
      Sleep(3000);
      Logging(ltInfo,string.Join(': ',[MAINTENANCE_SERVICE,MSG_DONE]));
    end;

  Logging(ltInfo,string.Join(': ',[WEB_SERVICE,MSG_SHUTTING_DOWN]));
  try
    Application.Terminate;
  except end;
  Logging(ltInfo,string.Join(': ',[WEB_SERVICE,MSG_DONE]));
  try
    with TFPHttpClient.Create(nil) do
      try
        SimpleGet('http://'+WebServerIPAddress+':'+WebServerPort.ToString);
      finally
        Free;
      end;
  except
  end;
end;

end.

