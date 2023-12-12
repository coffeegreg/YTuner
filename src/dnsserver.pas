unit dnsserver;

// YTuner: DNS proxy serwer unit.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  IdUDPServer, IdDNSServer, IdDNSCommon, IdGlobal, IdSocketHandle,
  common;

type
  TIdDNSServerProxy = class(TObject)
    IdDNSServer: TIdDNSServer;
    procedure IdDNS_UDPServerDoAfterQuery(ABinding: TIdSocketHandle; ADNSHeader: TDNSHeader; var QueryResult: TIdBytes; var ResultCode: string; Query : TIdBytes);
  end;

const
  DNS_SERVICE = 'DNS Service';

var
  IdDNSServerProxy: TIdDNSServerProxy;
  DNSServerIPAddress: string;
  DNSServerPort: integer = 53;
  DNSServerEnabled: boolean = True;
  DNSServers: string = '8.8.8.8,9.9.9.9';
  InterceptDNs: string = '*.vtuner.com';
  DNSAnswerBytes: TBytes = ($C0,$0C,          // Name (Bin=1100000000001100 Dec=12). Pointer to first occurrence of the name.
                            $00,$01,          // Type: A (Host Address)
                            $00,$01,          // Class: IN
                            $00,$00,$0E,$10,  // Time to live = 3600s
                            $00,$04,          // Data length = 4
                            $00,$00,$00,$00); // 4 bytes reserved for IPv4 Address

function StartDNSServer:boolean;

implementation

function StartDNSServer:boolean;
begin
  Result:=True;
  IdDNSServerProxy:=TIdDNSServerProxy.Create;
  IdDNSServerProxy.IdDNSServer:=TIdDNSServer.Create(nil);
  try
    with IdDNSServerProxy.IdDNSServer do
      begin
        TCPACLActive:=False;
        ServerType:=stPrimary;
      end;
    with IdDNSServerProxy.IdDNSServer.UDPTunnel do
      begin
        BufferSize:=8192;
        DefaultPort:=DNSServerPort;
        ThreadedEvent:=True;
        RootDNS_NET.Clear;
        RootDNS_NET.CommaText:=DNSServers;
        OnAfterQuery:=@IdDNSServerProxy.IdDNS_UDPServerDoAfterQuery;
      end;
    with IdDNSServerProxy.IdDNSServer.UDPTunnel.Bindings.Add do
      begin
        IP:=DNSServerIPAddress;
        IPVersion:=Id_IPv4;
      end;
    IdDNSServerProxy.IdDNSServer.Active:=True;
  except
    On E: Exception do
      begin
        Result:=False;
        Logging(ltError, 'DNS Server error: '+E.Message);
        if Assigned(IdDNSServerProxy.IdDNSServer) then
          IdDNSServerProxy.IdDNSServer.Free;
        if Assigned(IdDNSServerProxy) then
          IdDNSServerProxy.Free;
      end;
  end;
end;

procedure TIdDNSServerProxy.IdDNS_UDPServerDoAfterQuery(ABinding: TIdSocketHandle; ADNSHeader: TDNSHeader; var QueryResult: TIdBytes; var ResultCode: string; Query : TIdBytes);
var
  LInterceptDN: string;
  LDNQuery: string;
  LQueryResult: TBytes;

  function ReplaceSpecSymbol(S: String): String;
  var
    Count : Integer;
  begin
    Count:=0;
    Result:='';
    while True do
      begin
        Count:=Ord(S.Chars[0]);
        Result:=Result+S.Substring(1,Count)+'.';
        S:=S.Remove(0,Count+1);
        if Ord(S.Chars[0])=0 Then
          Break;
      end;
    Result:=Result.TrimRight(['.']);
  end;

begin
  LDNQuery:=ReplaceSpecSymbol(BytesToString(Query,12));
  for LInterceptDN in InterceptDNs.Split([',']) do
    begin
      if ((LInterceptDN.IndexOf('*')=0) and LDNQuery.EndsWith(LInterceptDN.Remove(0,1),True))
        or ((LInterceptDN.IndexOf('*')=-1) and (LDNQuery=LInterceptDN)) then
        begin
          if (ToHex(Query,4,2)='01000001')                           // Standard query & Questions: 1
            and (ToHex(Query,4,Length(Query)-4)='00010001') then     // Type "A" & Class "IN"
            begin
              Logging(ltDebug, 'DNS Query intercept : '+LDNQuery);
              AppendBytes(LQueryResult,Query);
              LQueryResult[2]:=$81;            //Flags: 0x8180 Standard query response, No error
              LQueryResult[3]:=$80;            //Flags: 0x8180 Standard query response, No error
              LQueryResult[4]:=$00;            //Questions: 1
              LQueryResult[5]:=$01;            //Questions: 1
              LQueryResult[6]:=$00;            //Answer RRs: 1
              LQueryResult[7]:=$01;            //Answer RRs: 1

              AppendBytes(LQueryResult,DNSAnswerBytes);
              LQueryResult[Length(LQueryResult)-4]:=StrToInt(ABinding.IP.Split(['.'])[0]);
              LQueryResult[Length(LQueryResult)-3]:=StrToInt(ABinding.IP.Split(['.'])[1]);
              LQueryResult[Length(LQueryResult)-2]:=StrToInt(ABinding.IP.Split(['.'])[2]);
              LQueryResult[Length(LQueryResult)-1]:=StrToInt(ABinding.IP.Split(['.'])[3]);
              ResultCode:='RC';
              QueryResult:=LQueryResult;
            end
          else
            begin
              ResultCode:='NA';
              QueryResult:=Query;
            end;
          Break;
        end;
    end;
end;

end.

