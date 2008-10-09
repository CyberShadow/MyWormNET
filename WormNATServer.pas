unit WormNATServer;
// a proxy for WormNAT routing

interface
uses
{$IFDEF WIN32}
  Windows, WinSock,
{$ELSE}
  Sockets, FakeWinSock,
{$ENDIF}
  Classes, Class_Server;

type
  TLinkType=(ltServer, ltClient);

  TLink=class (TThread)          // pumps data between two sockets
    LinkType: TLinkType;
    ServerNickname, ClientNickname: string;
    ServerAddress, ClientAddress: string;
    ServerSocket, ClientSocket: TSocket;
    procedure Execute; override;
    end;

  procedure StartWormNATServer;
  procedure PrepareLink(Server, Client: TUser);

var
  Links: array of TLink;
  WNATPort: integer;

implementation
uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  SysUtils;

procedure TLink.Execute;
var
  S: string;
  R, Bytes, I, N: Integer;
  ReadSet: record
    count: u_int;
    Socket: TSocket;
    end;
  TimeVal: TTimeVal;
begin
  try
    repeat
      // Client -> Server
      repeat
        ReadSet.count:=1;
        ReadSet.Socket:=ClientSocket;
        TimeVal.tv_sec:=0;
        TimeVal.tv_usec:=10000;  // 10 ms
        R:=select(ClientSocket+1, @ReadSet, nil, nil, @TimeVal);
        if R=SOCKET_ERROR then
          raise Exception.Create('Client select() error.');

        if (ReadSet.count=0)or(R=0) then
          Break;         // nothing to read

        R:=ioctlsocket(ClientSocket, FIONREAD, Bytes);
        if R=SOCKET_ERROR then
          raise Exception.Create('Client connection error.');

        if Bytes=0 then  // software disconnect
          raise Exception.Create('Client connection error (Graceful disconnect).');

        SetLength(S, Bytes);
        R:=recv(ClientSocket, S[1], Bytes, 0);
        if(R=0)or(R=SOCKET_ERROR)then
          raise Exception.Create('Client connection error.');
        SetLength(S, R);
        send(ServerSocket, S[1], Length(S), 0);
      until False;

      // Server -> Client
      repeat
        ReadSet.count:=1;
        ReadSet.Socket:=ServerSocket;
        R:=select(0, @ReadSet, nil, nil, @TimeVal);
        if R=SOCKET_ERROR then
          raise Exception.Create('Server select() error.');

        if (ReadSet.count=0)or(R=0) then
          Break;         // nothing to read

        R:=ioctlsocket(ServerSocket, FIONREAD, Bytes);
        if R=SOCKET_ERROR then
          raise Exception.Create('Server connection error.');

        if Bytes=0 then  // software disconnect
          raise Exception.Create('Server connection error (Graceful disconnect).');

        SetLength(S, Bytes);
        R:=recv(ServerSocket, S[1], Bytes, 0);
        if(R=0)or(R=SOCKET_ERROR)then
          raise Exception.Create('Server connection error.');
        SetLength(S, R);
        send(ClientSocket, S[1], Length(S), 0);
      until False;
    until False;
  except
    on E: Exception do
    end;
  closesocket(ServerSocket);
  closesocket(ClientSocket);

  // TODO: add some sync lock or something here
  N:=-1;
  for I:=0 to Length(Links)-1 do
    if Links[I]=Self then
      N:=I;
  for I:=N to Length(Links)-2 do
    Links[I]:=Links[I+1];
  SetLength(Links, Length(Links)-1);
  FreeOnTerminate:=True;
end;

// ***************************************************************

procedure PrepareLink(Server, Client: TUser);
var
  Link: TLink;
begin
  Link:=TLink.Create(True);
  Link.ServerNickname:=Server.Name;
  Link.ServerAddress:=Server.IP;
  Link.ClientNickname:=Client.Name;
  Link.ClientAddress:=Client.IP;
  Link.ServerSocket:=0;
  Link.ClientSocket:=0;
  SetLength(Links, Length(Links)+1);
  Links[Length(Links)-1]:=Link;
end;

// ***************************************************************

function MainProc(Nothing: Pointer): Integer; stdcall;
var
  m_socket, AcceptSocket: TSocket;
  service, incoming: TSockAddrIn;
  I, T: Integer;
  B: Boolean;
begin
  Result:=0;
  m_socket := socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );

  service.sin_family := AF_INET;
  service.sin_addr.s_addr := inet_addr('0.0.0.0');
  service.sin_port := htons(WNATPort);

  if bind(m_socket, service, sizeof(service))=SOCKET_ERROR then
    begin
    //Log('[WormNAT] bind error.');
    Exit;
    end;
  if listen( m_socket, 1 )=SOCKET_ERROR then
    begin
    //Log('[WormNAT] bind error.');
    Exit;
    end;
  //Log('[WormNAT] Listening on port '+IntToStr(17018)+'.');

  repeat
    T:=SizeOf(incoming);
    AcceptSocket := accept( m_socket, @incoming, @T );
    if AcceptSocket<>INVALID_SOCKET then
      begin
      T:=SizeOf(incoming);
      //Log('[WormNAT] Connection established from '+inet_ntoa(incoming.sin_addr));

      B:=False;
      for I:=0 to Length(Links)-1 do
       with Links[I] do
        begin
        if(ServerAddress=inet_ntoa(incoming.sin_addr))and(ServerSocket=0) then
          begin
          ServerSocket:=AcceptSocket;
          if ClientSocket<>0 then
            Resume;
          B:=True;
          end;
        if(ClientAddress=inet_ntoa(incoming.sin_addr))and(ClientSocket=0) then
          begin
          ClientSocket:=AcceptSocket;
          if ServerSocket<>0 then
            Resume;
          B:=True;
          end;
        end;
      if not B then
        begin
        //Log('[WormNAT] Error: Unexpected connection from '+inet_ntoa(incoming.sin_addr));
        closesocket(AcceptSocket);
        end;
      end
    else
      Sleep(5);
  until False;
end;

var 
  ThreadID: Cardinal = 0;

procedure StartWormNATServer;
begin
  if ThreadID=0 then  // start only once
    CreateThread(nil, 0, @MainProc, nil, 0, ThreadID);
end;

end.
