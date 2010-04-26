unit HTTPServer;
// simplistic HTTP server

interface
uses
  SysUtils, Classes,
{$IFDEF WIN32}
  Windows, WinSock,
{$ELSE}
  FakeWinSock, Sockets,
{$ENDIF}
  IRCServer;

type
  TRequest=class(TThread)
    Socket: TSocket;
    ConnectingFrom: string;
    FileName: string;
    Parameters: TStringList;
    procedure Execute; override;
    procedure SendLn(S: string);
    end;

  TGame=record
    Created: TDateTime;
    Name, Password, Loc: string;
    HosterNickname, HosterAddress: string;
    GameID: Integer;
    end;

var
  Games: array of TGame;
  GameCounter: Integer=0;

procedure StartHTTPServer;

implementation
uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  Base, Data, DateUtils;

procedure CleanUpGames;
var
  I, J: Integer; 
begin
  for I:=Length(Games)-1 downto 0 do
    if MinutesBetween(Games[I].Created, Now)>4 then
      begin
      for J:=I to Length(Games)-2 do
        Games[J]:=Games[J+1];
      SetLength(Games, Length(Games)-1);
      end;
end;

{$I mime.inc}

procedure TRequest.Execute;
var
  Buffer, S, Headers, Body: string;
  I, J, N, R, Bytes: Integer;
  User: TUser;
  Game: TGame;
begin
  try
    Buffer:='';
    repeat
      R:=ioctlsocket(Socket, FIONREAD, Bytes);
      if R=SOCKET_ERROR then
        begin
        Log('[HTTP] '+ConnectingFrom+' Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
        Exit;
        end;
      if Bytes=0 then 
        begin
        Sleep(10);
        Continue;
        end;
      SetLength(S, Bytes);
      R:=recv(Socket, S[1], Bytes, 0);
      if(R=0)or(R=SOCKET_ERROR)then
        begin
        Log('[HTTP] '+ConnectingFrom+' Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
        Exit;
        end;
      SetLength(S, R);
      Buffer := Buffer + S;
    until Copy(Buffer, Length(Buffer)-3, 4)=#13#10#13#10;      // ends with an empty line

    GetLine(Buffer, S);
    Log('[HTTP] '+ConnectingFrom+' '+S);
    if Copy(S, 1, 4)<>'GET ' then
      raise Exception.Create('Only GET requests are supported');
    Delete(S, 1, 4);
    S:=Copy(S, 1, Pos(' ', S+' ')-1);
    if LowerCase(Copy(S, 1, 7))='http://' then
      begin
      Delete(S, 1, 7);
      Delete(S, 1, Pos('/', S)-1);
      end;
    if Copy(S, 1, 2)='//' then   // workaround to some dumb bug
      begin
      Delete(S, 1, 2);
      Delete(S, 1, Pos('/', S));
      end; 
    while Copy(S, 1, 1)='/' do
      Delete(S, 1, 1);
    if Copy(S, 1, 15)='wormageddonweb/' then
      Delete(S, 1, 15);
    FileName:=Copy(S, 1, Pos('?', S+'?')-1);
    Delete(S, 1, Pos('?', S));
    S:=S+'&';
    Parameters:=TStringList.Create;
    Parameters.NameValueSeparator:='=';
    while S<>'' do
    begin
      Parameters.Add(Copy(S, 1, Pos('&', S)-1));
      Delete(S, 1, Pos('&', S));
    end;

    //while GetLine(Buffer, S) do
    //  WriteLn('> ' + S);

    Headers:='HTTP/1.0 200 OK'#13#10;
    Headers:=Headers+'X-Powered-By: MyWormNET'#13#10;
    Headers:=Headers+'X-Test: BlaBla'#13#10;
    Body:='';
    if FileName='Login.asp' then
    begin
      if ConnectingFrom='127.0.0.1' then
        Body:='<CONNECT 127.0.0.1>'
      else
        Body:='<CONNECT '+ServerHost+'>';
    end
    else
    if FileName='RequestChannelScheme.asp' then
      Body:='<SCHEME=Pf,Be>'
    else
    // Cmd=Create&Name=�CyberShadow-MD&HostIP=cybershadow.no-ip.org&Nick=CyberShadow-MD&Pwd=123&Chan=AnythingGoes&Loc=40&Type=0 HTTP/1.0
    if FileName='Game.asp' then
      if Parameters.Values['Cmd']='Create' then
        begin
        Inc(GameCounter);
        Game.Name:=Parameters.Values['Name'];
        if Length(Game.Name) > 29 then Game.Name := Copy(Game.Name, 1, 29);
        Game.Password:=Parameters.Values['Pwd'];
        Game.Loc:=Parameters.Values['Loc'];
        Game.GameID:=GameCounter;
        Game.Created:=Now;

        if IRCPort>0 then
          begin
          User:=nil;
          for I:=0 to Length(Users)-1 do                      // this check also matches the IP address
            if (Users[I].Nickname=Parameters.Values['Nick']){and(Users[I].ConnectingFrom=ConnectingFrom)} then
              User:=Users[I];
          if User=nil then
            raise Exception.Create('Can''t find IRC user "'+Parameters.Values['Nick']+'".');

          //if(Pos('http://wormnat.xeon.cc/', Parameters.Values['HostIP'])<>0)or(User.ConnectingFrom='127.0.0.1')or(Copy(User.ConnectingFrom, ) then
          //Game.HosterAddress:=Parameters.Values['HostIP'];
          //else
          if Pos(':', Parameters.Values['HostIP'])>0 then
            Game.HosterAddress:=User.ConnectingFrom + Copy(Parameters.Values['HostIP'], Pos(':', Parameters.Values['HostIP']), 1000)
          else
            Game.HosterAddress:=User.ConnectingFrom;  // auto-detect the user's external address
          Game.HosterNickname:=User.Nickname;
          end
        else
          begin
          Game.HosterNickname:=Parameters.Values['Nick'];
          Game.HosterAddress:=Parameters.Values['HostIP'];
          end;
        SetLength(Games, Length(Games)+1);
        Games[Length(Games)-1]:=Game;

        for I:=0 to Length(Users)-1 do
          if Users[I].InChannel then
            Users[I].SendLn(':'+ServerHost+' NOTICE '+IRCChannel+' :'+Game.HosterNickname+' has created a game ("'+Game.Name+'").');
        EventLog(Game.HosterNickname+' ('+Game.HosterAddress+') has created a game ("'+Game.Name+'").');

        Headers:=Headers+'SetGameId: : '+IntToStr(Game.GameID)+#13#10;
        Body:='<NOTHING>';
        end
      else
      if Parameters.Values['Cmd']='Close' then
        begin
        N:=-1;
        for I:=0 to Length(Games)-1 do
          if IntToStr(Games[I].GameID)=Parameters.Values['GameID'] then
            begin
            Game:=Games[I];
            for J:=I to Length(Games)-2 do
              Games[J]:=Games[J+1];
            SetLength(Games, Length(Games)-1);
            N:=I;
            end;
        if N=-1 then
          //raise Exception.Create('No such game');
          Log('Trying to close a non-existant game ('+Parameters.Values['GameID']+')')
        else
          begin
          for I:=0 to Length(Users)-1 do
            if Users[I].InChannel then
              Users[I].SendLn(':'+ServerHost+' NOTICE '+IRCChannel+' :'+Game.HosterNickname+'''s game "'+Game.Name+'" has closed.');
          EventLog(Game.HosterNickname+'''s game "'+Game.Name+'" has closed.');
          end;
        end
      else
      if Parameters.Values['Cmd']='Failed' then        // ?
        begin
        Body:='<NOTHING>';
        end
      else
        raise Exception.Create('Unknown game command - '+Parameters.Values['Cmd'])
    else
    if FileName='GameList.asp' then
      begin
      CleanUpGames;
      Body:=Body+'<GAMELISTSTART>'#13#10;
      for I:=0 to Length(Games)-1 do
       with Games[I] do
        Body:=Body+'<GAME '+Name+' '+HosterNickname+' '+HosterAddress+' '+Loc+' 1 0 '+IntToStr(GameID)+' 0><BR>'#13#10;
      Body:=Body+'<GAMELISTEND>'#13#10;
      end
    else
    if FileName='UpdatePlayerInfo.asp' then
      // ignore
    else
      begin
      for I:=Length(FileName) downto 1 do
        if(FileName[I]='/')and(PathDelim='\') then
          FileName[I]:=PathDelim
        else
        if FileName[I]='%' then
          begin
          FileName[I]:=Chr(StrToInt('$'+Copy(FileName, I+1, 2)));
          Delete(FileName, I+1, 2);
          end;
      if Pos('..', FileName)+Pos(PathDelim+PathDelim, FileName)<>0 then
        raise Exception.Create('hmm hmm hmm');
      if(FileName='')or(FileName[Length(FileName)]=PathDelim)then
        FileName:=FileName+'index.html';
      if FileExists('wwwroot'+PathDelim+FileName) then
        begin
        Log('[HTTP] '+ConnectingFrom+' Sending file '+FileName);
        S:='application/octet-stream';
        for I:=1 to High(MimeTypes) do
          if '.'+MimeTypes[I].Extension=ExtractFileExt(FileName) then
            S:=MimeTypes[I].MimeType;
        Headers:=Headers+'Content-Type: '+S+#13#10;
        Body:=GetFile('wwwroot'+PathDelim+FileName);
        Headers:=Headers+'Content-Length: '+IntToStr(Length(Body))+#13#10;
        end
      else
        raise Exception.Create('"File" not found - '+FileName);
      end;

    S:=Headers+#13#10+Body;
    SendLn(S);
    Parameters.Free;
  except
    on E: Exception do
      try
        Log('[HTTP] Error with '+ConnectingFrom+' : '+E.Message);
        S:=Headers+'Error: : '+E.Message+#13#10#13#10+'Error: '+E.Message;
        SendLn(S);
      except
        end;
    end;
  closesocket(Socket);
  FreeOnTerminate:=True;
end;


procedure TRequest.SendLn(S: string);
begin
  //WriteLn('> '+S);
  S:=S+#13#10;
  if send(Socket, S[1], Length(S), 0)<>Length(S) then
    Log('[HTTP > Failed ('+WinSockErrorCodeStr(WSAGetLastError)+') ]');
end;

// ***************************************************************

function MainProc(Nothing: Pointer): Integer; stdcall;
var
  m_socket, AcceptSocket: TSocket;
  service, incoming: TSockAddrIn;
  T: Integer;
  Request: TRequest;
begin
  Result:=0;
  m_socket := socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );

  service.sin_family := AF_INET;
  service.sin_addr.s_addr := inet_addr( '0.0.0.0' );
  service.sin_port := htons( HTTPPort );

  if bind(m_socket, service, sizeof(service))=SOCKET_ERROR then
    begin
    Log('[HTTP] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  if listen( m_socket, 50 )=SOCKET_ERROR then
    begin
    Log('[HTTP] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  Log('[HTTP] Listening on port '+IntToStr(HTTPPort)+'.');

  repeat
    T:=SizeOf(incoming);
    AcceptSocket := accept( m_socket, @incoming, @T );
    if AcceptSocket<>INVALID_SOCKET then
      begin
      T:=SizeOf(incoming);
      WriteLn('[HTTP] Connection established from '+inet_ntoa(incoming.sin_addr));

      Request:=TRequest.Create(True);
      Request.Socket:=AcceptSocket;
      Request.ConnectingFrom:=inet_ntoa(incoming.sin_addr);
      Request.Resume;
      end
    else
      Sleep(1);
  until False;
end;

var
  ThreadID: Cardinal = 0;

procedure StartHTTPServer;
begin
  if ThreadID=0 then  // start only once
    CreateThread(nil, 0, @MainProc, nil, 0, ThreadID);
end;


end.
