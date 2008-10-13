unit IRCServer;
// a quick hack of an IRC server, which supports only one channel

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface
uses
{$IFDEF WIN32}
  Windows, WinSock,
{$ELSE}
  Sockets, FakeWinSock,
{$ENDIF}
  Classes;

type
  TUser=class (TThread)
    ConnectingFrom: string;
    Nickname, Username, Hostname, Servername, Realname: string;
    Socket: TSocket;
    InChannel: Boolean;
    Modes: array[char] of Boolean;
    procedure Execute; override;
    procedure SendLn(S: string);
    end;

var
  Users: array of TUser;

procedure StartIRCServer;
procedure LogToOper(S: string);

resourcestring
  IRCPassword='ELSILRACLIHP ';
  IRCPassword2='ELSILRACLIHP';
  ValidNickChars='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789`_-|';

implementation
uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  Base, Data, SysUtils, HTTPServer, WormNATServer;

procedure TUser.Execute;
var
  Buffer, S, S2, Command, Target: string;
  R, Bytes, I, N: Integer;
  PingTimer: Integer;
  B: Boolean;
  ReadSet, ErrorSet: record
    count: u_int;
    Socket: TSocket;
    end;
  TimeVal: TTimeVal;
  User: TUser;
  Password: string;
  C: Char;
  
  procedure LogIn;
  var I: Integer;
  begin
    EventLog(Nickname+' ('+ConnectingFrom+') logged in.');
    SendLn(':'+ServerHost+' 001 '+Nickname+' :Welcome, '+Nickname+' !');
    SendLn(':'+ServerHost+' 002 '+Nickname+' :This is a minimal WormNet-compatible IRC server emulator,');
    SendLn(':'+ServerHost+' 003 '+Nickname+' :supporting only the base set of IRC features.');
    SendLn(':'+ServerHost+' 004 '+Nickname+' :The server software was written by ');
    SendLn(':'+ServerHost+' 005 '+Nickname+' :The_CyberShadow <thecybershadow@gmail.com>');
    if WormNATPort>0 then
      SendLn(':'+ServerHost+' 006 '+Nickname+' :[WormNATRouteOn:'+IntToStr(WormNATPort)+'] This server supports built-in WormNAT routing.');
    //SendLn(':'+ServerHost+' 007 '+Nickname+' :[YourIP:'+ConnectingFrom+'] Your external IP address is '+ConnectingFrom+'.');
    //SendLn(':'+ServerHost+' 004 '+Nickname+' wormnet1.team17.com 2.8/hybrid-6.3.1 oOiwszcrkfydnxb biklmnopstve');
    //SendLn(':'+ServerHost+' 005 '+Nickname+' WALLCHOPS PREFIX=(ov)@+ CHANTYPES=#& MAXCHANNELS=20 MAXBANS=25 NICKLEN=15 TOPICLEN=120 KICKLEN=90 NETWORK=EFnet CHANMODES=b,k,l,imnpst MODES=4 :are supported by this server');
    SendLn(':'+ServerHost+' 251 '+Nickname+' :There are '+IntToStr(Length(Users))+' users on the server.');
    N:=0;
    for I:=0 to Length(Users)-1 do
      if Users[I].Modes['o'] then
        Inc(N);
    SendLn(':'+ServerHost+' 252 '+Nickname+' '+IntToStr(N)+' :IRC Operators online');
    SendLn(':'+ServerHost+' 254 '+Nickname+' 1 :channel hard-coded limit');
    SendLn(':'+ServerHost+' 375 '+Nickname+' :- '+ServerHost+' Message of the Day - ');
    S:=GetFile('motd.txt')+#13#10;
    while GetLine(S, S2) do
     if(S<>'')or(S2<>'') then
      SendLn(':'+ServerHost+' 372 '+Nickname+' :- '+S2);
    SendLn(':'+ServerHost+' 376 '+Nickname+' :End of /MOTD command.');
  end;
  
begin
  try
    Buffer:='';
    PingTimer:=0;
    Password:='';
    repeat
      repeat
        ReadSet.count:=1;
        ReadSet.Socket:=Socket;
        ErrorSet.count:=1;
        ErrorSet.Socket:=Socket;
        TimeVal.tv_sec:=0;
        TimeVal.tv_usec:=10000;
        R:=select(Socket+1, @ReadSet, nil, @ErrorSet, @TimeVal);
        if (R=SOCKET_ERROR) or (ErrorSet.count>0) then
          begin
          Log('[IRC] '+ConnectingFrom+' select() error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create('Connection error.');
          end;

        if (ReadSet.count=0)or(R=0) then
          Break;  // nothing to read

        R:=ioctlsocket(Socket, FIONREAD, Bytes);
        if R=SOCKET_ERROR then
          begin
          Log('[IRC] '+ConnectingFrom+' Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create('Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          end;

        if Bytes=0 then  // software disconnect
          begin
          Log('[IRC] '+ConnectingFrom+' Connection error (Graceful disconnect).');
          raise Exception.Create('Software disconnect');
          end;

        SetLength(S, Bytes);
        R:=recv(Socket, S[1], Bytes, 0);
        if(R=0)or(R=SOCKET_ERROR)then
          begin
          Log('[IRC] '+ConnectingFrom+' Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create('Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+')');
          end;
        SetLength(S, R);
        Buffer := Buffer + S;
        PingTimer:=0;
      until False;

      while GetLine(Buffer, S) do
        begin
        WriteLn('< '+S);
        Command:=UpperCase(Copy(S, 1, Pos(' ', S+' ')-1));
        Delete(S, 1, Length(Command)+1);
        if Command='PING' then
          SendLn('PONG :'+ServerHost)
        else
        if Command='PONG' then 
        else
        if Command='PASS' then {ignore}
          begin
          Password:=S;
          {if (Password<>IRCPassword) and (Password<>IRCPassword2) then
            begin
            SendLn(':'+ServerHost+' 464 '+S+' :Password incorrect');
            raise Exception.Create('Bad password!');
            end;}
          end
        else
        if Command='NICK' then
        begin
          {if (Password<>IRCPassword) and (Password<>IRCPassword2) then
            begin
            SendLn(':'+ServerHost+' 464 '+S+' :Password incorrect');
            raise Exception.Create('Bad password!');
            end;}
          
          if Nickname<>'' then
            SendLn(':'+ServerHost+' 400 :Nick change isn''t supported.')
          else
          begin
            for I:=Length(S) downto 1 do
              if Pos(S[I], ValidNickChars)=0 then
                Delete(S, I, 1);
            if S='' then
              SendLn(':'+ServerHost+' 432 '+S+' :Erroneous nickname')
            else
            begin
              B := False;
              for I:=0 to Length(Users)-1 do
                if UpperCase(Users[I].Nickname)=UpperCase(S) then
                  B := True;
              if B then
                SendLn(':'+ServerHost+' 433 '+S+' :Nickname is already in use')
              else
                Nickname:=S;
              if UserName<>'' then
                LogIn;
            end;
          end;
        end
        else
        // USER Username hostname servername :40 0 RO 
        if Command='USER' then
          begin
            Username:=Copy(S, 1, Pos(' ', S)-1);
            Delete(S, 1, Pos(' ', S));
            Hostname:=Copy(S, 1, Pos(' ', S)-1);
            Delete(S, 1, Pos(' ', S));
            Servername:=Copy(S, 1, Pos(' ', S)-1);
            Delete(S, 1, Pos(':', S));
            Realname:=S;
          if Nickname<>'' then
            LogIn;
          end
        else
        if Command='QUIT' then
          begin
          // :CyberShadow!cybershado@38F7DF98.502358C0.F6DD7E74.IP QUIT :Input/output error
          if InChannel then
            for I:=0 to Length(Users)-1 do
              if Users[I].InChannel then
                Users[I].SendLn(':'+Nickname+'!'+Username+'@'+ConnectingFrom+' QUIT :'+Copy(S, 2, 1000));
          InChannel := False;
          Break
          end
        else
        if Command='JOIN' then
          begin
          if Nickname='' then
            SendLn(':'+ServerHost+' 451 :Register first.')
          else
          if InChannel then
            SendLn(':'+ServerHost+' 403 '+Nickname+' '+S+' :You already are in a channel')
          else
          if S=IRCChannel then
            begin
            EventLog(Nickname+' ('+ConnectingFrom+') has joined #'+IRCChannel);
            InChannel:=True;
            //:CyberShadow-MD!Username@no.address.for.you JOIN :#AnythingGoes
            for I:=0 to Length(Users)-1 do
              if Users[I].InChannel then
                begin
                Users[I].SendLn(':'+Nickname+'!'+Username+'@'+ConnectingFrom+' JOIN :'+IRCChannel);
                if Modes['o'] then
                  Users[I].SendLn(':'+ServerHost+' MODE '+IRCChannel+' +o '+Nickname);
                end;
            S:=':'+ServerHost+' 353 '+Nickname+' = '+IRCChannel+' :';
            for I:=0 to Length(Users)-1 do
              if Users[I].InChannel then
                begin
                if Users[I].Modes['o'] then
                  S:=S+'@';
                S:=S+Users[I].Nickname+' ';
                end;
            SendLn(S);
            SendLn(':'+ServerHost+' 366 '+Nickname+' '+IRCChannel+' :End of /NAMES list.');
            end
          else
            SendLn(':'+ServerHost+' 403 '+Nickname+' '+S+' :No such channel');
          end
        else
        if Command='PART' then
          begin
          if InChannel then
            begin
            EventLog(Nickname+' ('+ConnectingFrom+') has left #'+IRCChannel);
            for I:=0 to Length(Users)-1 do
              if Users[I].InChannel then
                Users[I].SendLn(':'+Nickname+'!'+Username+'@'+ConnectingFrom+' PART '+S);
            InChannel:=False;
            end;
          end
        else
        if Command='MODE' then
          begin
          Target:=Copy(S, 1, Pos(' ', S+' ')-1);
          Delete(S, 1, Pos(':', S+':')-1);
          if S<>'' then
            SendLn(':'+ServerHost+' 472 '+Nickname+' :Sorry, you can''t set modes for anything.')
          else
            if Target=IRCChannel then
              begin
              SendLn(':'+ServerHost+' 324 '+Nickname+' '+IRCChannel+' +tn');
              end
            else
              begin
              User:=nil;
              for I:=0 to Length(Users)-1 do
                if Users[I].Nickname=Target then
                  User:=Users[I];
              if User=nil then
                SendLn(':'+ServerHost+' 401 '+Nickname+' '+Target+' :No such nick/channel.')
              else
                begin
                S:='';
                for C:=#0 to #255 do
                  if Modes[C] then
                    S:=S+C;
                SendLn(':'+ServerHost+' 324 '+Nickname+' '+Target+' +'+S);
                end;
              end;
          end
        else
        if(Command='PRIVMSG')or(Command='NOTICE') then
          begin
          if Nickname='' then
            SendLn(':'+ServerHost+' 451 :Register first.')
          else
            begin
            Target:=Copy(S, 1, Pos(' ', S+' ')-1);
            Delete(S, 1, Pos(':', S+':')-1);
            if Target=IRCChannel then
              begin
              EventLog('['+IRCChannel+'] <'+Nickname+'> '+Copy(S, 1, 1000));
              for I:=0 to Length(Users)-1 do
                if Users[I].InChannel and (Users[I]<>Self)then
                  Users[I].SendLn(':'+Nickname+'!'+Username+'@'+ConnectingFrom+' '+Command+' '+IRCChannel+' '+S);
              end
            else
              begin
              User:=nil;
              for I:=0 to Length(Users)-1 do
                if LowerCase(Users[I].Nickname)=LowerCase(Target) then
                  User:=Users[I];
              if User=nil then
                SendLn(':'+ServerHost+' 401 '+Nickname+' '+Target+' :No such nick/channel.')
              else
                begin
                Target := User.Nickname;
                EventLog('['+Command+'] <'+Nickname+'> -> <'+Target+'> '+Copy(S, 1, 1000));
                LogToOper('['+Command+'] <'+Nickname+'> -> <'+Target+'> '+Copy(S, 1, 1000));
                User.SendLn(':'+Nickname+'!'+Username+'@'+ConnectingFrom+' '+Command+' '+Target+' '+S);
                end;
              end;
            Sleep(1000); // throttle
            end;
          end
        else
        if Command='OPER' then
          begin
          if Copy(S, 1, Pos(' ', S+' ')-1)<>IRCOperPassword then
            Delete(S, 1, Pos(' ', S+' '));  // ignore username
          if S=IRCOperPassword then
            begin
            EventLog(Nickname+' ('+ConnectingFrom+') has registered as an Operator.');
            Modes['o']:=True;
            SendLn(':'+Nickname+' MODE '+Nickname+' :+o');
            if InChannel then
              for I:=0 to Length(Users)-1 do
                if Users[I].InChannel then
                  Users[I].SendLn(':'+ServerHost+' MODE '+IRCChannel+' +o '+Nickname);
            end
          end
        else
        if Command='WHO' then
          begin
          //:wormnet1.team17.com 352 Alexis #AnythingGoes Username no.address.for.you wormnet1.team17.com TiCPU H :0 TiCpu
          //:wormnet1.team17.com 315 Alexis * :End of /WHO list.
          for I:=0 to Length(Users)-1 do
            if Users[I].InChannel then
              SendLn(':'+ServerHost+' 352 '+Nickname+' '+IRCChannel+' '+Users[I].Username+' '+Users[I].ConnectingFrom+' '+ServerHost+' '+Users[I].Nickname+' H :0 '+Users[I].Realname)
            else
              SendLn(':'+ServerHost+' 352 '+Nickname+' * '+Users[I].Username+' '+Users[I].ConnectingFrom+' '+ServerHost+' '+Users[I].Nickname+' H :0 '+Users[I].Realname);
          SendLn(':'+ServerHost+' 315 '+Nickname+' * :End of /WHO list.');
          end
        else
        if Command='LIST' then
          begin
          N:=0;
          for I:=0 to Length(Users)-1 do
            if Users[I].InChannel then
              Inc(N);
          SendLn(':'+ServerHost+' 321 '+Nickname+' Channel :Users  Name');
          SendLn(':'+ServerHost+' 322 '+Nickname+' '+IRCChannel+' '+IntToStr(N)+' :');
          SendLn(':'+ServerHost+' 323 '+Nickname+' :End of /LIST');
          end
        else
        if Command='EXPECT' then
          begin
          Log('Received EXPECT command from '+ConnectingFrom+' for '+S);;
          User:=nil;
          for I:=0 to Length(Users)-1 do
            if Users[I].Nickname=S then
              User:=Users[I];
          if User=nil then
            SendLn(':'+ServerHost+' 401 '+S+' :No such nick.')
          else
            begin
            SendLn(':'+ServerHost+' NOTICE '+Nickname+' :OK, expecting '+User.Nickname+' from '+User.ConnectingFrom);
            PrepareLink(Self, User);
            end;
          end
        else
        if Command='GAMES' then
          begin
          for I:=0 to Length(Games)-1 do
           with Games[I] do
            SendLn(':'+ServerHost+' NOTICE '+Nickname+' :'+Name+' '+HosterNickname+' '+HosterAddress);
          SendLn(':'+ServerHost+' NOTICE '+Nickname+' :--- '+IntToStr(Length(Games))+' games total ---');
          end
        else
          SendLn(':'+ServerHost+' 421 '+Nickname+' '+Command+' :Unknown command');
        end;

      Inc(PingTimer);
      if PingTimer=18000 then
        SendLn('PING :'+ServerHost);
      if PingTimer=24000 then
        begin
        if InChannel then
          for I:=0 to Length(Users)-1 do
            if Users[I].InChannel then
              Users[I].SendLn(':'+Nickname+'!'+Username+'@'+ConnectingFrom+' QUIT :Ping timeout');
        closesocket(Socket); Socket:=0;
        Break;
        end;
    until Socket=0;
    Log('[IRC] Closing link to '+ConnectingFrom);
    closesocket(Socket);
    
  except
    on E: Exception do
      begin
      if InChannel then
        for I:=0 to Length(Users)-1 do
          if Users[I].InChannel then
            try
              Users[I].SendLn(':'+Nickname+'!'+Username+'@'+ConnectingFrom+' QUIT :'+E.Message);
            except
              end;
      Log('[IRC] Error with '+ConnectingFrom+' : '+E.Message);
      end;
    end;

  if Socket<>0 then
    closesocket(Socket);   // ignore errors
  Socket:=0;

  EventLog(Nickname+' ('+ConnectingFrom+') has disconnected.');

  // TODO: add some sync lock or something here
  N:=-1;
  for I:=0 to Length(Users)-1 do
    if Users[I]=Self then
      N:=I;
  if N=-1 then
    Log(ConnectingFrom+': WTF can''t find myself!')
  else
    begin
    for I:=N to Length(Users)-2 do
      Users[I]:=Users[I+1];
    SetLength(Users, Length(Users)-1);
    end;
  FreeOnTerminate:=True;
end;

procedure TUser.SendLn(S: string);
begin
  if Socket=0 then Exit;
  WriteLn('['+TimeToStr(Now)+'] > '+S);
  S:=S+#13#10;
  if send(Socket, S[1], Length(S), 0)<>Length(S) then
    begin
    Socket:=0;  // avoid infinite recursion
    Log('[IRC > Failed ('+WinSockErrorCodeStr(WSAGetLastError)+') ]');
    end;
end;

// ***************************************************************

function MainProc(Nothing: Pointer): Integer; stdcall;
var
  m_socket, AcceptSocket: TSocket;
  service, incoming: TSockAddrIn;
  T: Integer;
  User: TUser;
begin
  Result:=0;
  m_socket := socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );

  service.sin_family := AF_INET;
  service.sin_addr.s_addr := inet_addr( '0.0.0.0' );
  service.sin_port := htons( IRCPort );

  if bind(m_socket, service, sizeof(service))=SOCKET_ERROR then
    begin
    Log('[IRC] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  if listen( m_socket, 1 )=SOCKET_ERROR then
    begin
    Log('[IRC] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  Log('[IRC] Listening on port '+IntToStr(IRCPort)+'.');

  repeat
    T:=SizeOf(incoming);
    AcceptSocket := accept( m_socket, @incoming, @T );
    if AcceptSocket<>INVALID_SOCKET then
      begin
      T:=SizeOf(incoming);
      Log('[IRC] Connection established from '+inet_ntoa(incoming.sin_addr));

      User:=TUser.Create(True);
      User.Socket:=AcceptSocket;
      User.ConnectingFrom:=inet_ntoa(incoming.sin_addr);
      User.Modes['s']:=True;
      SetLength(Users, Length(Users)+1);
      Users[Length(Users)-1]:=User;
      User.Resume;
      end
    else
      Sleep(5);
  until False;
end;

procedure LogToOper(S: string);
var
  I: Integer;
begin
  for I:=0 to Length(Users)-1 do
    if Users[I].Modes['o'] then
      Users[I].SendLn(':'+ServerHost+' NOTICE '+Users[I].Nickname+' :'+S);
end;

var 
  ThreadID: Cardinal = 0;

procedure StartIRCServer;
begin
  if ThreadID=0 then  // start only once
    CreateThread(nil, 0, @MainProc, nil, 0, ThreadID);
end;

end.
