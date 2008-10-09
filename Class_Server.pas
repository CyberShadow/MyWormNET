unit Class_Server;

interface

uses
  {$IFDEF WIN32}
  WinSock, Windows,
  {$ELSE}
  Sockets, FakeWinSock,
  {$ENDIF}
  Class_Log, SysUtils, Classes, DateUtils;

const myWormNET_Version = '2.0.0.3 ENG';

type
  THTTPListener = class;
  TIRCListener = class;
  TRequest = class;
  TServer = class;
  TUser = class;
  TChannel = class;

  EDisconnect = class(Exception);
  EChannelExists = class(Exception);
  EBanned = class(Exception);
  EHTTPError = class(Exception);

  TGame = record
    Created: TDateTime;
    Name, Password, Loc: string;
    HosterNickname, HosterAddress: string;
    GameID: Integer;
  end;

  TReadSet = record
    Count: integer;
    Soc: TSocket;
  end;

  TServer = class(TObject)
  private
    WSA: TWSAData;
    FAddress: string;
    FChannels: array of TChannel;
    FUsers: array of TUser;
    FBanlist: array of string;
    FHTTPPort: integer;
    FIRCPort: integer;
    FRefreshInterval: integer;
    FLog: PLog;
    FHTTPSocket: TSocket;
    FIRCSOcket: TSocket;
    FMaxConnections: integer;
    FHTTPListener: THTTPListener;
    FIRCListener: TIRCListener;
    FAutoRefreshInterval: boolean;
    FOperatorPassword: string;
    FNATPort: integer;

    function GetChannels(Index: integer): TChannel;
    function GetChannelsCount: integer;
    function GetUsers(Index: integer): TUser;
    function GetUsersCount: integer;
    function GetBanlist(Index: integer): string;
    function GetBanlistLength: integer;
  published
    procedure CreateSocket(var Soc: TSocket; Port: integer);
    procedure LoadChannels;
    procedure LoadBanlist;

    property HTTPSocket: TSocket read FHTTPSocket write FHTTPSocket;
    property IRCSOcket: TSOcket read FIRCSocket write FIRCSocket;
  public
    constructor Create(ServerAddress: string; LogPtr: PLog; HTTP, IRC, NAT, MaxConnections, RefreshInt: integer);
    destructor Destroy; override;

    procedure AddChannel(ChannelName, ChannelTopic, ChannelVersion, ChannelScheme: string); overload;
    procedure AddChannel(var ChannelFile: TextFile); overload;
    procedure AddUser(Soc: TSocket; UserIP: string);
    procedure RemoveUser(var User: TUser);
    procedure Ban(NameOrIP: string);

    function ChannelByName(ChanName: string): TChannel;
    function UserByName(UsrName: string): TUser;
    function IsBanned(NameOrIP: string): boolean;
    function UserExists(Name: string): boolean;
    function UsersInChannelsCount: integer;
    function RemoveFromBanlist(Rec: string): boolean;

    property Address: string read FAddress;

    property Channels[Index: integer]: TChannel read GetChannels;
    property ChannelsCount: integer read GetChannelsCount;

    property Banlist[Index: integer]: string read GetBanlist;
    property BanlistLength: integer read GetBanlistLength;

    property HTTPPort: integer read FHTTPPort;
    property IRCPort: integer read FIRCPort;
    property RefreshInterval: integer read FRefreshInterval write FRefreshInterval;
    property AutoRefreshInterval: boolean read FAutoRefreshInterval;
    property Log: PLog read FLog write FLog;
    property MaxConnections: integer read FMaxConnections;
    property OperatorPassword: string read FOperatorPassword write FOperatorPassword;
    property NATPort: integer read FNATPort;

    property Users[Index: integer]: TUser read GetUsers;
    property UsersCount: integer read GetUsersCount;
  end;

  TSrvThread = class(TThread)
  private
    FServer: TServer;
  public
    constructor Create(var Srv: TServer);

    property Server: TServer read FServer;
  end;

  TExSrvThread = class(TSrvThread)
  private
    FIP: string;
    FSocket: TSocket;
  public
    constructor Create(Soc: TSocket; Address: string; var Srv: TServer);

    function ExtractLine(var Source, Dest: string): boolean;
    function ReadFile(Path: string): string;

    procedure SendData(Text: string);

    property IP: string read FIP;
    property Socket: TSocket read FSocket;
  end;

  THTTPListener = class(TSrvThread)
  public
    procedure Execute; override;
  end;

  TIRCListener = class(TSrvThread)
  public
    procedure Execute; override;
  end;

  TRequest = class(TExSrvThread)
  public
    procedure Execute; override;

    function CleanUpGET(var Source: string): string;
  end;

  TUser = class(TExSrvThread)
  private
    FName: string;
    FFrom: string;
    FInfo: string;
    FVersion: string;
    FChannel: TChannel;
    FLastWho: TDateTime;
    FLastList: TDateTime;
    FTimer: TDateTime;
    FFloodpoints: integer;
    FMuted: boolean;
    FOper: boolean;
  public
    constructor Create(Soc: TSocket; Address: string; var Srv: TServer);

    procedure Execute; override;

    procedure Refresh;
    procedure FloodControl;

    procedure CmdList;
    procedure CmdWho;
    procedure CmdPart;
    procedure CmdQuit;
    procedure CmdPong;
    procedure CmdUser(var Str: string);
    procedure CmdNick(var Str: string);
    procedure CmdJoin(var Str: string);
    procedure CmdExpect(var Str: string);
    procedure CmdPrivMsg(var Str: string);
    procedure CmdNotice(var Str: string);

    procedure CustomCommand(var Str: string);

    function CmdHelp: string;
    function CmdBan(var Str: string): string;
    function CmdBlack(var Str: string): string;
    function CmdSwitch: string;
    function CmdFlood(var Str: string): string;
    function CmdInfo(var Str: string): string;
    function CmdListMode: string;
    function CmdBanlist(var Str: string): string;
    function CmdBlacklist(var Str: string): string;
    function CmdOperlist(var Str: string): string;
    function CmdUserlist(var Str: string): string;
    function CmdBanRemove(var Str: string): string;
    function CmdBlackRemove(var Str: string): string;
    function CmdAdmin(var Str: string): string;
    function CmdOper(var Str: string): string;

    procedure ServerAnswer(var Str: string);

    property Name: string read FName write FName;
    property From: string read FFrom;
    property Version: string read FVersion;
    property Info: string read FInfo;
    property FloodPoints: integer read FFloodPoints;
    property Oper: boolean read FOper write FOper;
    property Channel: TChannel read FChannel write FChannel;
  end;

  TChannel = class(TObject)
  private
    FName: string;
    FTopic: string;
    FScheme: string;
    FVersion: string;
    FWhiteListMode: boolean;
    FUsers: array of TUser;
    FBlackList: array of string;
    FServer: TServer;
    FGames: array of TGame;
    FGameCounter: integer;

    function GetUsers(Index: integer): TUser;
    function GetUsersCount: integer;
    function GetBlackList(Index: integer): string;
    function GetBlackListLength: integer;
    function GetGames(Index: integer): TGame;
    function GetGamesCount: integer;
  published
    procedure SpaceChanger(var Str: string);
    procedure GameTimer;
  public
    constructor Create(ChanName, ChanTopic, ChanVersion, ChanScheme: string; var Srv: TServer); overload;
    constructor Create(var ChanFile: TextFile; var Srv: TServer); overload;
    destructor Destroy; override;

    procedure Join(var Usr: TUser);
    procedure Leave(var Usr: TUser; Part: boolean; Reason: string);
    procedure AddToBlackList(BlackName: string);
    procedure Broadcast(var From: TUser; Msg: string); overload;
    procedure Broadcast(Msg: string); overload;

    procedure AddGame(GName, GPwd, GLoc, Host, IP: string);
    procedure RemoveGame(Index: integer);
    function CloseGame(ID: integer): boolean;

    function IsUserOnList(UserName: string; UserIP: string): boolean;
    function RemoveFromBlackList(Rec: string): boolean;

    property Name: string read FName;
    property Topic: string read FTopic;
    property Scheme: string read FScheme;
    property Version: string read FVersion;
    property WhiteListMode: boolean read FWhiteListMode write FWhiteListMode;
    property BlackList[Index: integer]: string read GetBlackList;
    property BlackListLength: integer read GetBlackListLength;
    property Server: TServer read FServer;

    property Games[Index: integer]: TGame read GetGames;
    property GamesCount: integer read GetGamesCount;

    property Users[Index: integer]: TUser read GetUsers;
    property UsersCount: integer read GetUsersCount;
  end;

implementation

uses WormNATServer;

{$I mime.inc}

{ TServer }

constructor TServer.Create(ServerAddress: string; LogPtr: PLog; HTTP, IRC, NAT, MaxConnections, RefreshInt: integer);
begin
  SetLength(FChannels, 0);
  SetLength(FUsers, 0);
  SetLength(FBanlist, 0);
  FAddress := ServerAddress;
  FChannels := nil;
  FHTTPPort := HTTP;
  FIRCPort := IRC;
  FNATPort := NAT;
  FMaxConnections := MaxConnections;
  FRefreshInterval := RefreshInt;
  FOperatorPassword := 'abc';

  if RefreshInterval < 1 then
    FAutoRefreshInterval := true
  else
    FAutoRefreshInterval := false;

  Log := LogPtr;
  Log.AddText('GENERAL|SERVER', 'Server address: ' + Address);
  Log.AddText('GENERAL|SERVER', 'Server ports: ' +inttostr(HTTPPort)+ ', ' +inttostr(IRCPort)+ ', ' + inttostr(NATPort));
  Log.AddText('GENERAL|SERVER', 'Server is loading...');

  LoadChannels;
  LoadBanlist;

  if WSAStartUp(2, WSA) = 1 then
  begin
    Log.AddText('ERROR|SERVER|WSA', 'WSA startup failed.');
    raise Exception.Create(Log.Lines[Log.LinesCount - 1].Text);
  end
  else
    Log.AddText('SERVER|WSA', 'WSA startup correctly.');

  CreateSocket(FHTTPSocket, HTTPPort);
  CreateSocket(FIRCSocket, IRCPort);

  FHTTPListener := THTTPListener.Create(Self);
  FHTTPListener.Resume;

  FIRCListener := TIRCListener.Create(Self);
  FIRCListener.Resume;

  WNATPort := NATPort;
  StartWormNATServer;
  Log.AddText('GENERAL|SERVER', 'Server is up.');
end;

destructor TServer.Destroy;
begin
  SetLength(FChannels, 0);

  CloseSocket(FHTTPSocket);
  WSACleanUp;

  Log.AddText('GENERAL|SERVER', 'Server has been shut down.');

  inherited;
end;

procedure TServer.AddUser(Soc: TSocket; UserIP: string);
begin
  if not(UsersCount >= MaxConnections) then
  begin
    SetLength(FUsers, UsersCount + 1);
    FUsers[UsersCount - 1] := TUser.Create(Soc, UserIP, Self);
    FUsers[UsersCount - 1].Resume;
  end;
end;

procedure TServer.Ban(NameOrIP: string);
begin
  if Length(NameOrIP) > 0 then
  begin
    SetLength(FBanlist, BanlistLength + 1);
    FBanlist[BanlistLength - 1] := NameOrIP;
    Log.AddText('BAN', NameOrIP + ' has been added to the current banlist.');
  end;
end;

procedure TServer.CreateSocket(var Soc: TSocket; Port: integer);
var SocInfo: sockaddr_in;
begin
  Soc := Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Soc = SOCKET_ERROR then
  begin
    Log.AddText('ERROR|SERVER|WSA', 'Creation of socket for port ' +inttostr(Port)+ ' failed.');
    raise Exception.Create(Log.Lines[Log.LinesCount - 1].Text);
  end
  else
    Log.AddText('SERVER|WSA', 'Socket for port ' +inttostr(Port)+ ' created correctly.');

  SocInfo.sin_family := AF_INET;
  SocInfo.sin_port := htons(Port);
  SocInfo.sin_addr.s_addr := inet_addr('0.0.0.0');

  if Bind(Soc, SocInfo, sizeof(SocInfo)) = SOCKET_ERROR then
  begin
    Log.AddText('ERROR|SERVER|WSA', 'Binding to port ' +inttostr(Port)+ ' failed.');
    CloseSocket(Soc);
    raise Exception.Create(Log.Lines[Log.LinesCount - 1].Text);
  end
  else
    Log.AddText('SERVER|WSA', 'Binding to port ' +inttostr(Port)+ ' done.');

  if Listen(Soc, MaxConnections) = SOCKET_ERROR then
  begin
    Log.AddText('ERROR|SERVER|WSA', 'Listening failed on port ' +inttostr(Port)+ '.');
    CloseSocket(Soc);
    raise Exception.Create(Log.Lines[Log.LinesCount - 1].Text);
  end
  else
    Log.AddText('SERVER|WSA', 'Listening on port ' +inttostr(Port)+ '.');
end;

procedure TServer.LoadBanlist;
var F: TextFile;
    Str: string;
begin
  if FileExists('Banlist.txt') then
  begin
    AssignFile(F, 'Banlist.txt');
    Reset(F);
    while not(eof(F)) do
    begin
      ReadLn(F, Str);
      Ban(Str);
    end;
    CloseFile(F);
  end;
end;

procedure TServer.LoadChannels;
var SearchResult: integer;
    SR: TSearchRec;
    FilesList: array of string;
    a: integer;
    F: TextFile;
begin
  SetLength(FilesList, 0);

  if not(DirectoryExists('.\Channels\')) then
  begin
    Log.AddText('ERROR|SERVER|CHANNEL|IN', '''.\Channels\'' not found. Default channel will be created.');
    AddChannel('AnythingGoes', 'Open games with ''Rope Knocking'' allowed & blood fx', 'V 0', 'Pf,Be');
  end
  else
  begin
    SearchResult := FindFirst('.\Channels\*.txt', faAnyFile - faDirectory	, SR);
    while SearchResult = 0 do
    begin
      if SearchResult = 0 then
      begin
        if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Size <> 0) then
        begin
          SetLength(FilesList, Length(FilesList) + 1);
          FilesList[High(FilesList)] := SR.Name;
        end;
      end;
      SearchResult := FindNext(SR);
    end;
    FindClose(SR);

    if Length(FilesList) = 0 then
    begin
      Log.AddText('ERROR|SERVER|CHANNEL|IN', 'Files in ''\Channels\'' not found. Default channel will be created.');
      AddChannel('AnythingGoes', 'Open games with ''Rope Knocking'' allowed & blood fx', 'V 0', 'Pf,Be');
    end
    else
    begin
      for a := 0 to Length(FilesList) - 1 do
      begin
        AssignFile(F, '.\Channels\' + FilesList[a]);
        Reset(F);
        AddChannel(F);
        CloseFile(F);
      end;
    end;
  end;
end;

procedure TServer.AddChannel(ChannelName, ChannelTopic, ChannelVersion, ChannelScheme: string);
begin
  SetLength(FChannels, ChannelsCount + 1);

  try
    FChannels[ChannelsCount - 1] := TChannel.Create(ChannelName, ChannelTopic, ChannelVersion, ChannelScheme, Self);
    Log.AddText('SERVER|CHANNEL', 'Channel ''#' +Channels[ChannelsCount - 1].Name+ ''' has beed added.');
  except
    SetLength(FChannels, ChannelsCount - 1);
    Log.AddText('SERVER|CHANNEL', 'Channel ''#' +Channels[ChannelsCount - 1].Name+ ''' already exists.');
  end;
end;

procedure TServer.AddChannel(var ChannelFile: TextFile);
begin
  SetLength(FChannels, ChannelsCount + 1);

  try
    FChannels[ChannelsCount - 1] := TChannel.Create(ChannelFile, Self);
    Log.AddText('SERVER|CHANNEL', 'Channel ''#' +Channels[ChannelsCount - 1].Name+ ''' has beed added.');
  except
    on E:Exception do
    begin
      SetLength(FChannels, ChannelsCount - 1);
      Log.AddText('SERVER|CHANNEL', 'Channel ''#' +E.Message+ ''' already exists.');
    end;
  end;
end;

function TServer.ChannelByName(ChanName: string): TChannel;
var a: integer;
    Found: boolean;
begin
  if ChanName[1] = '#' then
    Delete(ChanName, 1, 1);

  Found := false;
  for a := 0 to ChannelsCount - 1 do
  begin
    if Channels[a].Name = ChanName then
    begin
      Found := true;
      Break;
    end;
  end;

  if Found then
    Result := Channels[a]
  else
    Result := nil;
end;

function TServer.GetBanlist(Index: integer): string;
begin
  if (Index >= 0) and (Index < BanlistLength) then
    Result := FBanlist[Index]
  else
    Result := '';
end;

function TServer.GetUsers(Index: Integer): TUser;
begin
  if (Index >= 0) and (Index < UsersCount) then
    Result := FUsers[Index]
  else
    Result := nil;
end;

function TServer.GetBanlistLength: integer;
begin
  Result := Length(FBanlist);
end;

function TServer.GetChannels(Index: integer): TChannel;
begin
  if (Index >= 0) and (Index < ChannelsCount) then
    Result := FChannels[Index]
  else
    Result := nil;
end;

function TServer.GetChannelsCount: integer;
begin
  Result := Length(FChannels);
end;

function TServer.GetUsersCount: integer;
begin
  Result := Length(FUsers);
end;

function TServer.IsBanned(NameOrIP: string): boolean;
var a: integer;
begin
  Result := false;
  for a := 0 to BanlistLength - 1 do
  begin
    if NameOrIP = Banlist[a] then
    begin
      Result := true;
      Break;
    end;
  end;
end;

function TServer.RemoveFromBanlist(Rec: string): boolean;
var a: integer;
begin
  Result := false;
  for a := 0 to BanlistLength - 1 do
  begin
    if Rec = Banlist[a] then
    begin
      Result := true;
      Break;
    end;
  end;

  if Result then
  begin
    for a := a to BanlistLength - 2 do
      FBanlist[a] := FBanlist[a + 1];

    SetLength(FBanlist, BanlistLength - 1);
  end;
end;

procedure TServer.RemoveUser(var User: TUser);
var a: integer;
begin
  for a := 0 to UsersCount - 1 do
  begin
    if Users[a] = User then
      Break;
  end;

  Log.AddText('SERVER|USER', User.Name + '@' +User.IP+ ': has left the server.');
  for a := a to UsersCount - 2 do
    FUsers[a] := FUsers[a + 1];

  SetLength(FUsers, UsersCount  - 1);
  if User.Channel <> nil then
    User.Channel.Leave(User, false, '');
  User.Free;
end;

function TServer.UserByName(UsrName: string): TUser;
var a: integer;
begin
  Result := nil;
  for a := 0 to UsersCount - 1 do
  begin
    if Users[a].Name = UsrName then
    begin
      Result := Users[a];
      Break;
    end;
  end;
end;

function TServer.UserExists(Name: string): boolean;
var a: integer;
begin
  Result := false;
  for a := 0 to UsersCount - 1 do
  begin
    if Users[a].Name = Name then
    begin
      Result := true;
      Break;
    end;
  end;

  if (Name = '$SERVER$') or (Name = '$server$') or (Name = '$Server$') then
    Result := true;
end;

function TServer.UsersInChannelsCount: integer;
var a: integer;
begin
  Result := 0;
  for a := 0 to UsersCount - 1 do
  begin
    if Users[a].Channel <> nil then
      inc(Result);
  end;
end;

{ TSrvThread }

constructor TSrvThread.Create(var Srv: TServer);
begin
  inherited Create(true);

  FServer :=  Srv;
end;

{ TExSrvThread }

constructor TExSrvThread.Create(Soc: TSocket; Address: string; var Srv: TServer);
begin
  inherited Create(Srv);

  FIP := Address;
  FSocket := Soc;
end;

function TExSrvThread.ExtractLine(var Source, Dest: string): boolean;
var
  P, P1, P2: Integer;
begin
  P1 := Pos(#13, Source);
  P2 := Pos(#10, Source);
  if (P1 = 0) and (P2 = 0) then
  begin
    Dest := '';
    Result := False;
    Exit;
  end
  else
  begin
    if P1 = 0 then
      P := P2
    else
      if P2=0 then
        P := P1
      else
        if(P1 < P2) then
          P:=P1
        else
          P := P2;
  end;
  Result := true;
  Dest := Copy(Source, 1, P - 1);
  Delete(Source, 1, P);
  if Copy(Source, 1, 1) = #10 then
    Delete(Source, 1, 1);
end;

function TExSrvThread.ReadFile(Path: string): string;
var
  F: File;
begin
  Assign(F, Path);
  Reset(F, 1);
  SetLength(Result, FileSize(F));
  BlockRead(F, Result[1], FileSize(F));
  CloseFile(F);
end;

procedure TExSrvThread.SendData(Text: string);
begin
  Server.Log.AddText('NETWORK|OUT', Text);
  Text := Text + #13#10;
  if send(Socket, Text[1], Length(Text), 0) <> Length(Text) then
    Server.Log.AddText('ERROR|NETWORK|OUT', 'Failed on sending answer to ' + IP);
end;

{ THTTPListener}

procedure THTTPListener.Execute;
var AcceptSocket: TSocket;
    SocAdd :TSockAddrIn;
    Size: integer;
    Request: TRequest;
begin
  Size := sizeof(SocAdd);
  while true do
  begin
    AcceptSocket := Accept(Server.HTTPSocket, @SocAdd, @Size);
    if AcceptSocket <> INVALID_SOCKET then
    begin
      Server.Log.AddText('NETWORK|IN', 'Accept query from ' + inet_ntoa(SocAdd.sin_addr)+ ' on port ' +inttostr(Server.HTTPPort)+ '.');

      Request := TRequest.Create(AcceptSocket, inet_ntoa(SocAdd.sin_addr), FServer);
      Request.Resume;
    end;
  end;
end;

{ TRequest }

procedure TRequest.Execute;
var Result: integer;
    Bytes: integer;
    Buffer: string;
    Str: string;
    FileName: string;
    Parameters: TStringList;
    Header: string;
    Body: string;
    a, b: integer;
begin
  try
    Buffer := '';
    Str := '';

    if Server.IsBanned(IP) then
      raise EBanned.Create('This IP is banned.');

    repeat
      Result := ioctlsocket(Socket, FIONREAD, Bytes);
      if Result = SOCKET_ERROR then
      begin
        Server.Log.AddText('ERROR|NETWORK|WSA', 'I/O socket error (Socket: ' +inttostr(Socket)+ ' | IP: ' +IP+ ').');
        raise EHTTPError.Create('I/O socket error');
      end;

      if Bytes = 0 then
      begin
        Sleep(10);
        Continue;
      end;

      SetLength(Str, Bytes);
      Result := Recv(Socket, Str[1], Bytes, 0);
      if (Result = 0) or (Result = SOCKET_ERROR) then
      begin
        Server.Log.AddText('ERROR|NETWORK|WSA|IN', 'I/O socket read error (Socket: ' +inttostr(Socket)+ ' | IP: ' +IP+ ':' +inttostr(Server.HTTPPort)+ ').');
        raise EHTTPError.Create('I/O socket read error.');
      end;

      SetLength(Str, Result);
      Buffer := Buffer + Str;
    until Copy(Buffer, Length(Buffer) - 3, 4) = #13#10#13#10;

    ExtractLine(Buffer, Str);

    Server.Log.AddText('NETWORK|IN', 'Query: ' + Str);

    if Copy(Str, 1, 4) <> 'GET ' then
    begin
      Server.Log.AddText('ERROR|NETWORK|IN', 'Only GET requests are supported (' +IP+ ')');
      raise EHTTPError.Create(Server.Log.Lines[Server.Log.LinesCount- 1].Text);
    end
    else
      Delete(Str, 1, 4);

    FileName := CleanUpGET(Str);
    Parameters := TStringList.Create;
    Parameters.NameValueSeparator := '=';
    while Str <> '' do
    begin
      Parameters.Add(Copy(Str, 1, Pos('&', Str) - 1));
      Delete(Str, 1, Pos('&', Str));
    end;

    Header := 'HTTP/1.1 200 OK' + #13#10;
    Header := Header + 'X-Powered-By: MyWormNET 2' + #13#10;

    Body := '';

    if FileName = 'Login.asp' then
      Body := '<CONNECT ' + Server.Address+ '>'
    else
    begin
      if FileName = 'RequestChannelScheme.asp' then
        Body := '<SCHEME=' +Server.ChannelByName(Parameters.Values['Channel']).Scheme+ '>'
      else
      begin
        if FileName = 'Game.asp' then
        begin
          if Parameters.Values['Cmd'] = 'Create' then
          begin
            if Server.ChannelByName(Parameters.Values['Chan']) <> nil then
            begin
              Server.ChannelByName(Parameters.Values['Chan']).GameTimer;
              Server.ChannelByName(Parameters.Values['Chan']).AddGame(Parameters.Values['Name'], Parameters.Values['Pwd'], Parameters.Values['Loc'], Parameters.Values['Nick'], Parameters.Values['HostIP']);
              Header := Header + 'SetGameId: : ' + IntToStr(Server.ChannelByName(Parameters.Values['Chan']).Games[Server.ChannelByName(Parameters.Values['Chan']).GamesCount - 1].GameID) + #13#10;
              Body := '<NOTHING>';
            end;
          end;

          if Parameters.Values['Cmd'] = 'Close' then
          begin
            for a := 0 to Server.ChannelsCount - 1 do
            begin
              for b := 0 to Server.Channels[a].GamesCount - 1 do
              begin
                if Server.Channels[a].Games[b].GameID = strtoint(Parameters.Values['GameID']) then
                begin
                  Server.Channels[a].CloseGame(strtoint(Parameters.Values['GameID']));
                  Server.Channels[a].GameTimer;
                  Body := '<NOTHING>';
                end;
              end;
            end;
          end;

          if Parameters.Values['Cmd'] = 'Failed' then
            Body := '<NOTHING>';
        end
        else
        begin
          if FileName = 'UpdatePlayerInfo.asp' then
            //ignore
          else
          begin
            if FileName = 'GameList.asp' then
            begin
              if Server.ChannelByName(Parameters.Values['Channel']) <> nil then
              begin
                Server.ChannelByName(Parameters.Values['Channel']).GameTimer;
                Body := Body + '<GAMELISTSTART>' + #13#10;
                for a := 0 to Server.ChannelByName(Parameters.Values['Channel']).GamesCount - 1 do
                begin
                  with Server.ChannelByName(Parameters.Values['Channel']).Games[a] do
                    Body := Body + '<GAME ' +Name+ ' ' +HosterNickname+ ' ' +HosterAddress+ ' ' +Loc+ ' 1 0 ' +IntToStr(GameID)+ ' 0><BR>'#13#10;
                end;
                Body := Body + '<GAMELISTEND>' + #13#10;
              end;
            end
            else
            begin
              for a := Length(FileName) downto 1 do
              begin
                if(FileName[a] = '/') and (PathDelim = '\') then
                  FileName[a] := PathDelim
                else
                begin
                  if FileName[a] = '%' then
                  begin
                    FileName[a] := Chr(StrToInt('$' + Copy(FileName, a + 1, 2)));
                    Delete(FileName, a + 1, 2);
                  end;
                end;
              end;

              if Pos('..', FileName) + Pos(PathDelim + PathDelim, FileName) <> 0 then
                raise EHTTPError.Create('hmm hmm hmm');

              if(FileName = '') or (FileName[Length(FileName)] = PathDelim) then
                FileName := FileName + 'index.html';

              if FileExists('wwwroot' + PathDelim + FileName) then
              begin
                Server.Log.AddText('NETWORK|OUT', IP + ' Sending file ' + FileName);
                Str := 'application/octet-stream';

                for a := 1 to High(MimeTypes) do
                begin
                  if '.' + MimeTypes[a].Extension = ExtractFileExt(FileName) then
                    Str := MimeTypes[a].MimeType;
                end;

                Header := Header + 'Content-Type: ' + Str + #13#10;
                Body := ReadFile('wwwroot' + PathDelim + FileName);
                Header := Header + 'Content-Length: ' + IntToStr(Length(Body)) + #13#10;
              end
              else
                raise EHTTPError.Create('"File" not found - '+FileName);
            end;
          end;
        end;
      end;
    end;

    Str := Header + #13#10 + Body;
    SendData(Str);
    Parameters.Free;
  except
    on E: EBanned do
    begin
      try
        Server.Log.AddText('BAN|USER', 'Banned IP (' +IP+ ') request rejected.');
        Str := Header + 'Error: : ' +E.Message + #13#10#13#10 + 'Error: ' +E. Message;
        SendData(Str);
      except
      end;
    end;

    on E: EHTTPError do
    begin
      try
        Server.Log.AddText('ERROR|NETWORK|OUT', 'Error with ' +IP+ ': ' +E.Message);
        Str := Header + 'Error: : ' +E.Message + #13#10#13#10 + 'Error: ' +E. Message;
        SendData(Str);
      except
      end;
    end;
  end;
  CloseSocket(Socket);
end;

function TRequest.CleanUpGET(var Source: string): string;
begin
  Source := Copy(Source, 1, Pos(' ', Source + ' ') - 1);

  if LowerCase(Copy(Source, 1, 7)) = 'http://' then
  begin
    Delete(Source, 1, 7);
    Delete(Source, 1, Pos('/', Source) - 1);
  end;

  while Copy(Source, 1, 1) = '/' do
    Delete(Source, 1, 1);

  if Copy(Source, 1, 15) = 'wormageddonweb/' then
    Delete(Source, 1, 15);

  Result := Copy(Source, 1, Pos('?', Source+ '?') - 1);
  Delete(Source, 1, Pos('?', Source));
  Source := Source + '&';
end;

{ TIRCListener }

procedure TIRCListener.Execute;
var AcceptSocket: TSocket;
    SocAdd :TSockAddrIn;
    Size: integer;
begin
  Size := sizeof(SocAdd);
  while true do
  begin
    AcceptSocket := Accept(Server.IRCSocket, @SocAdd, @Size);
    if AcceptSocket <> INVALID_SOCKET then
    begin
      Server.Log.AddText('NETWORK|IN', 'Accept query from ' + inet_ntoa(SocAdd.sin_addr)+ ' on port ' +inttostr(Server.IRCPort)+ '.');

      Server.AddUser(AcceptSocket, inet_ntoa(SocAdd.sin_addr));
    end
    else
      Sleep(5);
  end;
end;

{ TUser }

constructor TUser.Create(Soc: TSocket; Address: string; var Srv: TServer);
begin
  inherited Create(Soc, Address, Srv);
  FLastWho := Now;
  FLastList := Now;
  FTimer := Now;
  FFloodpoints := 0;
  FMuted := false;
  FOper := false;

  FChannel := nil;
end;

procedure TUser.CustomCommand(var Str: string);
var Cmd: string;
    Answer: string;
begin
  if Length(Str) > 1 then
  begin
    Delete(Str, 1, 1);

    if Pos(' ', Str) <> 0 then
    begin
      Cmd := AnsiUpperCase(Copy(Str, 1, Pos(' ', Str) - 1));
      Delete(Str, 1, Pos(' ', Str));
    end
    else
    begin
      Cmd := AnsiUpperCase(Str);
      Str := '';
    end;

    Answer := 'Unknown command.';

    if Cmd = 'OPER' then
      Answer := CmdOper(Str);

    if Cmd = 'ADMIN' then
      Answer := CmdAdmin(Str);

    if Cmd = 'HELP' then
      Answer := CmdHelp;

    if Oper then
    begin
      if Cmd = 'BAN' then
        Answer := CmdBan(Str);

      if Cmd = 'BLACK' then
        Answer := CmdBlack(Str);

      if Cmd = 'SWITCH' then
        Answer := CmdSwitch;

      if Cmd = 'FLOOD' then
        Answer := CmdFlood(Str);

      if Cmd = 'INFO' then
        Answer := CmdInfo(Str);

      if Cmd = 'LISTMODE' then
        Answer := CmdListMode;

      if Cmd = 'BANLIST' then
        Answer := CmdBanlist(Str);

      if Cmd = 'BLACKLIST' then
        Answer := CmdBlacklist(Str);

      if Cmd = 'OPERLIST' then
        Answer := CmdOperlist(Str);

      if Cmd = 'USERLIST' then
        Answer := CmdUserlist(Str);

      if Cmd = 'BANREMOVE' then
        Answer := CmdBanRemove(Str);

      if Cmd = 'BLACKREMOVE' then
        Answer := CmdBlackRemove(Str);
    end;

    ServerAnswer(Answer);
  end;
end;

function TUser.CmdAdmin(var Str: string): string;
var a: integer;
begin
  Str := Name + '@' +IP+ '#' +Channel.Name+ ': requests admin (' +Str+ ').';

  for a := 0 to Server.UsersCount - 1 do
  begin
    if Server.Users[a].Oper then
    begin
      if Server.Users[a].Channel <> nil then
        Server.Users[a].ServerAnswer(Str);
    end;
  end;

  Result := 'Request sent. Wait for admin/operator.';
end;

function TUser.CmdBan(var Str: string): string;
begin
  Server.Ban(Str);
  Result := '''' +Str + ''' has been banned.';
end;

function TUser.CmdBanlist(var Str: string): string;
var a: integer;
begin
  Str := 'Banlist length: ' + inttostr(Server.BanlistLength);
  ServerAnswer(Str);

  for a := 0 to Server.BanlistLength - 1 do
  begin
    Str := '[' +inttostr(a)+ '] ' + Server.Banlist[a];
    ServerAnswer(Str);
  end;

  Result := 'End of banlist.';
end;

function TUser.CmdBanRemove(var Str: string): string;
begin
  if Channel.RemoveFromBlackList(Str) then
    Result := '' +Str+ ''' has been removed from black/white list.'
  else
    Result := 'Black/white list do not contain ' +Str+ '.';
end;

function TUser.CmdBlack(var Str: string): string;
begin
  Channel.AddToBlacklist(Str);
  Result := '''' +Str + ''' has been added to the black/white list for this channel (#' +Channel.Name+ ').';
end;

function TUser.CmdBlacklist(var Str: string): string;
var a: integer;
begin
  Str := 'Blacklist length: ' + inttostr(Channel.BlackListLength);
  ServerAnswer(Str);

  for a := 0 to Channel.BlackListLength - 1 do
  begin
    Str := '[' +inttostr(a)+ '] ' + Channel.BlackList[a];
    ServerAnswer(Str);
  end;

  Result := 'End of blacklist.';
end;

function TUser.CmdBlackRemove(var Str: string): string;
begin
  if Server.RemoveFromBanlist(Str) then
    Result := '' +Str+ ''' has been removed from banlist.'
  else
    Result := 'Banlist do not contain ''' +Str+ '''.';
end;

procedure TUser.CmdExpect(var Str: string);
var Usr: TUser;
begin
  Usr := Server.UserByName(Str);
  if Usr = nil then
    SendData(':' +Server.Address+ ' 401 ' +Str+ ' :No such nick.')
  else
  begin
    SendData(':' +Server.Address+ ' NOTICE ' +Name+ ' :OK, expecting ' +Usr.Name+ ' from ' + Usr.IP);
    PrepareLink(Self, Usr);
  end;
end;

function TUser.CmdFlood(var Str: string): string;
var Usr: TUser;
begin
  Usr := Server.UserByName(Str);
  if Usr <> nil then
    Result := Str + '''s flood points: ' + inttostr(Usr.FloodPoints)
  else
    Result := 'There is no user logged with this name.';
end;

function TUser.CmdHelp: string;
var Str: string;
    a: integer;
begin
  a := -1;
  Str := ' ';
  repeat
    inc(a);

    case a of
      0: Str := 'User commands:';
      1: Str := 'help - list of commands';
      2: Str := 'admin <reason> - request for admin/operator intervention';
      3: Str := 'oper <password> - logging to the operator mode';
    else
      Str := '';
    end;

    if Oper then
    begin
      case a of
        4: Str := 'Operator commands:';
        5: Str := 'ban <name or ip> - add <...> to the global banlist';
        6: Str := 'banlist - shows all names/ips on global banlist';
        7: Str := 'banremove <name or ip> - removes <...> from global banlist';
        8: Str := 'black <name or ip> - add <...> to the black/white list';
        9: Str := 'blacklist - shows all names/ips on black/white list for current channel';
        10: Str := 'blackremove <name or ip> - remove <...> from black/white list';
        11: Str := 'switch - changes channel mode from black -> white or from white -> black';
        12: Str := 'info <name> - shows nick, ip, game version and localization of <...>';
        13: Str := 'flood <name> - how much flood points have user';
        14: Str := 'listmode - is current channel on white or black mode';
        15: Str := 'operlist - list of current operators on server';
        16: Str := 'userlist - list of users on server';
      else
        Str := '';
      end;
    end;

    if Str <> '' then
      ServerAnswer(Str);
  until Str = '';

  Result := 'All comands shown.';
end;

function TUser.CmdInfo(var Str: string): string;
var Usr: TUser;
begin
  Usr := Server.UserByName(Str);
  if Usr <> nil then
    Result := Usr.Name + '@' +Usr.IP+ ' Version: ' +Usr.Version+ ' From: ' + Usr.From
  else
    Result := 'There is no user logged with this name.';
end;

procedure TUser.CmdJoin(var Str: string);
var Chan: TChannel;
    MayJoin: boolean;
    a: integer;
begin
  Delete(Str, 1, 1);
  Chan := Server.ChannelByName(Str);
  if Chan <> nil then
  begin
    MayJoin := not(Chan.IsUserOnList(Name, IP));
    if Chan.WhiteListMode then
      MayJoin := not(MayJoin);

    if (Chan.Version <> '0') and (Chan.Version <> '') and (Chan.Version <> Version) then
      MayJoin := false;

    if Oper then
      MayJoin := true;

    if MayJoin then
    begin
      Chan.Join(Self);
      Str := ':' +Server.Address+ ' 353 ' +Name+ ' = #' +Chan.Name+ ' :';

      for a := 0 to Chan.UsersCount - 1 do
      begin
        if Channel.Users[a].Oper then
          Str := Str + '@';
        Str := Str + Chan.Users[a].Name + ' ';
      end;

      SendData(Str);
      SendData(':' +Server.Address+ ' 366 ' +Name+ ' #' +Chan.Name+ ' :End of /NAMES list.');

      Server.Log.AddText('NETWORK|USER|CHANNEL', Name + '@' +IP+ ': has joned ''#' +Chan.Name+ ''' channel.');
    end
    else
    begin
      if not(Chan.WhiteListMode) then
        Str := 'You are banned on this channel'
      else
        Str := 'You are not on permission list for this channel';

      if (Chan.Version <> '0') and (Chan.Version <> '') and (Chan.Version <> Version) then
        Str := 'This channel allows ' +Chan.Version+ ' version only (Your version: ' + Version+ ')';

      SendData(':' +Server.Address+ ' 474 #' +Chan.Name+ ' :' + Str);
    end;
  end
  else
    SendData(':' +Server.Address+ ' 403 ' +Name+ ' ' +Str+ ' :No such channel');
end;

procedure TUser.CmdList;
var a: integer;
begin
  FLastList := Now;
  SendData(':' +Server.Address+ ' 321 ' +Name+ ' Channel :Users  Name');
  for a := Server.ChannelsCount - 1 downto 0 do
    SendData(':' +Server.Address+ ' 322 ' +Name+ ' #' +Server.Channels[a].Name+ ' ' +inttostr(Server.Channels[a].UsersCount)+ ' ' +Server.Channels[a].Topic+ ' :');
  SendData(':' +Server.Address+ ' 323 ' +Name+ ' :End of /LIST');
end;

function TUser.CmdListMode: string;
begin
  if Channel.WhiteListMode then
    Result := 'Current list mode: white'
  else
    Result := 'Current list mode: black';
end;

procedure TUser.CmdNick(var Str: string);
var a: integer;
    F: TextFile;
    Strg: string;
begin
  while Copy(Str, Length(Str), 1) = ' ' do
    Delete(Str, Length(Str), 1);

  if Server.UserExists(Str) then
  begin
    a := 0;
    while Server.UserExists(Str + inttostr(a)) do
      inc(a);
    FName := Str + inttostr(a);
  end
  else
    FName := str;

  FName := Str;
  if Name <> '' then
  begin
    Server.Log.AddText('NETWORK|USER', Name + '@' +IP+ ': has logged in.');
    SendData(':' +Server.Address+ ' 001 ' +Name+ ' :Welcome, ' +Name+ '!');
    SendData(':' +Server.Address+ ' 002 ' +Name+ ' :This is a custom WormNET-compatible IRC server emulator,');
    SendData(':' +Server.Address+ ' 003 ' +Name+ ' :supporting set of IRC features.');
    SendData(':' +Server.Address+ ' 004 ' +Name+ ' :The server software was written by ');
    SendData(':' +Server.Address+ ' 005 ' +Name+ ' :The_CyberShadow <thecybershadow@gmail.com>');
    SendData(':' +Server.Address+ ' 006 ' +Name+ ' :and modified by');
    SendData(':' +Server.Address+ ' 007 ' +Name+ ' :Expro <mds.expro@gmail.com>');
    SendData(':' +Server.Address+ ' 007 ' +Name+ ' :myWormNET version: ' + myWormNET_Version);
    SendData(':' +Server.Address+ ' 251 ' +Name+ ' :There are ' +inttostr(Server.UsersCount)+ ' users on the server.');
    SendData(':' +Server.Address+ ' 254 ' +Name+ ' :There are ' +inttostr(Server.ChannelsCount)+ ' channels on the server');
    SendData(':' +Server.Address+ ' 375 ' +Name+ ' :- myWormNET 2.0 Message of the day');
    if FileExists('MOTD.txt') then
    begin
      AssignFile(F, 'MOTD.txt');
      Reset(F);
      while not(eof(F)) do
      begin
        ReadLn(F, Strg);
        SendData(':' +Server.Address+ ' 372 ' +Name+ ' :- ' + Strg);
      end;
      CloseFile(F);
    end;
    SendData(':' +Server.Address+ ' 376 ' +Name+ ' :End of /MOTD command.');
  end;
end;

procedure TUser.CmdNotice(var Str: string);
var Usr: TUser;
    Chn: TChannel;
    Nm: string;
begin
  if Str[1] = '#' then
  begin
    Delete(Str, 1, 1);
    Nm := Copy(Str, 1, Pos(' ', Str) - 1);
    Delete(Str, 1, Pos(':', Str));
    Chn := Server.ChannelByName(Nm);
    if Chn <> nil then
    begin
      Channel.SpaceChanger(Str);
      Str := ':' +Name+ '!' +Name+ '@' +IP+ ' NOTICE #' +Chn.Name+ ' :' + Str;
      Channel.Broadcast(Self, Str);
    end;
  end
  else
  begin
    Nm := Copy(Str, 1, Pos(' ', Str) - 1);
    Delete(Str, 1, Pos(':', Str));
    Usr := Server.UserByName(Nm);
    if Usr <> nil then
    begin
      Channel.SpaceChanger(Str);
      Str := ':' +Name+ '!' +Name+ '@' +IP+ ' NOTICE ' +Usr.Name+ ' :' + Str;
      Usr.SendData(Str);
    end;
  end;
end;

function TUser.CmdOper(var Str: string): string;
begin
  if not(Oper) then
  begin
    if Str = Server.OperatorPassword then
    begin
      Oper := true;
      Server.Log.AddText('SERVER|USER', Name + '@' +IP+ ': logged as operator.');
      Result := 'You are operator.';
      SendData(':' +Name+ ' MODE ' +Name+ ' :+o');
      Channel.Broadcast(':' +Server.Address+ ' MODE ' +Channel.Name+ ' +o '+ Name);
    end
  else
    Result := 'Wrong password.';
  end
  else
    Result := 'You are already logged as operator.';
end;

function TUser.CmdOperlist(var Str: string): string;
var a: integer;
begin
  Str := 'Logged operators:';
  ServerAnswer(Str);
  for a := 0 to Server.UsersCount - 1 do
  begin
    if Server.Users[a].Oper then
    begin
      Str := Server.Users[a].Name + '@' + Server.Users[a].IP;
      ServerAnswer(Str);
    end;
  end;
  Result := 'End of operators list.';
end;

procedure TUser.CmdPart;
begin
  Channel.Leave(Self, true, '');
end;

procedure TUser.CmdPong;
begin
  SendData('PONG :' + Server.Address);
end;

procedure TUser.CmdPrivMsg(var Str: string);
var Usr: TUser;
    Chn: TChannel;
    Nm: string;
begin
  Nm := Copy(Str, 1, Pos(' ', Str) - 1);
  Delete(Str, 1, Pos(':', Str));

  if Nm[1] = '#' then
  begin
    Delete(Nm, 1, 1);
    if Length(Str) > 0 then
    begin
      if Str[1] = '$' then
        CustomCommand(Str)
      else
      begin
        FFloodPoints := FFloodPoints + 750 + Length(Str)*10;
        Chn := Server.ChannelByName(Nm);
        if Chn <> nil then
        begin
          Server.Log.AddText('USER|CHANNEL', Name + '@' +IP+ '#' +Channel.Name+ ': ' + Str);
          Str := ':' +Name+ '!' +Name+ '@' +IP+ ' PRIVMSG #' +Chn.Name+ ' :' + Str;
          if not(FMuted) then
            Channel.Broadcast(Self, Str);
        end;
      end;
    end;
  end
  else
  begin
    FFloodPoints := FFloodPoints + 750 + Length(Str)*2;
    Usr := Server.UserByName(Nm);
    if Usr <> nil then
    begin
      Server.Log.AddText('USER|CHANNEL', Name + '@' +IP+ '#' +Channel.Name+ ': ' + Str);
      Str := ':' +Name+ '!' +Name+ '@' +IP+ ' PRIVMSG ' +Usr.Name+ ' :' + Str;
      if not(FMuted) then
        Usr.SendData(Str);
    end;
  end;
end;

procedure TUser.CmdQuit;
begin
  Channel.Leave(Self, false, '');
end;

function TUser.CmdSwitch: string;
var Str: string;
begin
  Channel.WhiteListMode := not(Channel.WhiteListMode);
  Str := '$LISTMODE';
  CustomCommand(Str);
  Result := 'Mode switched.';
end;

procedure TUser.CmdUser(var Str: string);
begin
  Delete(Str, 1, Pos(' ', Str));
  Delete(Str, 1, Pos(' ', Str));
  Delete(Str, 1, Pos(':', Str));
  FInfo := Copy(Str, 1, Pos(' ', Str) - 1);
  Delete(Str, 1, Pos(' ', Str));
  FInfo := FInfo + ' ' + Copy(Str, 1, Pos(' ', Str) - 1);
  Delete(Str, 1, Pos(' ', Str));
  FFrom := Copy(Str, 1, Pos(' ', Str) - 1);
  Delete(Str, 1, Pos(' ', Str));
  FVersion := Copy(Str, 1, Length(Str));
  SetLength(FVersion, Length(FVersion) - 1);
end;

function TUser.CmdUserlist(var Str: string): string;
var a: integer;
begin
  Str := 'Users count: ' + inttostr(Server.UsersCount);
  ServerAnswer(Str);
  for a := 0 to Server.UsersCount - 1 do
  begin
    if Server.Users[a].Channel <> nil then
      Str := '[' + inttostr(a) + '] ' +Server.Users[a].Name+ '@' +Server.Users[a].IP+ '#' + Server.Users[a].Channel.Name
    else
      Str := '[' + inttostr(a) + '] ' +Server.Users[a].Name+ '@' +Server.Users[a].IP;
    ServerAnswer(Str);
  end;
  Result := 'End of users list.';
end;

procedure TUser.CmdWho;
var a: integer;
begin
  FLastWho := Now;
  for a := 0 to Server.UsersCount - 1 do
  begin
    if Server.Users[a].Channel <> nil then
      SendData(':' +Server.Address+ ' 352 ' +Name+ ' #' +Server.Users[a].Channel.Name+ ' ' +Server.Users[a].Name+ ' ' +Server.Users[a].IP+ ' ' +Server.Address+ ' ' +Server.Users[a].Name+ ' H :0 ' +Server.Users[a].Info+ ' :' +Server.Users[a].From+ ' ' +Server.Users[a].Version)
    else
      SendData(':' +Server.Address+ ' 352 ' +Name+ ' * ' +Server.Users[a].Name+ ' ' +Server.Users[a].IP+ ' ' +Server.Address+ ' ' +Server.Users[a].Name+ ' H :0 ' +Server.Users[a].Info+ ' :' +Server.Users[a].From+ ' ' +Server.Users[a].Version);
  end;

  SendData(':' +Server.Address+ ' 315 ' +Name+ ' * :End of /WHO list.')
end;

procedure TUser.Execute;
var Result: integer;
    Str: string;
    Bytes: integer;
    TimeVal: TTimeVal;
    Buffer: string;
    Cmd: string;
    ReadSet: TReadSet;
    Pass: boolean;
begin
  try
    while true do
    begin
      Buffer := '';
      while true do
      begin
        TimeVal.tv_sec := 0;
        TimeVal.tv_usec := 10000;

        ReadSet.Count := 1;
        ReadSet.Soc := Socket;

        Result := select(0, @ReadSet, nil, nil, @TimeVal);
        if Result = SOCKET_ERROR then
        begin
          Server.Log.AddText('ERROR|WSA|USER', 'Select() error with ' + IP + ':' + inttostr(Server.IRCPort) + '.');
          raise Exception.Create(Server.Log.Lines[Server.Log.LinesCount- 1].Text);
        end;

        if (ReadSet.Count = 0) or (Result = 0) then
          Break;

        Result := ioctlsocket(Socket, FIONREAD, Bytes);
        if Result = SOCKET_ERROR then
        begin
          Server.Log.AddText('ERROR|WSA|USER', 'Ioctlsock() error with ' + IP + ':' + inttostr(Server.IRCPort) + '.');
          raise Exception.Create(Server.Log.Lines[Server.Log.LinesCount- 1].Text)
        end;

        if Bytes = 0 then
          raise EDisconnect.Create(Name);

        SetLength(Str, Bytes);
        Result := recv(Socket, Str[1], Bytes, 0);

        if (Result = 0) or (Result = SOCKET_ERROR) then
        begin
          Server.Log.AddText('ERROR|WSA|USER|IN', 'I/O socket read error (Socket: ' +inttostr(Socket)+ ' | IP: ' +IP+ ':' +inttostr(Server.HTTPPort)+ ').');
          raise Exception.Create(Server.Log.Lines[Server.Log.LinesCount- 1].Text)
        end;

        SetLength(Str, Result);
        Buffer := Buffer + Str;
      end;



      if (Server.IsBanned(IP)) and(not(Oper)) then
        raise EDisconnect.Create('This IP is banned.');

      if Length(Name) > 0 then
      begin
        if (Server.IsBanned(Name)) and (not(Oper)) then
          raise EDisconnect.Create('This name is banned.');
      end;



      if (Channel <> nil) and (not(Oper)) then
      begin
        Pass := not(Channel.IsUserOnList(Name, IP));

        if Channel.WhiteListMode then
          Pass := not(Pass);

        if not(Pass) then
          Channel.Leave(Self, true, Name + ' banned on this channel.');
      end;


      FloodControl;


      while ExtractLine(Buffer, Str) do
      begin
        Cmd := UpperCase(Copy(Str, 1, Pos(' ', Str + ' ') - 1));
        Delete(Str, 1, Pos(' ', Str + ' '));

        Server.Log.AddText('NETWORK|IN', '(' +Name+ '@' +IP+') ' +Cmd+ ': ' + Str);

        if Cmd = 'PART' then
          CmdPart;

        if Cmd = 'USER' then
          CmdUser(Str);

        if Cmd = 'QUIT' then
          CmdQuit;

        if Cmd = 'NICK' then
          CmdNick(Str);

        if Cmd = 'LIST' then
          CmdList;

        if Cmd = 'WHO' then
          CmdWho;

        if Cmd = 'JOIN' then
          CmdJoin(Str);

        if Cmd = 'PRIVMSG' then
          CmdPrivMsg(Str);

        if Cmd = 'EXPECT' then
          CmdExpect(Str);

        if Cmd = 'PING' then
          CmdPong;
      end;

      Refresh;
    end;
  except
    CloseSocket(Socket);
    Server.RemoveUser(Self);
  end;
end;

procedure TUser.FloodControl;
var Str: string;
begin
  FFloodPoints := FFloodPoints - 1 - MilliSecondsBetween(Now, FTimer);
  if FFloodPoints < 0 then
    FFloodPoints := 0;
  FTimer := Now;

  if FFloodPoints > 50000 then
  begin
    Server.Log.AddText('FLOOD|USER', Name + '@' +IP+ ': kicked for flooding.');
    raise EDisconnect.Create('Flood kick.');
  end
  else
  begin
    if (FFloodPoints > 10000) and (not(FMuted)) then
    begin
      FFloodPoints := FFloodPoints + 30000;
      FMuted := true;
      Str := 'You have been muted for 30 seconds for flooding.';
      Channel.SpaceChanger(Str);
      Server.Log.AddText('FLOOD|USER', Name + '@' +IP+ ': muted for flooding.');
      SendData(':$SERVER$!SERVER@' +Server.Address+ ' NOTICE ' +Name+ ' :' + Str);
    end;

    if (FFloodPoints < 10000) and (FMuted) then
    begin
      FMuted := false;
      Str := 'You have been unmuted.';
      Channel.SpaceChanger(Str);
      Server.Log.AddText('FLOOD|USER', Name + '@' +IP+ ': unmuted.');
      SendData(':$SERVER$!SERVER@' +Server.Address+ ' NOTICE ' +Name+ ' :' + Str);
    end;
  end;
end;

procedure TUser.Refresh;
begin
  if (Length(Name) > 0) and (Channel = nil) and (Length(Name) > 0) then
  begin
    if Server.AutoRefreshInterval then
    begin
      if MillisecondsBetween(Now, FLastWho) > round((Server.UsersInChannelsCount * 0.75 + (Server.UsersCount - Server.UsersInChannelsCount) * 0.25) * 1000) then
        CmdWho;

      if MillisecondsBetween(Now, FLastWho) > Server.ChannelsCount * 1000 then
        CmdList;
      end
      else
      begin
        if MillisecondsBetween(Now, FLastWho) > Server.RefreshInterval then
          CmdWho;

        if MillisecondsBetween(Now, FLastList) > Server.RefreshInterval then
          CmdList;
      end;
  end;
end;

procedure TUser.ServerAnswer(var Str: string);
begin
  if Channel <> nil then
    Channel.SpaceChanger(Str);
  SendData(':$SERVER$!SERVER@' +Server.Address+ ' NOTICE ' +Name+ ' :' + Str);
end;

{ TChannel }

constructor TChannel.Create(ChanName, ChanTopic, ChanVersion, ChanScheme: string; var Srv: TServer);
var a: integer;
begin
  FName := ChanName;
  FTopic := ChanTopic;
  FScheme := ChanScheme;
  FVersion := ChanVersion;
  FWhiteListMode := false;
  SetLength(FBlackList, 0);
  FServer := Srv;
  FGameCounter := 0;

  if FName[1] = '#' then
    Delete(FName, 1, 1);

  SpaceChanger(FName);
  SpaceChanger(FTopic);
  for a := 0 to Server.ChannelsCount - 2 do
  begin
    if Name = Server.Channels[a].Name then
      raise EChannelExists.Create(FName);
  end;
end;

procedure TChannel.GameTimer;
var a: integer;
begin
  a := 0;
  while a < GamesCount do
  begin
    if MinutesBetween(Now, Games[a].Created) > 4 then
    begin
      RemoveGame(a);
      a := 0;
    end
    else
      inc(a);
  end;
end;

procedure TChannel.Broadcast(var From: TUser; Msg: string);
var a: integer;
begin
  for a := 0 to UsersCount - 1 do
  begin
    if not(Users[a] = From) then
      Users[a].SendData(Msg);
  end;
end;

procedure TChannel.Broadcast(Msg: string);
var a: integer;
begin
  for a := 0 to UsersCount - 1 do
    Users[a].SendData(Msg);
end;

procedure TChannel.RemoveGame(Index: integer);
var a: integer;
begin
  for a := Index to GamesCount - 2 do
    FGames[a] := FGames[a + 1];

  SetLength(FGames, GamesCount - 1);
end;

constructor TChannel.Create(var ChanFile: TextFile; var Srv: TServer);
var a: integer;
    Str: string;
begin
  FName := '';
  FTopic := '';
  FScheme := '';
  FVersion := '';
  FWhiteListMode := false;
  SetLength(FBlackList, 0);
  FServer := Srv;
  FGameCounter := 0;

  while not(eof(ChanFile)) do
  begin
    ReadLn(ChanFile, Str);

    while (Str[2] = ' ') and (Length(Str) >= 2) do
      Delete(Str, 2, 1);

    if Length(Str) >= 2 then
    begin
      Str[1] := UpCase(Str[1]);

      if Str[1] = 'N' then
      begin
        Delete(Str, 1, 1);
        FName := Str;
      end;

      if Str[1] = 'T' then
      begin
        Delete(Str, 1, 1);
        FTopic := Str;
      end;

      if Str[1] = 'S' then
      begin
        Delete(Str, 1, 1);
        FScheme := Str;
      end;

      if Str[1] = 'V' then
      begin
        Delete(Str, 1, 1);
        FVersion := Str;
      end;

      if Str[1] = 'B' then
      begin
        Delete(Str, 1, 1);
        AddToBlackList(Str);
      end;

      if Str[1] = 'W' then
      begin
        Delete(Str, 1, 1);

        if Str[1] <> '0' then
          FWhiteListMode := true;
      end;
    end;
  end;

  if FName[1] = '#' then
    Delete(FName, 1, 1);

  a := Length(FScheme);
  while a <> 0 do
  begin
    if FScheme[a] = ' ' then
    begin
      Delete(FScheme, a, 1);
      a := Length(FScheme);
    end;

    dec(a);
  end;

  SpaceChanger(FName);
  SpaceChanger(FTopic);
  for a := 0 to Server.ChannelsCount - 2 do
  begin
    if Name = Server.Channels[a].Name then
      raise EChannelExists.Create(FName);
  end;
end;

destructor TChannel.Destroy;
begin
  SetLength(FUsers, 0);
  SetLength(FBlackList, 0);
  SetLength(FGames, 0);
end;

procedure TChannel.AddToBlackList(BlackName: string);
begin
  if Length(BlackName) > 0 then
  begin
    SetLength(FBlackList, BlackListLength + 1);
    FBlackList[BlackListLength - 1] := BlackName;
    Server.Log.AddText('BAN', BlackName + 'has been added to the #' +Name+ ' black list.');
  end;
end;

procedure TChannel.SpaceChanger(var Str: string);
var a: integer;
begin
  for a := 1 to Length(Str) do
  begin
    if Str[a] = ' ' then
      Str[a] := Chr(160);
  end;
end;

function TChannel.GetGames(Index: integer): TGame;
begin
  if (Index >= 0) and (Index < GamesCount) then
    Result := FGames[Index];
end;

procedure TChannel.AddGame(GName, GPwd, GLoc, Host, IP: string);
var a: integer;
	 Exists: boolean;
begin
  Exists := false;
  for a := 0 to GamesCount - 1 do
  begin
    if Games[a].HosterAddress = IP then
    begin
      Exists := true;
		Break;
    end;
  end;

  if not(Exists) then
  begin
    SetLength(FGames, GamesCount + 1);
	  inc(FGameCounter);
	  with FGames[GamesCount - 1] do
    begin
      Name := GName;
      Password := GPwd;
      Loc := GLoc;
      HosterNickname := Host;
      HosterAddress := IP;
      GameID := FGameCounter;
      Created := Now;
    end;
  end
  else
  begin
    with FGames[a] do
    begin
      Name := GName;
      Password := GPwd;
      Loc := GLoc;
      HosterNickname := Host;
      HosterAddress := IP;
      Created := Now;
    end;
    Server.Log.AddText('FLOOD', Host + '@' +IP+ '#' +Name+ ': second game created on same IP. Replaced old one with new.');
  end;
end;

function TChannel.CloseGame(ID: integer): boolean;
var a, b: integer;
begin
  Result := false;
  for a := 0 to GamesCount - 1 do
  begin
    if Games[a].GameID = ID then
    begin
      for b := a to GamesCount - 2 do
        FGames[b] := FGames[b + 1];
      SetLength(FGames, GamesCount - 1);
      Result := true;
      Break;
    end;
  end;
end;

function TChannel.GetGamesCount: integer;
begin
  Result := Length(FGames);
end;

function TChannel.GetUsers(Index: integer): TUser;
begin
  if (Index >= 0) or (Index < UsersCount) then
    Result :=FUsers[Index]
  else
    Result := nil;
end;

function TChannel.GetBlackList(Index: integer): string;
begin
  if (Index >= 0) and (Index < BlackListLength) then
    Result := FBlackList[Index];
end;

function TChannel.GetBlackListLength: integer;
begin
  Result := Length(FBlackList);
end;

function TChannel.GetUsersCount: integer;
begin
  Result := Length(FUsers);
end;

function TChannel.IsUserOnList(UserName: string; UserIP: string): boolean;
var a: integer;
begin
  Result := false;
  for a := 0 to BlackListLength - 1 do
  begin
    if (UserName= BlackList[a]) or (UserIP = BlackList[a]) then
    begin
      Result := true;
      Break;
    end;
  end;
end;

procedure TChannel.Join(var Usr: TUser);
begin
  SetLength(FUsers, UsersCount + 1);
  FUsers[UsersCount - 1] := Usr;
  Usr.Channel := Self;

  Broadcast(':' +Usr.Name+ '!' +Usr.Name+ '@' +Usr.IP+ ' JOIN :#' + Name);
  if Usr.Oper then
    Broadcast(':' +Server.Address+ ' MODE ' +Name+ ' +o ' + Usr.Name);
end;

procedure TChannel.Leave(var Usr: TUser; Part: boolean; Reason: string);
var a: integer;
begin
  Server.Log.AddText('NETWORK|USER|CHANNEL', Usr.Name + '@' +Usr.IP+ ': has left #' + Name);

  if Part then
  begin
    for a := 0 to UsersCount - 1 do
      Users[a].SendData(':' +Usr.Name+ '!' +Usr.Name+ '@' +Usr.IP+ ' PART :#' + Name);
  end
  else
  begin
    for a := 0 to UsersCount - 1 do
      Users[a].SendData(':' +Usr.Name+ '!' +Usr.Name+ '@' +Usr.IP+ ' QUIT :#' + Name);
  end;

  for a := 0 to UsersCount - 1 do
  begin
    if FUsers[a] = Usr then
      Break;
  end;

  for a := a to UsersCount - 2 do
    FUsers[a] := FUsers[a + 1];

  SetLength(FUsers, UsersCount  - 1);
  Usr.Channel := nil;
end;

function TChannel.RemoveFromBlackList(Rec: string): boolean;
var a: integer;
begin
  Result := false;
  for a := 0 to BlacklistLength - 1 do
  begin
    if Rec = Blacklist[a] then
    begin
      Result := true;
      Break;
    end;
  end;

  if Result then
  begin
    for a := a to BlacklistLength - 2 do
      FBlacklist[a] := FBlacklist[a + 1];

    SetLength(FBlackList, BlackListLength - 1);
  end;
end;

end.
