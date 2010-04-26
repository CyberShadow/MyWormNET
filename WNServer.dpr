// WormNet server.

program WNServer; 

uses
{$IFDEF WIN32}
  {$APPTYPE CONSOLE}
  Windows, WinSock,
{$ELSE}
  cthreads, FakeWinSock,
{$ENDIF}
  SysUtils, Base, IniFiles, HTTPServer, IRCServer, WormNATServer, Data;

var
  Config: TMemIniFile;
{$IFDEF WIN32}
  WSA: TWSAData;
{$ENDIF}
 
begin
  ChDir(ExtractFilePath(ExpandFileName(ParamStr(0))));
  //Log('------------------ '+DateTimeToStr(Now)+' ------------------');
  EventLog('------------------ '+DateTimeToStr(Now)+' ------------------');

  Config := TMemIniFile.Create(ExtractFilePath(ParamStr(0))+'WNServer.ini');
  ServerHost      :=Config.ReadString ('WormNet','ServerHost',     'localhost');
  IRCPort         :=Config.ReadInteger('WormNet','IRCPort',               6667);
  HTTPPort        :=Config.ReadInteger('WormNet','HTTPPort',                80);
  WormNATPort     :=Config.ReadInteger('WormNet','WormNATPort',          17018);
  IRCOperPassword :=Config.ReadString ('WormNet','IRCOperPassword', 'password');
  IRCChannel      :=Config.ReadString ('WormNet','IRCChannel',      '#Welcome');

  {$IFDEF WIN32}
  WSAStartUp(2, WSA);
  {$ENDIF}
  if IRCPort>0 then
    StartIRCServer;
  Sleep(50);
  if HTTPPort>0 then
    StartHTTPServer;
  Sleep(50);
  if WormNATPort>0 then
    StartWormNATServer;
  Sleep(INFINITE);
end.
