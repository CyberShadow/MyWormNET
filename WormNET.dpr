program WormNET;

{$APPTYPE CONSOLE}
{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses
  Class_Server in 'Class_Server.pas',
  Class_Log in 'Class_Log.pas',
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  SysUtils,
  IniFiles,
  WormNATServer in 'WormNATServer.pas';

{$R *.res}

var Log: TLog;
    Server: TServer;
    INI: TINIFile;

procedure Dialog(Str: string);
begin
  WriteLn(Str);
end;

begin
  Server := nil;
  Log := nil;
  
  try
    INI := TINIFile.Create(ExtractFileDir(ParamStr(0)) + '\ServerSettings.ini');
    Log := TLog.Create;
    Log.OnNewLine := Dialog;
    Log.Mask := INI.ReadString('Log', 'Mask', 'GENERAL|ERROR|SERVER');
    Log.ShowTags := INI.ReadBool('Log', 'ShowTags', false);
    Log.FileMask := INI.ReadString('Log', 'FileMask', 'GENERAL|ERROR|SERVER|USER|CHANNEL');
    Log.WriteTags := INI.ReadBool('Log', 'WriteTags', false);

    try
      Server := TServer.Create(INI.ReadString('Server', 'Address', '127.0.0.1'), @Log, INI.ReadInteger('Server', 'HTTPPort', 80), INI.ReadInteger('Server', 'IRCPort', 6667), INI.ReadInteger('Server', 'NATPort', 17018), INI.ReadInteger('Server', 'MaxConnections', 999), INI.ReadInteger('Server', 'RefreshInterval', 1000));
      Server.OperatorPassword := INI.ReadString('Server', 'OperatorPassword', 'abc');
    except
      Log.ExceptLog;
      Server.Free;
      Log.Free;
      INI.Free;
      Exit;
    end;

    Sleep(infinite);
  except
    on E: Exception do
    begin
      WriteLn('Failed on initializing log, closing...');
      Readln;
      Exit;
    end;
  end;
end.
