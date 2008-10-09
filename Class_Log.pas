unit Class_Log;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Classes, SysUtils;

type
  TLog = class;

  PLog = ^TLog;

  TLogLine = record
    Tags: array of string;
    Time: string;
    Text: string;
  end;

  TLogEvent = procedure(Str: string);

  TLog = class(TObject)
  private
    FFile: TextFile;
    FMask: array of string;
    FFileMask: array of string;
    FShowTags: boolean;
    FWriteTags: boolean;
    FLines: array of TLogLine;

    FOnNewLine: TLogEvent;

    function GetLines(Index: integer): TLogLine;
    function GetLinesCount: integer;
    procedure SetMask(const Value: string);
    function GetMask: string;
    function GetFileMask: string;
    procedure SetFileMask(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddText(Tags: string; Text: string);
    procedure ExceptLog;

    property Lines[Index: integer]: TLogLine read GetLines;
    property LinesCount: integer read GetLinesCount;
    property Mask: string read GetMask write SetMask;
    property FileMask: string read GetFileMask write SetFileMask;
    property ShowTags: boolean read FShowTags write FShowTags;
    property WriteTags: boolean read FWriteTags write FWriteTags;

    property OnNewLine: TLogEvent read FOnNewLine write FOnNewLine;
  end;

implementation

{ TLog }

constructor TLog.Create;
begin
  SetLength(FMask, 0);
  SetLength(FFileMask, 0);
  FShowTags := false;
  FWriteTags := false;
  AssignFile(FFile, 'Log.txt');

  if FileExists('Log.txt') then
    Append(FFile)
  else
  begin
    Rewrite(FFile);
    WriteLn(FFile, '---------------------- myWormNET 2 Log ---------------------' + #13#10);
  end;

  WriteLn(FFile, '-------------------- ' +DateTimeToStr(Now)+ ' --------------------');

  FLines := nil;
  FOnNewLine := nil;
end;

destructor TLog.Destroy;
begin
  SetLength(FLines, 0);
  CloseFile(FFile);
end;

procedure TLog.ExceptLog;
var a, b: integer;
    Str: string;
begin
  CloseFile(FFile);
  if FileExists('ExpectLog.txt') then
    DeleteFile('ExpectLog.txt');
  
  AssignFile(FFile, 'ExpectLog.txt');
  Rewrite(FFile);

  WriteLn(FFile, '------------------ myWormmNET 2 Except Log ------------------' + #13#10);
  WriteLn(FFile, '-------------------- ' +DateTimeToStr(Now)+ ' --------------------');

  for a := 0 to LinesCount - 1 do
  begin
    Str := '[' +Lines[a].Time+ '] [';
    for b := 0 to Length(Lines[a].Tags) - 1 do
      Str := Lines[a].Tags[b] + '|';
    Delete(Str, Length(Str), 1);
    Str := Str + '] ' + Lines[a].Text;

    WriteLn(FFile, Str);
  end;

  CloseFile(FFile);
end;

procedure TLog.AddText(Tags: string; Text: string);
var Pass: boolean;
    Str: string;
    a, b: integer;
    label OutOfLoop1;
    label OutOfLoop2;
begin
  if Length(Text) > 0 then
  begin
    SetLength(FLines, LinesCount + 1);
    FLines[LinesCount - 1].Text := Text;
    FLines[LinesCount - 1].Time := TimeToStr(Now);

    while Length(Tags) > 0 do
    begin
      SetLength(FLines[LinesCount - 1].Tags, Length(FLines[LinesCount - 1].Tags) + 1);
      if Pos('|', Tags) = 0 then
      begin
        FLines[LinesCount - 1].Tags[Length(FLines[LinesCount - 1].Tags) - 1] := Tags;
        SetLength(Tags, 0);
      end
      else
      begin
        FLines[LinesCount - 1].Tags[Length(FLines[LinesCount - 1].Tags) - 1] := Copy(Tags, 1, Pos('|', Tags) - 1);
        Delete(Tags, 1, Pos('|', Tags));
      end;
    end;

    Pass := false;
    if Length(FFileMask) = 0 then
      Pass := true
    else
    begin
      for a := 0 to Length(FFileMask) - 1 do
      begin
        for b := 0 to Length(Lines[LinesCount - 1].Tags) - 1 do
        begin
          if SameText(Lines[LinesCount - 1].Tags[b], FFileMask[a]) then
          begin
            Pass := true;
            goto OutOfLoop1;
          end;
        end;
      end;

      OutOfLoop1:;
    end;

    if Pass then
    begin
      if WriteTags then
      begin
        Str := '[' +Lines[LinesCount - 1].Time+ '] [';
        for a := 0 to Length(Lines[LinesCount - 1].Tags) - 1 do
          Str := Str + Lines[LinesCount - 1].Tags[a] + '|';
        Delete(Str, Length(Str), 1);
        Str := Str + '] ' + Lines[LinesCount - 1].Text;
      end
      else
        Str := '[' +Lines[LinesCount - 1].Time+ '] ' + Lines[LinesCount - 1].Text;

      WriteLn(FFile, Str);
    end;

    Pass := false;
    if Length(FMask) = 0 then
      Pass := true
    else
    begin
      for a := 0 to Length(FMask) - 1 do
      begin
        for b := 0 to Length(Lines[LinesCount - 1].Tags) - 1 do
        begin
          if SameText(Lines[LinesCount - 1].Tags[b], FMask[a]) then
          begin
            Pass := true;
            goto OutOfLoop2;
          end;
        end;
      end;

      OutOfLoop2:;
    end;

    if (Assigned(FOnNewLine)) and (Pass) then
    begin
      if ShowTags then
      begin
        Str := '[' +Lines[LinesCount - 1].Time+ '] [';
        for a := 0 to Length(Lines[LinesCount - 1].Tags) - 1 do
          Str := Str + Lines[LinesCount - 1].Tags[a] + '|';
        Delete(Str, Length(Str), 1);
        Str := Str + '] ' + Lines[LinesCount - 1].Text;
      end
      else
        Str := '[' +Lines[LinesCount - 1].Time+ '] ' + Lines[LinesCount - 1].Text;

      FOnNewLine(Str);
    end;
  end;
end;

function TLog.GetFileMask: string;
var a: integer;
begin
  Result := '';
  for a := 0 to Length(FFileMask) - 1 do
    Result := FFileMask[a] + '|';

  Delete(Result, 1, 1);
end;

function TLog.GetLines(Index: integer): TLogLine;
begin
  if (Index >= 0) and (Index < LinesCount) then
    Result := FLines[Index];
end;

function TLog.GetLinesCount: integer;
begin
  Result := Length(FLines);
end;

function TLog.GetMask: string;
var a: integer;
begin
  Result := '';
  for a := 0 to Length(FMask) - 1 do
    Result := FMask[a] + '|';

  Delete(Result, 1, 1);
end;

procedure TLog.SetFileMask(const Value: string);
var Str: string;
begin
  Str := AnsiUpperCase(Value);
  SetLength(FFileMask, 0);
  while Length(Str) > 0 do
  begin
    SetLength(FFileMask, Length(FFileMask) + 1);
    if Pos('|', Str) = 0 then
    begin
      FFileMask[Length(FFileMask) - 1] := Str;
      SetLength(Str, 0);
    end
    else
    begin
      FFileMask[Length(FFileMask) - 1] := Copy(Str, 1, Pos('|', Str) - 1);
      Delete(Str, 1, Pos('|', Str));
    end;
  end;
end;

procedure TLog.SetMask(const Value: string);
var Str: string;
begin
  Str := AnsiUpperCase(Value);
  SetLength(FMask, 0);
  while Length(Str) > 0 do
  begin
    SetLength(FMask, Length(FMask) + 1);
    if Pos('|', Str) = 0 then
    begin
      FMask[Length(FMask) - 1] := Str;
      SetLength(Str, 0);
    end
    else
    begin
      FMask[Length(FMask) - 1] := Copy(Str, 1, Pos('|', Str) - 1);
      Delete(Str, 1, Pos('|', Str));
    end;
  end;
end;

end.
