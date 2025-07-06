program ra2launcher;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  sha1,
  Process,
  getopts;

  {$R *.res}

  // https://www.mail-archive.com/fpc-pascal@lists.freepascal.org/msg53129.html
  function GetFileSize(const aFileName: string): int64;
  var
    Info: TSearchRec;
  begin
    if FindFirst(aFileName, 0, Info) = 0 then
      Result := Info.Size
    else
      Result := -1; // File not found
    FindClose(Info);
  end;

  function CalculateFileSHA1(const FileName: string): string;
  begin
    Result := SHA1Print(SHA1File(FileName));
  end;

  function Get_IsSteam(const gamePath: string): boolean;
  const
    steamGameSha1Hash = 'd1715d5328910b572545883cbb71314340ba52fb';
  const
    cncfdGameSha1Hash = 'dc6cee520441316a18b7dd6a75c3189d06de52b8';
  const
    originGameSha1Hash = 'cc85b793ab729c2a7f6f08ee51a09cef952b9377';
  const
    retailCdUncrackedGameSha1Hash = '4a9d7f822af8326a826b67e1d28c9f085f8a0f97';
  const
    steamGameSizeInByteAtLeast = 4800000; // greater or equal than this size: steam
  var
    gameHash: string;
  var
    gameSizeInByte: int64;
  begin
    gameHash := CalculateFileSHA1(gamePath);
    gameSizeInByte := GetFileSize(gamePath);
    WriteLn('SHA-1 hash of game.exe: ', gameHash);
    WriteLn('Size (bytes) of game.exe: ', gameSizeInByte);

    // Determine by hash
    if gameHash = steamGameSha1Hash then
    begin
      Exit(True);
    end
    else if gameHash = cncfdGameSha1Hash then
    begin
      Exit(False);
    end
    else if gameHash = originGameSha1Hash then
    begin
      Exit(False);
    end
    else if gameHash = retailCdUncrackedGameSha1Hash then
    begin
      Exit(False);
    end;

    // Failed. Might be a cracked version. Determine by size
    Exit(gameSizeInByte >= steamGameSizeInByteAtLeast);
  end;

var
  isSteam: boolean;
var
  appPath: string;
var
  gamePath: string;
var
  ra2Path: string;
var
  gameProcess: TProcess;
var
  i: integer;
begin
  WriteLn('Launcher for Red Alert 2');
  WriteLn('Author: SadPencil');
  WriteLn('This program automatically determines whether the game is a Steam installation or not, and launches the appropriate game executable.');

  appPath := ExtractFileDir(ParamStr(0));
  gamePath := appPath + '\game.exe';
  ra2Path := appPath + '\Ra2.exe';

  if not FileExists(gamePath) then
  begin
    WriteLn('Error: game.exe not found.');
    ExitCode := 2;
    Exit();
  end;

  if not FileExists(ra2Path) then
  begin
    isSteam := True;
  end
  else
  begin
    isSteam := Get_IsSteam(gamePath);
  end;

  gameProcess := TProcess.Create(nil);

  if isSteam then
  begin
    Writeln('Launching game.exe');
    gameProcess.Executable := gamePath;
  end
  else
  begin
    Writeln('Launching Ra2.exe');
    gameProcess.Executable := ra2Path;
  end;
  for i := 1 to ParamCount() do
  begin
    gameProcess.Parameters.Add(ParamStr(i));
  end;

  gameProcess.Execute();
  gameProcess.Free();
end.
