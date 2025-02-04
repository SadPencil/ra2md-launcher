program ra2mdlauncher;

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

  function Get_IsSteam(const gameMdPath: string): boolean;
  const
    steamGameMdSha1HashV1 = '1b57b09d9912023eda3124b959d5f274e9a9bd5f';
  const
    steamGameMdSha1HashV2 = 'f5ae9d38f1f628f8bc0c27b1be463c3f5c5d811b';
  const
    cncfdGameMdSha1Hash = '6fdf606f9b08ee4c5813aed373953fa1c822e934';
  const
    originGameMdSha1Hash = '3ffb7cc1240164c4fc94d093fe5abe43691940ac';
  const
    retailCdUncrackedGameMdSha1Hash = '6314ca9a8254d70d6e38bf436da86b23b5345a91';
  const
    steamGameMdSizeInByteAtLeast = 5000000; // greater or equal than this size: steam
  var
    gameMdHash: string;
  var
    gameMdSizeInByte: int64;
  begin
    gameMdHash := CalculateFileSHA1(gameMdPath);
    gameMdSizeInByte := GetFileSize(gameMdPath);
    WriteLn('SHA-1 hash of gamemd.exe: ', gameMdHash);
    WriteLn('Size (bytes) of gamemd.exe: ', GetFileSize(gameMdPath));

    // Determine by hash
    if gameMdHash = steamGameMdSha1HashV1 then
    begin
      Exit(True);
    end
    else if gameMdHash = steamGameMdSha1HashV2 then
    begin
      Exit(True);
    end
    else if gameMdHash = cncfdGameMdSha1Hash then
    begin
      Exit(False);
    end
    else if gameMdHash = originGameMdSha1Hash then
    begin
      Exit(False);
    end
    else if gameMdHash = retailCdUncrackedGameMdSha1Hash then
    begin
      Exit(False);
    end;

    // Failed. Might be a cracked version. Determine by size
    Exit(gameMdSizeInByte >= steamGameMdSizeInByteAtLeast);
  end;

var
  isSteam: boolean;
var
  appPath: string;
var
  gameMdPath: string;
var
  ra2MdPath: string;
var
  gameProcess: TProcess;
var
  i: integer;
begin
  WriteLn('Launcher for Yuri''s Revenge');
  WriteLn('Author: SadPencil');
  WriteLn('This program automatically determines whether the game is a Steam installation or not, and launches the appropriate game executable.');

  appPath := ExtractFileDir(ParamStr(0));
  gameMdPath := appPath + '\gamemd.exe';
  ra2MdPath := appPath + '\Ra2md.exe';

  if not FileExists(gameMdPath) then
  begin
    WriteLn('Error: gamemd.exe not found.');
    ExitCode := 2;
    Exit();
  end;

  if not FileExists(ra2MdPath) then
  begin
    isSteam := True;
  end
  else
  begin
    isSteam := Get_IsSteam(gameMdPath);
  end;

  gameProcess := TProcess.Create(nil);

  if isSteam then
  begin
    Writeln('Launching gamemd.exe');
    gameProcess.Executable := gameMdPath;
  end
  else
  begin
    Writeln('Launching Ra2md.exe');
    gameProcess.Executable := ra2MdPath;
  end;
  for i := 1 to ParamCount() do
  begin
    gameProcess.Parameters.Add(ParamStr(i));
  end;

  gameProcess.Execute();
  gameProcess.Free();
end.
