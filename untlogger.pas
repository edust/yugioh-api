unit untLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untEnv;

type
  TLogLevel = (lvDebug, lvInfo, lvWarn, lvError);

procedure log(alevel: TLogLevel; msg: string);

implementation

function logLevelToStr(lv: TLogLevel): string;
var
  ret: string = 'debug';
begin
  case lv of
  lvInfo : ret := 'info';
  lvWarn : ret := 'warn';
  lvError : ret := 'error';
  end;
  Exit(ret);
end;

procedure log(alevel: TLogLevel; msg: string);
var
  logPath: string;
  txt: TextFile;
begin
  logPath:= workPath + 'log.txt';
  AssignFile(txt, logPath);
  if (FileExists(logPath)) then begin
    Append(txt);
  end else begin
    Rewrite(txt);
  end;
  WriteLn(txt, Format('[%s][%s] %s', [logLevelToStr(alevel), FormatDateTime('yyyy-MM-dd hh:mm:ss', Now), msg]));
  Flush(txt);
  CloseFile(txt);
end;

end.

