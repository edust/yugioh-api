unit untLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untEnv, HTTPDefs;

type
  TLogLevel = (lvDebug, lvInfo, lvWarn, lvError);

procedure log(alevel: TLogLevel; AReq: TRequest; msg: string);
procedure log(alevel: TLogLevel; msg: string);

implementation

function getIPAddr(AReq: TRequest): string;
var
  ipAddress: string;
begin
  ipAddress:= AReq.GetCustomHeader('x-forwarded-for').Trim;
  if (ipAddress = '') or (ipAddress.ToLower = 'unknown') then begin
    ipAddress:= AReq.GetCustomHeader('Proxy-Client-IP').Trim;
  end;
  if (ipAddress = '') or (ipAddress.ToLower = 'unknown') then begin
    ipAddress:= AReq.GetCustomHeader('WL-Proxy-Client-IP').Trim;
  end;
  if (ipAddress = '') or (ipAddress.ToLower = 'unknown') then begin
    ipAddress:= AReq.RemoteAddr.Trim;
  end;
  Exit(ipAddress);
end;

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

procedure log(alevel: TLogLevel; AReq: TRequest; msg: string);
var
  lp: string;
  txt: TextFile;
  ip: string;
begin
  lp:= logPath + FormatDateTime('yyyy-MM-dd', Now) + '.txt';
  AssignFile(txt, lp);
  if (FileExists(lp)) then begin
    Append(txt);
  end else begin
    Rewrite(txt);
  end;
  ip := getIPAddr(AReq);
  WriteLn(txt, Format('[%s][%s][%s] %s', [logLevelToStr(alevel), ip, FormatDateTime('yyyy-MM-dd hh:mm:ss', Now), msg]));
  Flush(txt);
  CloseFile(txt);
end;

procedure log(alevel: TLogLevel; msg: string);
var
  lp: string;
  txt: TextFile;
begin
  lp:= logPath + FormatDateTime('yyyy-MM-dd', Now) + '.txt';
  AssignFile(txt, lp);
  if (FileExists(lp)) then begin
    Append(txt);
  end else begin
    Rewrite(txt);
  end;
  WriteLn(txt, Format('[%s][0][%s] %s', [logLevelToStr(alevel), FormatDateTime('yyyy-MM-dd hh:mm:ss', Now), msg]));
  Flush(txt);
  CloseFile(txt);
end;

end.

