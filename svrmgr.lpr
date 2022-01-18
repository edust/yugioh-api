program svrmgr;

{$mode objfpc}{$H+}

uses
  cmem, cthreads,
  Classes, sysutils, fphttpapp, httproute, HTTPDefs, process, Unix;


procedure errorHandler(AResp: TResponse; AErr: Exception; var AHandled: boolean);
begin
  AHandled:= True;
  AResp.ContentType:= 'application/json; chaset=utf-8';
  AResp.Code:= 404;
  AResp.Content:= '{"error": "Request route not found."}';
end;

procedure restart(AReq: TRequest; AResp: TResponse);
var
  ret: cint;
begin
  ret := fpSystem('killall ygosvr');
  WriteLn('KILL PROCESS: ', ret);
  ret := fpSystem('nohup ./ygosvr &');
  WriteLn('START PROCESS: ', ret);
  AResp.Code:= 200;
  AResp.ContentType:= 'application/json; chaset=utf-8';
  AResp.Content:= '{"result": 0}';
end;

begin

  HTTPRouter.RegisterRoute('/api/yugioh/restart', rmPost, @restart, False);

  Application.Port:= 9801;
  Application.QueueSize:= 1000;
  Application.Threaded:= True;
  Application.OnShowRequestException:= @errorHandler;
  Application.Initialize;
  Application.Run;
end.

