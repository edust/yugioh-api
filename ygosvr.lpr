{.$DEFINE DEBUG}

// yum install sqlite-devel
// yum install mysql-devel

program ygosvr;

{$mode objfpc}{$H+}

uses
  cthreads, cmem, Classes, sysutils, fpwebfile, fphttpapp, HTTPDefs, httproute, fphttp,
  untRoute, untEnv, untDataObj, untMySQL, untDataConvert,
  untStringExtension, untNoDbData, untExternalExecutor, untLogger{$IFDEF DEBUG}, untTest, untTestUTF8 {$ENDIF};

procedure showRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
begin
  log(lvError, 'showRequestException: ' + AnException.Message);
  AResponse.Code:= 500;
  AResponse.ContentType:= 'application/json';
  AResponse.Content:= '{"error": "%s"}'.Format([StrToJSONEncoded(AnException.Message)]);
  handled:= True;
end;

begin
  // fill env
  workPath:= ExtractFilePath(Application.ExeName);
  if (workPath.EndsWith('./')) then begin
    workPath:= workPath.Substring(0, workPath.Length - 2);
  end;
  WriteLn('workPath: ' + workPath);
  filePath:= workPath + 'files/';
  WriteLn('filePath: ' + filePath);
  logPath:= workPath + 'logs/';
  if (not DirectoryExists(logPath)) then begin
    ForceDirectories(logPath);
  end;
  WriteLn('logPath: ' + logPath);

  // database
  initNoDbData();

  // file location
  RegisterFileLocation('static', filePath);

  // common
  HTTPRouter.RegisterRoute('/', rmAll, @index);
  HTTPRouter.RegisterRoute('/favicon.ico', rmAll, @favicon);
  HTTPRouter.RegisterRoute('/system/status', rmAll, @systemStatus);
  HTTPRouter.RegisterRoute('/system/syncStatus', rmAll, @lastSync);
  HTTPRouter.RegisterRoute('/api/common/count', rmAll, @getCommonCount);

  // yugioh
  HTTPRouter.RegisterRoute('/api/yugioh/search', rmPost, @searchCards);
  HTTPRouter.RegisterRoute('/api/yugioh/list', rmAll, @getCardList);
  HTTPRouter.RegisterRoute('/api/yugioh/card/:password', rmAll, @getOneCard);

  HTTPRouter.RegisterRoute('/api/yugioh/random', rmAll, @getRandomCard);

  // ydk
  HTTPRouter.RegisterRoute('/api/ydk/find', rmPost, @ydkFindCard);
  HTTPRouter.RegisterRoute('/api/ydk/names', rmPost, @ydkGetNamesByIds);

  // kanji-kana
  HTTPRouter.RegisterRoute('/api/kanjikana/name', rmPost, @kkCardName);
  HTTPRouter.RegisterRoute('/api/kanjikana/effect', rmPost, @kkCardEffect);
  HTTPRouter.RegisterRoute('/api/kanjikana/text', rmPost, @kkNormalText);

  {$IFNDEF DEBUG}
  Application.OnShowRequestException:= @showRequestException;
  Application.QueueSize:= 1000;
  Application.Port:=9800;
  Application.Threaded:=True;
  Application.Initialize;
  Application.Run;
  {$ELSE}
  testGetOneCard();
  testSearchCard();
  testJsonArray();
  testRegexp();
  testUTF8();
  testEffectCardName();
  testUTF8Helper();
  {$ENDIF}

  freeNoDbData();
end.

