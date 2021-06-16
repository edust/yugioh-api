program ygosvr;

{$mode objfpc}{$H+}

uses
  cthreads, cmem, Classes, sysutils, fpwebfile, fphttpapp, HTTPDefs, httproute,
  fphttp, untEnv, untDataObj, untMySQL, untDataConvert, untStringExtension,
  untNoDbData, untExternalExecutor, untRoute, ISCLogger, ISCConsts, ISCHttp;

procedure showRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
begin
  TLogger.error('main', 'showRequestException: ' + AnException.Message);
  AResponse.Code:= 500;
  AResponse.ContentType:= MIME_JSON;
  AResponse.Content:= '{"error": "%s"}'.Format([StrToJSONEncoded(AnException.Message)]);
  handled:= True;
end;

// var
//  jvmPath: string;
//  retJvm: Boolean;
begin
  loadEnv();
  // jvmPath:= ISCJvmPath;
  //if (FileExists(jvmPath)) then begin
  //  retJvm := ISCInitJVM(jvmPath, '.:./files/DIYKana.jar');
  //  if (retJvm) then begin
  //    TLogger.info('main', 'init jvm completed.');
  //  end else begin
  //    TLogger.error('main', 'init jvm failed.');
  //  end;
  //end else begin
  //  TLogger.error('main', 'jvm library not exists.');
  //end;

  // database
  initNoDbData();

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

  Application.OnShowRequestException:= @showRequestException;
  Application.QueueSize:= 1000;
  Application.Port:= SERVER_PORT;
  Application.Threaded:=True;
  Application.Initialize;
  Application.Run;

  freeNoDbData();
end.

