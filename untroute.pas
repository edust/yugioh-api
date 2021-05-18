unit untRoute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, untEnv, fphttpapp, untDataObj, untMySQL, fpjson, jsonparser, jsonscanner,
  untDataConvert, untStringExtension, untExternalExecutor, httpprotocol, untLogger;

procedure index(AReq: TRequest; AResp: TResponse);
procedure favicon(AReq: TRequest; AResp: TResponse);
procedure systemStatus(AReq: TRequest; AResp: TResponse);
procedure lastSync(AReq: TRequest; AResp: TResponse);

procedure searchCards(AReq: TRequest; AResp: TResponse);
procedure getOneCard(AReq: TRequest; AResp: TResponse);
procedure getCardList(AReq: TRequest; AResp: TResponse);
procedure getRandomCard(AReq: TRequest; AResp: TResponse);
procedure ydkFindCard(AReq: TRequest; AResp: TResponse);
procedure ydkGetNamesByIds(AReq: TRequest; AResp: TResponse);
procedure getCommonCount(AReq: TRequest; AResp: TResponse);
procedure kkCardName(AReq: TRequest; AResp: TResponse);
procedure kkCardEffect(AReq: TRequest; AResp: TResponse);
procedure kkNormalText(AReq: TRequest; AResp: TResponse);

implementation

procedure allowCors(AReq: TRequest; AResp: TResponse);
var
  AOri: string;
begin
  AOri:= AReq.CustomHeaders.Values['Origin'];
  if (AOri = '') then AOri:= '*';
  AResp.SetCustomHeader('Access-Control-Allow-Origin', AOri);
  AResp.SetCustomHeader('Access-Control-Allow-Credentials', 'true');
end;

function StrToJSONEncoded(AStr: string): string;
begin
  Exit(AStr.Replace('"', '\"', [rfIgnoreCase, rfReplaceAll]).Replace(#10, '\n'));
end;

procedure index(AReq: TRequest; AResp: TResponse);
var
  html : string;
begin
  with TStringList.Create do begin
    LoadFromFile(filePath + 'index.html');
    html:= Text;
    Free;
  end;
  AResp.ContentType := 'text/html; charset=utf-8';
  AResp.Content := html;
end;

procedure extractSearchParams(jsonstr: string;
  out AKey: string;
  out ACardType: Integer; out AAttr: Integer; out AIcon: Integer; out ASubType: Integer; out ARace: Integer; out AMonsterType: Integer;
  out ALang: string);
var
  json: TJSONObject;
  parser: TJSONParser;
begin
  parser := TJSONParser.Create(jsonstr, []);
  json := TJSONObject(parser.Parse);
  AKey := json.Strings['key'];
  ACardType := StrToCardType(json.Strings['cardtype']);
  AAttr := StrToAttribute(json.Strings['attribute']);
  AIcon := StrToIcon(json.Strings['icon']);
  ASubType := StrToSubType(json.Strings['subtype']);
  ARace:= StrToRace(json.Strings['race']);
  AMonsterType:= StrToMonsterType(json.Strings['monstertype']);
  ALang:= json.Strings['lang'];
  parser.Free;
  json.Free;
end;

procedure favicon(AReq: TRequest; AResp: TResponse);
begin
  try
    AResp.ContentType:= 'image/x-icon';
    AResp.SetCustomHeader('Accept-Ranges', 'bytes');
    AResp.ContentStream := TMemoryStream.Create;
    TMemoryStream(AResp.ContentStream).LoadFromFile(workPath + 'favicon.ico');
    AResp.ContentLength:= AResp.ContentStream.Size;
    AResp.SendContent;
  finally
    AResp.ContentStream.Free;
  end;
end;

procedure systemStatus(AReq: TRequest; AResp: TResponse);
begin
  log(lvInfo, AReq, 'systemStatus');
  AResp.Code:= 200;
  AResp.ContentType:= 'text/plain; charset=utf-8';
  AResp.Content:= Format('%s [OK]', [FormatDateTime('yyyy-MM-dd hh:mm:ss', Now)]);
  log(lvInfo, AReq, 'systemStatus[end]');
end;

procedure lastSync(AReq: TRequest; AResp: TResponse);
var
  dtStr: string;
  retJson: string;
begin
  dtStr:= doGetLastSyncDate();
  AResp.Code:= 200;
  AResp.ContentType:= 'application/json; charset=utf-8';
  with TRespJsonData.Create(200, '', '"%s"'.Format([dtStr])) do begin
    retJson := toJSON();
    Free;
  end;
  AResp.Content:= retJson;
end;

procedure searchCards(AReq: TRequest; AResp: TResponse);
var
  AKey, ALang: string;
  ACardType, AAttr, AIcon, ASubType, ARace, AMonsterType: Integer;
  list: TListCardData = nil;
  retJson: string;
begin
  allowCors(AReq, AResp);
  extractSearchParams(AReq.Content, AKey, ACardType, AAttr, AIcon, ASubType, ARace, AMonsterType, ALang);
  log(lvInfo, AReq, 'searchCards: ' + AKey);
  if (ALang = '') then ALang:= 'jp';
  AResp.ContentType:= 'application/json; charset=utf-8';
  list := doSearchCardData(AKey, ACardType, AAttr, AIcon, ASubType, ARace, AMonsterType, ALang);
  if (list <> nil) then begin
    AResp.Code:= 200;
    with TRespJsonData.Create(200, '', list.toJSON()) do begin
      retJson := toJSON();
      Free;
    end;
    list.FreeList;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'searchCards[end]');
    Exit;
  end;
  AResp.Code:= 200;
  with TRespData.Create(200, 'no result found.') do begin
    retJson:= toJSON();
    Free;
  end;
  AResp.Content:= retJson;
  log(lvInfo, AReq, 'searchCards[end][no result found]');
end;

procedure getOneCard(AReq: TRequest; AResp: TResponse);
var
  apass: string;
  alang: string;
  apassInt: Integer;
  cardinfo: TCardData;
  retJson: string;
begin
  allowCors(AReq, AResp);
  apass := AReq.RouteParams['password'].Trim;
  alang := AReq.QueryFields.Values['lang'].Trim;
  log(lvInfo, AReq, 'getOneCard: ' + apass + ', lang: ' + alang);
  if (alang = '') then alang := 'jp';
  AResp.ContentType := 'application/json; charset=utf-8';
  if (apass = '') then begin
    AResp.Code := 500;
    with TRespData.Create(500, 'no card id.') do begin
      retJson := toJSON();
      Free;
    end;
    AResp.Content := retJson;
    log(lvInfo, Areq, 'getOneCard[end][no card id]');
    Exit;
  end;
  apassInt := StrToIntDef(apass, 0);
  if (apassInt = 0) then begin
    AResp.Code := 500;
    with TRespData.Create(500, 'card id must be an integer.') do begin
      retJson := toJSON();
      Free;
    end;
    AResp.Content := retJson;
    log(lvInfo, AReq, 'getOneCard[end][card id must be an integer]');
    Exit;
  end;
  cardinfo := doGetOneCard(apassInt, alang);
  if (cardinfo <> nil) then begin
    AResp.Code := 200;
    with TRespJsonData.Create(200, '', cardinfo.toJSON()) do begin
      retJson := toJSON();
      Free;
    end;
    cardinfo.Free;
    AResp.Content := retJson;
    log(lvInfo, AReq, 'getOneCard[end]');
    Exit;
  end;
  AResp.Code := 200;
  with TRespData.Create(200, 'card not found.') do begin
    retJson:= toJSON();
    Free;
  end;
  AResp.Content := retJson;
  log(lvInfo, AReq, 'getOneCard[end][card not found]');
end;

procedure getCardList(AReq: TRequest; AResp: TResponse);
var
  aname: string;
  alang: string;
  retJson: String;
  list: TListCardNameData = nil;
begin
  allowCors(AReq, AResp);
  aname := AReq.QueryFields.Values['name'].Trim;
  alang := Areq.QueryFields.Values['lang'].Trim;
  log(lvInfo, AReq, 'getCardList: ' + aname);
  AResp.Code:= 200;
  AResp.ContentType:= 'application/json; charset=utf-8';
  if (aname = '') then begin
    with TRespJsonData.Create(200, '', '[]') do begin
      retJson:= toJSON();
      Free;
    end;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'getCardList[end][empty]');
    Exit;
  end;
  if (alang = '') then alang:= 'jp';
  list := doGetCardList(aname, alang);
  if (list <> nil) then begin
    AResp.Code:= 200;
    with TRespJsonData.Create(200, '', list.toJSON()) do begin
      retJson := toJSON();
      Free;
    end;
    list.FreeList;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'getCardList[end]');
    Exit;
  end;
  AResp.Code:= 200;
  with TRespData.Create(200, 'no result found.') do begin
    retJson:= toJSON();
    Free;
  end;
  AResp.Content:= retJson;
  log(lvInfo, AReq, 'getCardList[end][no result found]');
end;

procedure getRandomCard(AReq: TRequest; AResp: TResponse);
var
  alang: string;
  card: TCardData = nil;
  retJson: string;
begin
  allowCors(AReq, AResp);
  alang := Areq.QueryFields.Values['lang'].Trim;
  log(lvInfo, AReq, 'getRandomCard: ' + alang);
  if (alang = '') then alang:= 'jp';
  AResp.ContentType:= 'application/json; charset=utf-8';
  card := doGetRandomCard(alang);
  if (card <> nil) then begin
    with TRespJsonData.Create(200, '', card.toJSON()) do begin
      retJson := toJSON();
      Free;
    end;
    card.Free;
    AResp.Code:= 200;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'getRandomCard[end]');
    Exit;
  end;
  AResp.Code:= 500;
  with TRespData.Create(500, 'no result found.') do begin
    retJson := toJSON();
    Free;
  end;
  AResp.Content:= retJson;
  log(lvInfo, AReq, 'getRandomCard[end][no result found]');
end;

procedure extractFindCardParam(jsonstr: string; out AByEffect: Boolean; out AKey: string; out ALang: string);
var
  json: TJSONObject;
  parser: TJSONParser;
begin
  parser := TJSONParser.Create(jsonstr, []);
  json := TJSONObject(parser.Parse);
  AByEffect:= json.Booleans['byEffect'];
  AKey:= json.Strings['key'];
  ALang:= json.Strings['lang'];
  json.Free;
  parser.Free;
end;

procedure ydkFindCard(AReq: TRequest; AResp: TResponse);
var
  AByEffect: Boolean;
  AKey, ALang: string;
  retJson: string;
  list: TListCardNameData;
begin
  allowCors(AReq, AResp);
  extractFindCardParam(AReq.Content, AByEffect, AKey, ALang);
  log(lvInfo, AReq, 'ydkFindCard: ' + BoolToStr(AByEffect) + ', key: ' + AKey);
  if (ALang = '') then ALang:= 'jp';
  AResp.ContentType:= 'application/json; charset=utf-8';
  if (AKey = '') then begin
    AResp.Code:= 500;
    with TRespData.Create(500, 'no key.') do begin
      retJson:= toJSON();
      Free;
    end;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'ydkFindCard[end][no key]');
    Exit;
  end;
  list := doYdkFindCardList(AByEffect, AKey, ALang);
  if (list <> nil) then begin
    with TRespJsonData.Create(200, '', list.toJSON()) do begin
      retJson := toJSON();
      Free;
    end;
    list.FreeList;
    AResp.Code:= 200;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'ydkFindCard[end]');
    Exit;
  end;
  AResp.Code:= 200;
  with TRespJsonData.Create(200, '', '[]') do begin
    retJson:= toJSON();
    Free;
  end;
  AResp.Content:= retJson;
  log(lvInfo, AReq, 'ydkFindCard[end][empty]');
end;

procedure extractGetNamesByIdsParam(jsonstr: string; AList: TStringList; out Alang: string);
var
  parser: TJSONParser;
  json: TJSONObject;
  jarr: TJSONArray;
  i: Integer;
begin
  parser := TJSONParser.Create(jsonstr, []);
  json := TJSONObject(parser.Parse);
  jarr := json.Arrays['ids'];
  for i := 0 to jarr.Count - 1 do begin
    AList.Add(IntToStr(jarr.Integers[i]));
  end;
  Alang:= json.Strings['lang'];
  json.Free;
  parser.Free;
end;

procedure ydkGetNamesByIds(AReq: TRequest; AResp: TResponse);
var
  AList: TStringList;
  ALang: string;
  retJson: string;
  list: TListCardNameData;
begin
  allowCors(AReq, AResp);
  AList := TStringList.Create;
  extractGetNamesByIdsParam(AReq.Content, AList, ALang);
  log(lvInfo, AReq, 'ydkGetNamesByIds: ' + IntToStr(AList.Count) + ', lang: ' + ALang);
  if (ALang = '') then ALang:= 'jp';
  AResp.ContentType:= 'application/json; charset=utf-8';
  if (AList.Count = 0) then begin
    AList.Free;
    AResp.Code:= 500;
    with TRespData.Create(500, 'no card id.') do begin
      retJson:= toJSON();
      Free;
    end;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'ydkGetNamesByIds[end][no card id]');
    Exit;
  end;

  list := doYdkGetNamesByIds(AList, ALang);
  AList.Free;

  if (list <> nil) then begin
    with TRespJsonData.Create(200, '', list.toJSON()) do begin
      retJson := toJSON();
      Free;
    end;
    list.FreeList();
    AResp.Code:= 200;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'ydkGetNamesByIds[end]');
    Exit;
  end;
  AResp.Code:= 200;
  with TRespJsonData.Create(200, '', '[]') do begin
    retJson:= toJSON();
    Free;
  end;
  AResp.Content:= retJson;
  log(lvInfo, AReq, 'ydkGetNamesByIds[end][empty]');
end;

procedure getCommonCount(AReq: TRequest; AResp: TResponse);
var
  cardCount, kanaCount, setCount: Integer;
  retJson: string;
begin
  log(lvInfo, AReq, 'getCommonCount');
  allowCors(AReq, AResp);
  cardCount:= doGetCardCount();
  kanaCount:= doGetKanaCount();
  setCount:= doGetSetCount();
  AResp.Code:= 200;
  AResp.ContentType:= 'application/json; charset=utf-8';
  with TRespJsonData.Create(200, '', '{"cardCount":%d,"kanaCount":%d,"setCount":%d}'.Format([cardCount, kanaCount, setCount])) do begin
    retJson:= toJSON();
    Free;
  end;
  AResp.Content:= retJson;
  log(lvInfo, AReq, 'getCommonCount[end]');
end;

procedure extractKKNameParam(jsonstr: string; out AName: string);
var
  parser: TJSONParser;
  json: TJSONObject;
begin
  parser := TJSONParser.Create(jsonstr, []);
  json := TJSONObject(parser.Parse);
  AName:= json.Strings['name'];
  json.Free;
  parser.Free;
end;

procedure kkCardName(AReq: TRequest; AResp: TResponse);
var
  aname: string;
  retJson: string;
  akana: string;
begin
  allowCors(AReq, AResp);
  extractKKNameParam(AReq.Content, aname);
  log(lvInfo, AReq, 'kkCardName: ' + aname);
  AResp.ContentType:= 'application/json; charset=utf-8';
  if (aname = '') then begin
    with TRespData.Create(500, 'content is null.') do begin
      retJson:= toJSON();
      Free;
    end;
    AResp.Code:= 500;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'kkCardName[end][content is null]');
    Exit;
  end;
  aname:= removeKana(aname);
  akana:= getNameKanjiKana(aname);
  if (akana <> '') then begin
    with TRespJsonData.Create(200, 'found', '"%s"'.Format([StrToJSONEncoded(akana)])) do begin
      retJson := toJSON();
      Free;
    end;
    AResp.Code:= 200;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'kkCardName[end]');
    Exit;
  end;
  AResp.Code:= 200;
  with TRespData.Create(200, 'not found') do begin
    retJson:= toJSON();
    Free;
  end;
  AResp.Content:= retJson;
  log(lvInfo, AReq, 'kkCardName[end][not found]');
end;

procedure kkCardEffect(AReq: TRequest; AResp: TResponse);
var
  aname: String;
  retJson: string;
  akana: string;
begin
  allowCors(AReq, AResp);
  extractKKNameParam(AReq.Content, aname);
  log(lvInfo, AReq, 'kkCardEffect: ' + aname);
  AResp.ContentType:= 'application/json; charset=utf-8';
  if (aname = '') then begin
    with TRespData.Create(500, 'content is null.') do begin
      retJson:= toJSON();
      Free;
    end;
    AResp.Code:= 500;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'kkCardEffect[end][content is null]');
    Exit;
  end;
  aname:= removeKana(aname);
  akana:= getEffectKanjiKana(aname);
  if (akana <> '') then begin
    with TRespJsonData.Create(200, 'found', '"%s"'.Format([StrToJSONEncoded(akana)])) do begin
      retJson := toJSON();
      Free;
    end;
    AResp.Code:= 200;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'kkCardEffect[end]');
    Exit;
  end;
  AResp.Code:= 200;
  with TRespData.Create(200, 'not found') do begin
    retJson:= toJSON();
    Free;
  end;
  AResp.Content:= retJson;
  log(lvInfo, AReq, 'kkCardEffect[end][not found]');
end;

procedure kkNormalText(AReq: TRequest; AResp: TResponse);
var
  aname: string;
  retJson: string;
  akana: string;
begin
  allowCors(AReq, AResp);
  extractKKNameParam(AReq.Content, aname);
  log(lvInfo, AREq, 'kkNormalText: ' + aname);
  AResp.ContentType:= 'application/json; charset=utf-8';
  if (aname = '') then begin
    with TRespData.Create(500, 'content is null.') do begin
      retJson:= toJSON();
      Free;
    end;
    AResp.Code:= 500;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'kkNormalText[end][content is null]');
    Exit;
  end;
  aname:= removeKana(aname);
  akana:= doNormalKana(aname);
  if (akana <> '') then begin
    with TRespJsonData.Create(200, 'found', '"%s"'.Format([StrToJSONEncoded(akana)])) do begin
      retJson := toJSON();
      Free;
    end;
    AResp.Code:= 200;
    AResp.Content:= retJson;
    log(lvInfo, AReq, 'kkNormalText[end]');
    Exit;
  end;
  AResp.Code:= 200;
  with TRespData.Create(200, 'not found') do begin
    retJson:= toJSON();
    Free;
  end;
  AResp.Content:= retJson;
  log(lvInfo, AReq, 'kkNormalText[end][not found]');
end;

end.

