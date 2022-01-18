unit untMySQL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql80conn, SQLDB, untEnv, untDataObj, RegExpr, untStringExtension, ISCLogger, DateUtils, untExternalExecutor;

type
  TMySQLConnection = TMySQL80Connection;

procedure initMySQL(out conn: TMySQLConnection; out query: TSQLQuery; out trans: TSQLTransaction);
procedure freeMySQL(conn: TMySQLConnection; query: TSQLQuery; trans: TSQLTransaction);

function doGetKanaCount(): Integer;
function doGetSetCount(): Integer;
function getNameKanjiKana(aname: string): string;
function getSetKanjiKana(asetname: string): string;
function getEffectKanjiKana(aname: string): string;
function getNormalKanjiKana(AName: string): string;

// port from sqlite
function doSearchCardData(AKey: string; ACardType: Integer; AAttr: Integer; AIcon: Integer; ASubType: Integer; ARace: Integer; AMonsterType: Integer; ALang: string): TListCardData;
function doGetOneCard(APassword: Integer; ALang: string): TCardData;
function doGetCardList(AName: string; ALang: string): TListCardNameData;
function doGetRandomCard(ALang: string): TCardData;
function doYdkFindCardList(AByEffect: Boolean; AKey: string; ALang: string): TListCardNameData;
function doYdkGetNamesByIds(AIds: TStringList; ALang: string): TListCardNameData;
function doGetCardCount(): Integer;

function doGetLastSyncDate(): string;

implementation

function GenPackName(lang: string; abbr: string): string;
var
  r: Integer;
  rstr: string;
begin
  Randomize;
  r := Random(100);
  if (r = 0) then begin
    r := 1;
  end;
  rstr:= IntToStr(r);
  if (rstr.Length < 2) then begin
    rstr:= '0' + rstr;
  end;
  if (lang.ToLower = 'ko') then begin
    lang:= 'kr';
  end;
  Result := abbr.ToUpper + '-' + lang.ToUpper + '0' + rstr;
end;

procedure initMySQL(out conn: TMySQLConnection; out query: TSQLQuery; out
  trans: TSQLTransaction);
var
  succ: Boolean;
begin
  conn := TMySQLConnection.Create(nil);
  conn.SkipLibraryVersionCheck:= True;
  trans := TSQLTransaction.Create(nil);
  conn.Transaction := trans;
  conn.HostName:= MYSQL_HOST;
  conn.Port:= MYSQL_PORT;
  conn.LoginPrompt:= False;
  conn.CharSet:= 'utf8';
  conn.UserName:= MYSQL_USER;
  conn.Password:= MYSQL_PASSWORD;
  conn.DatabaseName:= 'YugiohAPI';
  try
    conn.Open;
    succ := True;
  except
    on E: Exception do begin
      succ:= False;
      TLogger.error('mysql', 'Open MYSQL Failed, reason: ' + E.Message);
    end;
  end;
  if (not succ) then begin
    // connect failed, free all components
    conn.Free;
    trans.Free;
    conn := nil;
    trans := nil;
    query := nil;
  end else begin
    query := TSQLQuery.Create(nil);
    query.DataBase := conn;
  end;
end;

procedure freeMySQL(conn: TMySQLConnection; query: TSQLQuery;
  trans: TSQLTransaction);
begin
  if (query <>  nil) then begin
    query.Clear;
    query.Free;
  end;
  if (conn <> nil) then begin
    conn.Close(True);
    conn.Free;
  end;
  if (trans <> nil) then begin
    trans.Free;
  end;
end;

function doGetKanaCount(): Integer;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  cnt: Integer = 0;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit(0);
  query.Clear;
  query.SQL.Text:= 'select count(1) ''count'' from YGOCardName';
  try
    query.Open;
    if not query.EOF then begin
      cnt := query.FieldByName('count').AsInteger;
    end;
    query.Clear;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'doGetKanaCount: ' + E.Message);
    end;
  end;
  freeMySQL(conn, query,trans);
  Exit(cnt);
end;

function doGetSetCount(): Integer;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  cnt: Integer = 0;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit(0);
  query.Clear;
  query.SQL.Text:= 'select count(1) ''count'' from YGOSetName';
  try
    query.Open;
    if not query.EOF then begin
      cnt := query.FieldByName('count').AsInteger;
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'doGetSetCount: ' + E.Message);
    end;
  end;
  query.Clear;
  freeMySQL(conn, query,trans);
  Exit(cnt);
end;

function getNameKanjiKana(aname: string): string;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  ret: string = '';
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit('');
  query.Close;
  query.SQL.Text:= 'select kk from YGOCardName where kanji = ''%s'' or kanji = ''%s'''.Format([aname, toDBC(aname)]);
  try
    query.Open;
    if (not query.EOF) then begin
      ret := toCardName(query.FieldByName('kk').AsString);
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'getNameKanjiKana: ' + E.Message);
    end;
  end;
  query.Close;
  freeMySQL(conn, query,trans);
  Exit(ret);
end;

function getSetKanjiKana(asetname: string): string;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  ret: string = '';
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit('');
  query.Clear;
  query.SQL.Text:= 'select kk from YGOSetName where kanji = ''%s'' or kanji = ''%s'''.Format([asetname, toDBC(asetname)]);
  try
    query.Open;
    if (not query.EOF) then begin
      ret := toCardName(query.FieldByName('kk').AsString);
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'getSetKanjiKana: ' + E.Message);
    end;
  end;
  query.Clear;
  freeMySQL(conn, query,trans);
  Exit(ret);
end;

function getEffectKanjiKana(aname: string): string;
var
  cn: TStringArray;
  e2: string;
  i: Integer;
  isToken: Boolean;
  tmp: string;
  kk: string;
begin
  cn := effectCardNames(aname);
  cn := sortByLength(cn);
  e2 := aname;
  for i := 0 to Length(cn) - 1 do begin
    e2 := e2.Replace(cn[i], '{{%d}}'.Format([i]), [rfReplaceAll, rfIgnoreCase]);
  end;
  e2 := kana(e2);
  for i := 0 to Length(cn) - 1 do begin
    isToken:= False;
    tmp := cn[i];
    if (tmp.EndsWith('トークン')) then begin
      isToken:= True;
    end;
    kk := getNameKanjiKana(tmp);
    if (kk = '') then begin
      tmp := tmp.Replace('トークン', '', [rfReplaceAll]).Trim;
      kk := getSetKanjiKana(tmp);
      if (kk = '') then kk := kana(tmp);
    end;
    if (isToken) and (not kk.EndsWith('トークン')) then kk += 'トークン';
    e2 := e2.Replace(Format('{{%d}}', [i]), kk, [rfReplaceAll]);
  end;
  Exit(e2);
end;

function getNormalKanjiKana(AName: string): string;
var
  cn: TStringArray;
  e2: string;
  i: Integer;
  isToken: Boolean;
  tmp: string;
  kk: string;
begin
  cn := effectCardNames(AName);
  cn := sortByLength(cn);
  e2 := AName;
  for i := 0 to Length(cn) - 1 do begin
    e2 := e2.Replace(cn[i], '{{%d}}'.Format([i]), [rfReplaceAll, rfIgnoreCase]);
  end;
  e2 := doNormalKana(e2);
  for i := 0 to Length(cn) - 1 do begin
    isToken:= False;
    tmp := cn[i];
    if (tmp.EndsWith('トークン')) then begin
      isToken:= True;
    end;
    kk := getNameKanjiKana(tmp);
    if (kk = '') then begin
      tmp := tmp.Replace('トークン', '', [rfReplaceAll]).Trim;
      kk := getSetKanjiKana(tmp);
      if (kk = '') then kk := kana(tmp);
    end;
    if (isToken) and (not kk.EndsWith('トークン')) then kk += 'トークン';
    e2 := e2.Replace(Format('{{%d}}', [i]), kk, [rfReplaceAll]);
  end;
  Exit(e2);
end;

function ToDBStr(str: string): string;
begin
  Exit(str
    .Replace('''', '''''', [rfIgnoreCase, rfReplaceAll])
    .Replace(#13, '', [rfReplaceAll, rfIgnoreCase])
    .Replace(#10, '', [rfIgnoreCase, rfReplaceAll])
  );
end;

function getTextTableName(ALang: string): string;
var
  tn: string = 'ja_texts';
begin
  case ALang of
  'sc': tn := 'zhcn_texts';
  'tc': tn := 'zhtw_texts';
  'en': tn := 'texts';
  'kr': tn := 'ko_texts';
  'de': tn := 'de_texts';
  'es': tn := 'es_texts';
  'fr': tn := 'fr_texts';
  'it': tn := 'it_texts';
  'th': tn := 'th_texts';
  'vi': tn := 'vi_texts';
  end;
  Exit(tn);
end;

function doSearchCardData(AKey: string; ACardType: Integer; AAttr: Integer;
  AIcon: Integer; ASubType: Integer; ARace: Integer; AMonsterType: Integer;
  ALang: string): TListCardData;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  tn: string;
  sql: string;
  list: TListCardData;
  item: TCardData;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit(nil);
  list := TListCardData.Create;
  tn := getTextTableName(ALang);
  sql := 'select t.id, t.name, t.desc, d.type, d.atk, d.def, d.level, d.race, d.attribute, d.setid from '+tn+' t join datas d on t.id = d.id where 1 = 1';
  AKey:= AKey.Replace('''', '''''');
  if (AKey.Trim() <> '') then sql += ' and (t.name like ''%'+ ToDBStr(AKey) +'%'' or t.desc like ''%'+ ToDBStr(AKey) + '%'')';
  if (ACardType <> 0) then sql += ' and d.type & %d'.Format([ACardType]);
  if (AAttr <> 0) then sql += ' and d.attribute & %d'.Format([AAttr]);
  if (AIcon <> 0) then sql += ' and d.type & %d'.Format([AIcon]);
  if (ASubType <> 0) then sql += ' and d.type & %d'.Format([ASubType]);
  if (ARace <> 0) then sql += ' and d.race & %d'.Format([ARace]);
  if (AMonsterType <> 0) then sql += ' and d.type & %d'.Format([AMonsterType]);
  query.Clear;
  query.SQL.Text:= sql;
  try
    query.Open;
    while not query.EOF do begin
      item := TCardData.Create;
      item.id:= query.FieldByName('id').AsInteger;
      item.name:= query.FieldByName('name').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
      item.desc:= query.FieldByName('desc').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
      item.&type:= query.FieldByName('type').AsInteger;
      item.atk:= query.FieldByName('atk').AsInteger;
      item.def:= query.FieldByName('def').AsInteger;
      item.level:= query.FieldByName('level').AsInteger;
      item.race:= query.FieldByName('race').AsInteger;
      item.attribute:= query.FieldByName('attribute').AsInteger;
      item.setid:= GenPackName(ALang, query.FieldByName('setid').AsString);
      list.Add(item);
      query.Next;
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'doSearchCardData: ' + E.Message);
    end;
  end;
  query.Clear;
  freeMySQL(conn, query, trans);
  Exit(list);
end;

function doGetOneCard(APassword: Integer; ALang: string): TCardData;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  tn: string;
  d: TCardData = nil;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit(nil);
  tn := getTextTableName(ALang);
  query.Clear;
  query.SQL.Text:= 'select t.id, t.name, t.desc, d.type, d.atk, d.def, d.level, d.race, d.attribute, d.setid from %s t join datas d on t.id = d.id where t.id = %d'.Format([tn, APassword]);
  try
    query.Open;
    if (not query.EOF) then begin
      d := TCardData.Create;
      d.id:= query.FieldByName('id').AsInteger;
      d.name:= query.FieldByName('name').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
      d.desc:= query.FieldByName('desc').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
      d.&type:= query.FieldByName('type').AsInteger;
      d.atk:= query.FieldByName('atk').AsInteger;
      d.def:= query.FieldByName('def').AsInteger;
      d.level:= query.FieldByName('level').AsInteger;
      d.race:= query.FieldByName('race').AsInteger;
      d.attribute:= query.FieldByName('attribute').AsInteger;
      d.setid:= GenPackName(ALang, query.FieldByName('setid').AsString);
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'doGetOneCard: ' + E.Message);
    end;
  end;
  query.Clear;
  freeMySQL(conn, query, trans);
  Exit(d);
end;

function doGetCardList(AName: string; ALang: string): TListCardNameData;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  tn: string;
  list: TListCardNameData;
  item: TCardNameData;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit(nil);
  list := TListCardNameData.Create;
  tn := getTextTableName(ALang);
  query.Clear;
  query.SQL.Text:= 'select id, name from '+tn+' where name like ''%'+ToDBStr(AName)+'%'' limit 0,10';
  try
    query.Open;
    while not query.EOF do begin
      item := TCardNameData.Create;
      item.id:= query.FieldByName('id').AsInteger;
      item.name:= query.FieldByName('name').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
      list.Add(item);
      query.Next;
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'doGetCardList: '+ E.Message);
    end;
  end;
  query.Clear;
  freeMySQL(conn, query, trans);
  Exit(list);
end;

function doGetRandomCard(ALang: string): TCardData;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  tn: string;
  id: Integer = -1;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit(nil);
  tn := getTextTableName(ALang);
  query.Clear;
  query.SQL.Text:= 'select id from '+tn+' where id >= 10000 and id <= 99999999 order by RAND() limit 0,1';
  try
    query.Open;
    if (not query.EOF) then begin
      id := query.FieldByName('id').AsInteger;
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'doGetRandomCard: ' + E.Message);
    end;
  end;
  query.Clear;
  freeMySQL(conn, query, trans);
  if (id <> -1) then begin
    Exit(doGetOneCard(id, ALang));
  end else begin
    Exit(nil);
  end;
end;

function doYdkFindCardList(AByEffect: Boolean; AKey: string; ALang: string): TListCardNameData;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  tn: string;
  kf: string = 'name';
  list: TListCardNameData;
  item: TCardNameData;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit(nil);
  list := TListCardNameData.Create;
  tn := getTextTableName(ALang);
  if (AByEffect) then kf := 'desc';
  query.Clear;
  query.SQL.Text:= 'select id, name from '+tn+' where '+kf+' like ''%'+ToDBStr(AKey)+'%'' limit 0,100';
  try
    query.Open;
    while not query.EOF do begin
      item := TCardNameData.Create;
      item.id:= query.FieldByName('id').AsInteger;
      item.name:= query.FieldByName('name').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
      list.Add(item);
      query.Next;
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'doYdkFindCardList: ' + E.Message);
    end;
  end;
  query.Clear;
  freeMySQL(conn, query, trans);
  Exit(list);
end;

function doYdkGetNamesByIds(AIds: TStringList; ALang: string
  ): TListCardNameData;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  tn: string;
  list: TListCardNameData;
  item: TCardNameData;
  inStr: string = '';
  i: Integer;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit(nil);
  list := TListCardNameData.Create;
  tn := getTextTableName(ALang);
  query.Clear;

  for i := 0 to AIds.Count - 1 do begin
    inStr += '%s,'.Format([AIds[i]]);
  end;
  inStr:= inStr.TrimRight([',']);
  query.SQL.Text:= 'select id, name from '+tn+' where id in ('+inStr+')';
  try
    query.Open;
    while not query.EOF do begin
      item := TCardNameData.Create;
      item.id:= query.FieldByName('id').AsInteger;
      item.name:= query.FieldByName('name').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
      list.Add(item);
      query.Next;
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'doYdkGetNamesByIds: ' + E.Message);
    end;
  end;
  query.Clear;
  freeMySQL(conn, query, trans);
  Exit(list);
end;

function doGetCardCount(): Integer;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  cnt: Integer = 0;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit(0);
  query.Clear;
  query.SQL.Text:= 'select count(1) ''count'' from datas';
  try
    query.Open;
    if not query.EOF then begin
      cnt := query.FieldByName('count').AsInteger;
    end;
  except
    on E: Exception do begin
      TLogger.error('mysql', 'doGetCardCount: ' + E.Message);
    end;
  end;
  query.Clear;
  freeMySQL(conn, query, trans);
  Exit(cnt);
end;

function doGetLastSyncDate(): string;
var
  conn: TMySQLConnection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  dtStr: string = 'unknown';
  dtLong: LongInt;
  dt: TDateTime;
begin
  initMySQL(conn, query, trans);
  if (conn = nil) then Exit('');
  query.Clear;
  query.SQL.Text:= 'select lastsync from YGOSync';
  query.Open;
  dtLong := query.FieldByName('lastsync').AsInteger;
  dt := UnixToDateTime(dtLong, False);
  dtStr:= FormatDateTime('yyyy-MM-dd hh:mm:ss', dt);
  query.Clear;
  freeMySQL(conn, query, trans);
  Exit(dtStr);
end;

end.

