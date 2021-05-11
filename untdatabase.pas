unit untDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, fphttpapp, SQLDB, untEnv, untDataObj;


procedure initDatabase();
procedure freeDatabase();

function doSearchCardData(AKey: string; ACardType: Integer; AAttr: Integer; AIcon: Integer; ASubType: Integer; ARace: Integer; AMonsterType: Integer; ALang: string): TListCardData;
function doGetOneCard(APassword: Integer; ALang: string): TCardData;
function doGetCardList(AName: string; ALang: string): TListCardNameData;
function doGetRandomCard(ALang: string): TCardData;
function doYdkFindCardList(AByEffect: Boolean; AKey: string; ALang: string): TListCardNameData;
function doYdkGetNamesByIds(AIds: TStringList; ALang: string): TListCardNameData;
function doGetCardCount(): Integer;

implementation

var
  sqlite: TSQLite3Connection = nil;
  trans: TSQLTransaction = nil;
  query: TSQLQuery = nil;

procedure initDatabase();
var
  dbPath: string;
begin
  dbPath := filePath + 'OmegaDB.cdb';
  if (FileExists(dbPath)) then begin
    trans := TSQLTransaction.Create(nil);
    sqlite := TSQLite3Connection.Create(nil);
    sqlite.DatabaseName:= dbPath;
    sqlite.Transaction := trans;
    sqlite.KeepConnection:= False;
    sqlite.Open;
    WriteLn('Database Opened.');
    query:= TSQLQuery.Create(nil);
    query.DataBase := sqlite;
  end else begin
    WriteLn('Database not found!');
  end;
end;

procedure freeDatabase();
begin
  if (query <>  nil) then begin
    query.Clear;
    query.Free;
  end;
  if (sqlite <> nil) then begin
    sqlite.Close(True);
    sqlite.Free;
  end;
  if (trans <> nil) then begin
    trans.Free;
  end;
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
  tn: string;
  sql: string;
  list: TListCardData;
  item: TCardData;
begin
  list := TListCardData.Create;
  tn := getTextTableName(ALang);
  sql := 'select t.id, t.name, t.desc, d.type, d.atk, d.def, d.level, d.race, d.attribute, d.setid from '+tn+' t join datas d on t.id = d.id where 1 = 1';
  AKey:= AKey.Replace('''', '''''');
  if (AKey.Trim() <> '') then sql += ' and (t.name like ''%'+ AKey +'%'' or t.desc like ''%'+AKey+'%'')';
  if (ACardType <> 0) then sql += ' and d.type & %d'.Format([ACardType]);
  if (AAttr <> 0) then sql += ' and d.attribute & %d'.Format([AAttr]);
  if (AIcon <> 0) then sql += ' and d.type & %d'.Format([AIcon]);
  if (ASubType <> 0) then sql += ' and d.type & %d'.Format([ASubType]);
  if (ARace <> 0) then sql += ' and d.race & %d'.Format([ARace]);
  if (AMonsterType <> 0) then sql += ' and d.type & %d'.Format([AMonsterType]);
  query.Clear;
  query.SQL.Text:= sql;
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
    item.setid:= query.FieldByName('setid').AsString;
    list.Add(item);
    query.Next;
  end;
  query.Clear;
  Exit(list);
end;

function doGetOneCard(APassword: Integer; ALang: string): TCardData;
var
  tn: string;
  d: TCardData = nil;
begin
  tn := getTextTableName(ALang);
  query.Clear;
  query.SQL.Text:= 'select t.id, t.name, t.desc, d.type, d.atk, d.def, d.level, d.race, d.attribute, d.setid from %s t join datas d on t.id = d.id where t.id = %d'.Format([tn, APassword]);
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
    d.setid:= query.FieldByName('setid').AsString;
  end;
  query.Clear;
  Exit(d);
end;

function doGetCardList(AName: string; ALang: string): TListCardNameData;
var
  tn: string;
  list: TListCardNameData;
  item: TCardNameData;
begin
  list := TListCardNameData.Create;
  tn := getTextTableName(ALang);
  query.Clear;
  query.SQL.Text:= 'select id, name from '+tn+' where name like ''%'+AName+'%'' limit 10';
  query.Open;
  while not query.EOF do begin
    item := TCardNameData.Create;
    item.id:= query.FieldByName('id').AsInteger;
    item.name:= query.FieldByName('name').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
    list.Add(item);
    query.Next;
  end;
  query.Clear;
  Exit(list);
end;

function doGetRandomCard(ALang: string): TCardData;
var
  tn: string;
  id: Integer = -1;
begin
  tn := getTextTableName(ALang);
  query.Clear;
  query.SQL.Text:= 'select id from '+tn+' where id >= 10000 and id <= 99999999 order by RANDOM() limit 1';
  query.Open;
  if (not query.EOF) then begin
    id := query.FieldByName('id').AsInteger;
  end;
  query.Clear;
  if (id <> -1) then begin
    Exit(doGetOneCard(id, ALang));
  end else begin
    Exit(nil);
  end;
end;

function doYdkFindCardList(AByEffect: Boolean; AKey: string; ALang: string): TListCardNameData;
var
  tn: string;
  kf: string = 'name';
  list: TListCardNameData;
  item: TCardNameData;
begin
  list := TListCardNameData.Create;
  tn := getTextTableName(ALang);
  if (AByEffect) then kf := 'desc';
  query.Clear;
  query.SQL.Text:= 'select id, name from '+tn+' where '+kf+' like ''%'+AKey+'%'' limit 100';
  query.Open;
  while not query.EOF do begin
    item := TCardNameData.Create;
    item.id:= query.FieldByName('id').AsInteger;
    item.name:= query.FieldByName('name').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
    list.Add(item);
    query.Next;
  end;
  query.Clear;
  Exit(list);
end;

function doYdkGetNamesByIds(AIds: TStringList; ALang: string
  ): TListCardNameData;
var
  tn: string;
  list: TListCardNameData;
  item: TCardNameData;
  inStr: string = '';
  i: Integer;
begin
  list := TListCardNameData.Create;
  tn := getTextTableName(ALang);
  query.Clear;

  for i := 0 to AIds.Count - 1 do begin
    inStr += '%s,'.Format([AIds[i]]);
  end;
  inStr:= inStr.TrimRight([',']);
  query.SQL.Text:= 'select id, name from '+tn+' where id in ('+inStr+')';
  query.Open;

  while not query.EOF do begin
    item := TCardNameData.Create;
    item.id:= query.FieldByName('id').AsInteger;
    item.name:= query.FieldByName('name').AsString.Replace('&#64025;', '神', [rfReplaceAll]);
    list.Add(item);
    query.Next;
  end;
  query.Clear;
  Exit(list);
end;

function doGetCardCount(): Integer;
var
  cnt: Integer = 0;
begin
  query.Clear;
  query.SQL.Text:= 'select count(1) ''count'' from datas';
  query.Open;
  if not query.EOF then begin
    cnt := query.FieldByName('count').AsInteger;
  end;
  query.Clear;
  Exit(cnt);
end;

end.

