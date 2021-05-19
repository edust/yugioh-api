program kanaAnalysis;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, SQLDB, mysql57conn, DB, fgl, untStringExtension;

const
  MYSQL_HOST = 'localhost';
  MYSQL_PORT = 3306;
  MYSQL_USER = 'root';
  MYSQL_PASSWORD = 'root';

type
  TCardMap = specialize TFPGMap<LongInt, String>;

var
  mysql: TMySQL57Connection;
  query: TSQLQuery;
  trans: TSQLTransaction;
  map: TCardMap;


procedure getDiff();
var
  AKanaList: TStringList;
  n: string;
  idx: Integer = 0;
begin
  AKanaList := TStringList.Create;
  query.Clear;
  query.SQL.Text:= 'select kanji from YGOCardName';
  query.Open;
  WriteLn('Get kanji list: ' + IntToStr(query.RecordCount));
  while (not query.EOF) do begin
    AKanaList.Add(toDBData(query.FieldByName('kanji').AsString));
    query.Next;
  end;
  query.Clear;
  query.SQL.Text:= 'select a.id, a.name from ja_texts a join datas b on a.id = b.id where b.setid not like ''RD/%'';';
  query.Open;
  WriteLn('get card name list: ' + IntToStr(query.RecordCount));
  while (not query.EOF) do begin
    Inc(idx);
    n := toDBData(query.FieldByName('name').AsString);
    Write(#13'[%d] %s'#9#9#9#9#9#9#9#9.Format([idx, n]));
    if (AKanaList.IndexOf(n) = -1) then begin
      map.Add(query.FieldByName('id').AsInteger, n);
    end;
    query.Next;
  end;
  query.Clear;
  AKanaList.Free;
  WriteLn();
end;

procedure saveDiff();
var
  fn: string;
  list: TStringList;
  i: Integer;
  sql: string;
begin
  WriteLn('save card list.');
  fn := ExtractFilePath(ParamStr(0)) + 'NoKana.txt';
  list := TStringList.Create;

  for i := 0 to map.Count - 1 do begin
    sql := 'insert into YGOCardName(pack, kanji, kana, kk, done, donetime) values (''Omega'',''%s'', ''%s'', '''',0, 0);'.Format([map.Data[i], map.Data[i]]);
    if (list.IndexOf(sql) = -1) then list.Add(sql);
  end;
  list.SaveToFile(fn);
  list.Free;
end;

begin
  map := TCardMap.Create;
  trans := TSQLTransaction.Create(nil);
  mysql := TMySQL57Connection.Create(nil);
  mysql.SkipLibraryVersionCheck:= True;
  mysql.KeepConnection:= True;
  mysql.HostName:= MYSQL_HOST;
  mysql.Port:= MYSQL_PORT;
  mysql.UserName:= MYSQL_USER;
  mysql.Password:= MYSQL_PASSWORD;
  mysql.CharSet:= 'utf8';
  mysql.DatabaseName:= 'YugiohAPI';
  mysql.Transaction := trans;
  mysql.Open;
  query:= TSQLQuery.Create(nil);
  query.DataBase := mysql;
  getDiff();
  saveDiff();
  query.Clear;
  query.Free;
  mysql.Close(True);
  mysql.Free;
  trans.Free;
  map.Free;
end.

