unit untMySQL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql57conn, SQLDB, untEnv, RegExpr, untStringExtension;

procedure initMySQL();
procedure freeMySQL();

function doGetKanaCount(): Integer;
function doGetSetCount(): Integer;
function getNameKanjiKana(aname: string): string;
function getSetKanjiKana(asetname: string): string;
function getEffectKanjiKana(aname: string): string;

implementation

var
  mysql: TMySQL57Connection = nil;
  trans: TSQLTransaction = nil;
  query: TSQLQuery = nil;

procedure reconnect();
var
  st: string;
begin
  st := mysql.ServerStatus.ToLower;
  if (st.Contains('lost connection') or st.Contains('gone away')) then begin
    mysql.Close(True);
    mysql.Open;
  end;
end;

procedure initMySQL();
begin
  mysql := TMySQL57Connection.Create(nil);
  mysql.SkipLibraryVersionCheck:= True;
  trans := TSQLTransaction.Create(nil);
  mysql.Transaction := trans;
  mysql.HostName:= MYSQL_HOST;
  mysql.Port:= MYSQL_PORT;
  mysql.LoginPrompt:= False;
  mysql.CharSet:= 'utf8mb4';
  mysql.UserName:= MYSQL_USER;
  mysql.Password:= MYSQL_PASSWORD;
  mysql.DatabaseName:= 'YugiohAPI';
  mysql.Params.Add('autoReconnect=true');
  mysql.Params.Add('failOverReadOnly=false');
  try
    mysql.Open;
    WriteLn('MySQL Opened,');
  except
    on E: Exception do begin
      WriteLn('Open MYSQL Failed, reason: ' + E.Message);
    end;
  end;
  query := TSQLQuery.Create(nil);
  query.DataBase := mysql;
end;

procedure freeMySQL();
begin
  if (query <>  nil) then begin
    query.Clear;
    query.Free;
  end;
  if (mysql <> nil) then begin
    mysql.Close(True);
    mysql.Free;
  end;
  if (trans <> nil) then begin
    trans.Free;
  end;
end;

function doGetKanaCount(): Integer;
var
  cnt: Integer = 0;
begin
  reconnect();
  query.Clear;
  query.SQL.Text:= 'select count(1) ''count'' from YGOCardName';
  query.Open;
  if not query.EOF then begin
    cnt := query.FieldByName('count').AsInteger;
  end;
  query.Clear;
  Exit(cnt);
end;

function doGetSetCount(): Integer;
var
  cnt: Integer = 0;
begin
  reconnect();
  query.Clear;
  query.SQL.Text:= 'select count(1) ''count'' from YGOSetName';
  query.Open;
  if not query.EOF then begin
    cnt := query.FieldByName('count').AsInteger;
  end;
  query.Clear;
  Exit(cnt);
end;

function getNameKanjiKana(aname: string): string;
var
  ret: string = '';
begin
  reconnect();
  query.Clear;
  query.SQL.Text:= 'select kk from YGOCardName where kanji = ''%s'' or kanji = ''%s'''.Format([aname, toDBC(aname)]);
  query.Open;
  if (not query.EOF) then begin
    ret := toCardName(query.FieldByName('kk').AsString);
  end;
  query.Clear;
  Exit(ret);
end;

function getSetKanjiKana(asetname: string): string;
var
  ret: string = '';
begin
  reconnect();
  query.Clear;
  query.SQL.Text:= 'select kk from YGOSetName where kanji = ''%s'' or kanji = ''%s'''.Format([asetname, toDBC(asetname)]);
  query.Open;
  if (not query.EOF) then begin
    ret := toCardName(query.FieldByName('kk').AsString);
  end;
  query.Clear;
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
  reconnect();
  cn := effectCardNames(aname);
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
      tmp := tmp.Replace('トークン', '', [rfReplaceAll]);
    end;

    kk := getNameKanjiKana(tmp);
    if (kk = '') then begin
      kk := getSetKanjiKana(tmp);
      if (kk = '') then kk := kana(tmp);
    end;
    if (isToken) then kk += 'トークン';
    e2 := e2.Replace(Format('{{%d}}', [i]), kk, [rfReplaceAll]);
  end;
  Exit(e2);
end;

end.

