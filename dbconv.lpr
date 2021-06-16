program dbconv;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, mysql57conn, SQLite3Conn, SQLDB, DB, TypInfo, fphttpclient, fpopenssl, openssl, opensslsockets, ssockets, DateUtils, IniFiles;

const
  SQLITE_FILE = 'OmegaDB.cdb';
var
  MYSQL_HOST: string = '127.0.0.1';
  MYSQL_PORT: Integer = 3306;
  MYSQL_USER: string = 'root';
  MYSQL_PASSWORD: string = 'root';

const
  SQL_CREATE_DATAS = 'CREATE TABLE `datas` (`id` INT PRIMARY KEY NOT NULL DEFAULT 0, `ot` INT NOT NULL DEFAULT 0, `alias` INT NOT NULL DEFAULT 0, `setcode` INT NOT NULL DEFAULT 0, `type` INT NOT NULL DEFAULT 0, `atk` INT NOT NULL DEFAULT 0, `def` INT NOT NULL DEFAULT 0, `level` INT NOT NULL DEFAULT 0, `race` INT NOT NULL DEFAULT 0, `attribute` INT NOT NULL DEFAULT 0, `category` INT NOT NULL DEFAULT 0, `genre` INT NOT NULL DEFAULT 0, `setid` TEXT, `script` TEXT, `support` INT NOT NULL DEFAULT 0) character set utf8mb4;';
  SQL_CREATE_LANGS = 'CREATE TABLE `%s` (`id` INT PRIMARY KEY NOT NULL DEFAULT 0, `name` VARCHAR(512) NOT NULL DEFAULT '''', `desc` TEXT, `str1` TEXT, `str2` TEXT, `str3` TEXT, `str4` TEXT, `str5` TEXT, `str6` TEXT, `str7` TEXT, `str8` TEXT, `str9` TEXT, `str10` TEXT, `str11` TEXT, `str12` TEXT, `str13` TEXT, `str14` TEXT, `str15` TEXT, `str16` TEXT) character set utf8mb4;';

var
  mTrans: TSQLTransaction;
  mQuery: TSQLQuery;
  mDb: TMySQL57Connection;

var
  sTrans: TSQLTransaction;
  sQuery: TSQLQuery;
  sDb: TSQLite3Connection;

type
  TDataReceiveEvent = class
  public
    Procedure onDownloading(Sender : TObject; Const ContentLength, CurrentPos : Int64);
  end;

procedure TDataReceiveEvent.onDownloading(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  Write(#13'downloading: %d'.Format([CurrentPos]));
end;

function downloadDatabase(): string;
var
  http: TFPHTTPClient;
  path: string;
  sqlitePath: string;
  evt: TDataReceiveEvent;
begin
  InitSSLInterface;
  WriteLn('downloading database ...');
  path := ExtractFilePath(ParamStr(0));
  if (path.EndsWith('./')) then path := path.Substring(0, path.Length - 2);
  path += 'download/';
  if (not DirectoryExists(path)) then begin
    ForceDirectories(path);
  end;
  sqlitePath:= path + SQLITE_FILE;
  if (FileExists(sqlitePath)) then begin
    Exit(sqlitePath);
    // DeleteFile(sqlitePath);
  end;
  evt := TDataReceiveEvent.Create;
  http := TFPHTTPClient.Create(nil);
  http.OnDataReceived:= @evt.onDownloading;
  http.AllowRedirect:= True;
  try
    http.Get('https://duelistsunite.org/omega/OmegaDB.cdb', sqlitePath);
  except
    on E: Exception do begin
      DeleteFile(sqlitePath);
      WriteLn('download error: ' + E.Message);
    end;
  end;
  http.Free;
  evt.Free;
  WriteLn();
  Exit(sqlitePath);
end;

function toSQLStr(s: string): string;
begin
  Exit(s.Replace('''', '\''', [rfIgnoreCase, rfReplaceAll]).Replace(#13#10, '\n', [rfIgnoreCase, rfReplaceAll]).Replace(#10, '\n', [rfIgnoreCase, rfReplaceAll]).Replace(#13, '\n', [rfIgnoreCase, rfReplaceAll]));
end;

function genSQL(tn: string; q: TSQLQuery): string;
var
  fields: string = '';
  values: string = '';
  i: Integer;
  ret: string;
begin
  for i := 0 to q.FieldCount - 1 do begin
    fields += '`%s`,'.Format([q.Fields[i].FieldName]);
    Case q.Fields[i].DataType of
      ftAutoInc, ftInteger: values += '%d,'.Format([q.Fields[i].AsInteger]);
      ftMemo, ftBlob: values += '''%s'','.Format([toSQLStr(q.Fields[i].AsString)]);
    end;
  end;
  fields:= fields.TrimRight([',']);
  values := values.TrimRight([',']);
  ret := 'insert into %s (%s) values (%s);'.Format([tn, fields, values]);
  Exit(ret);
end;

function convertTable(tableName: string): Integer;
var
  tableList: TStringList;
  cnt: Integer = 0;
  sqliteCount: Integer;
  mysqlCount: Integer;
begin
  WriteLn('convert %s...'.Format([tableName]));

  // delete existed table
  tableList := TStringList.Create;
  mDb.GetTableNames(tableList, False);
  if (tableList.IndexOf(tableName) <> -1) then begin
    // if table exists, check the row count

    sQuery.Clear;
    sQuery.SQL.Text:= 'select count(1) from %s'.Format([tableName]);
    sQuery.Open;
    sqliteCount := sQuery.Fields[0].AsInteger;
    sQuery.Clear;

    mQuery.Clear;
    mQuery.SQL.Text:= 'select count(1) from %s'.Format([tableName]);
    mQuery.Open;
    mysqlCount:= mQuery.Fields[0].AsInteger;
    mQuery.Clear;

    if (sqliteCount = mysqlCount) then begin
      // do not need to convert
      tableList.Free;
      Exit(0);
    end;

    mQuery.Clear;
    mQuery.SQL.Text:= 'drop table %s'.Format([tableName]);
    mQuery.ExecSQL;
    mTrans.Commit;
  end;
  tableList.Free;

  // prepare base data
  sQuery.Clear;
  sQuery.SQL.Text:= 'select * from %s'.Format([tableName]);
  sQuery.Open;

  // create new table
  mQuery.Clear;
  if (tableName = 'datas') then begin
    mQuery.SQL.Text:= SQL_CREATE_DATAS;
  end else begin
    mQuery.SQL.Text:= SQL_CREATE_LANGS.Format([tableName]);
  end;
  mQuery.ExecSQL;
  mTrans.Commit;

  // import data
  mQuery.Clear;
  mQuery.SQL.Text:= 'select * from %s'.Format([tableName]);
  mQuery.Open;

  while (not sQuery.EOF) do begin
    Inc(cnt);
    mQuery.SQL.Text:= genSQL(tableName, sQuery);
    mQuery.ExecSQL;
    sQuery.Next;
  end;
  mQuery.ApplyUpdates;
  mTrans.Commit;
  sQuery.Clear;
  Exit(cnt);
end;

procedure loadEnv();
const
  SEC_DATABAE = 'database';
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ygosvr.cfg');
  MYSQL_HOST:= ini.ReadString(SEC_DATABAE, 'host', 'localhost');
  MYSQL_PORT:= ini.ReadInteger(SEC_DATABAE, 'port', 3306);
  MYSQL_USER:= ini.ReadString(SEC_DATABAE, 'user', 'root');
  MYSQL_PASSWORD:= ini.ReadString(SEC_DATABAE, 'password', 'root');
  ini.Free;
end;

var
  sqlitePath: string;
  tables: array[0..11] of string = ('datas', 'ja_texts', 'zhcn_texts', 'zhtw_texts', 'texts', 'ko_texts', 'de_texts', 'es_texts', 'fr_texts', 'it_texts', 'th_texts', 'vi_texts');
  // tables : array[0..0] of string = ('ja_texts');
  tn: string;
  dataCount: Integer = 0;
  timeStart, timeEnd: Int64;
begin
  loadEnv();

  sqlitePath:= downloadDatabase();
  WriteLn('sqlite path: ' + sqlitePath);
  if (not FileExists(sqlitePath)) then begin
    WriteLn('download failed.');
    Exit;
  end;

  sTrans := TSQLTransaction.Create(nil);
  sDb := TSQLite3Connection.Create(nil);
  sDb.DatabaseName:= sqlitePath;
  sDb.Transaction := sTrans;
  sDb.CharSet:= 'utf8';
  sDb.KeepConnection:= True;
  sDb.Open;
  WriteLn('SQLite Opened.');
  sQuery := TSQLQuery.Create(nil);
  sQuery.DataBase := sDb;

  mTrans := TSQLTransaction.Create(nil);
  mDb := TMySQL57Connection.Create(nil);
  mDb.SkipLibraryVersionCheck:= True;
  mDb.Transaction := mTrans;
  mDb.HostName:= MYSQL_HOST;
  mDb.Port:= MYSQL_PORT;
  mDb.LoginPrompt:= False;
  mDb.KeepConnection:= True;
  mDb.CharSet:= 'utf8';
  mDb.UserName:= MYSQL_USER;
  mDb.Password:= MYSQL_PASSWORD;
  mDb.DatabaseName:= 'YugiohAPI';
  mDb.Params.Add('useUnicode=true');
  mDb.Params.Add('characterEncoding=utf8');
  mTrans.DataBase := mDb;
  mDb.Open;

  mDb.ExecuteDirect('SET CHARACTER SET utf8');
  mDb.ExecuteDirect('SET NAMES utf8');
  mTrans.Commit;

  WriteLn('MySQL Opened.');

  mQuery := TSQLQuery.Create(nil);
  mQuery.DataBase := mDb;
  mQuery.Transaction := mTrans;

  timeStart:= GetTickCount64;

  for tn in tables do begin
    dataCount += convertTable(tn);
  end;

  timeEnd:= GetTickCount64;

  mQuery.Clear;
  mQuery.SQL.Text:= 'update YGOSync set lastsync = %d'.Format([DateTimeToUnix(Now, False)]);
  mQuery.ExecSQL;
  mTrans.Commit;

  mQuery.Clear;
  mQuery.Free;

  mDb.Close(True);
  mDb.Free;
  mTrans.Free;

  sQuery.Clear;
  sQuery.Free;
  sDb.Close(True);
  sDb.Free;
  sTrans.Free;

  WriteLn('DONE');
  WriteLn('time used: %d sec.'.Format([((timeEnd - timeStart) div 1000)]));
  WriteLn('data count: %d'.Format([dataCount]));
end.

