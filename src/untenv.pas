unit untEnv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

var
  MYSQL_HOST: string = 'localhost';
  MYSQL_PORT: Integer = 3306;
  MYSQL_USER: string = 'root';
  MYSQL_PASSWORD: string = 'root';

procedure loadEnv();

implementation

procedure loadEnv();
const
  SEC_DATABASE = 'database';
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.cfg'));
  MYSQL_HOST:= ini.ReadString(SEC_DATABASE, 'host', 'localhost');
  MYSQL_PORT:= ini.ReadInteger(SEC_DATABASE, 'port', 3306);
  MYSQL_USER:= ini.ReadString(SEC_DATABASE, 'user', 'root');
  MYSQL_PASSWORD:= ini.ReadString(SEC_DATABASE, 'password', '');
  ini.Free;
end;

end.

