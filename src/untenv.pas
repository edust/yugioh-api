unit untEnv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ISCYaml;

var
  MYSQL_HOST: string = 'localhost';
  MYSQL_PORT: Integer = 3306;
  MYSQL_USER: string = 'root';
  MYSQL_PASSWORD: string = 'root';

procedure loadEnv(AYaml: TYamlFile);

implementation

procedure loadEnv(AYaml: TYamlFile);
begin
  MYSQL_HOST:= AYaml.GetValue('database.host', 'localhost');
  MYSQL_PORT:= StrToIntDef(AYaml.GetValue('database.port', '3306'), 3306);
  MYSQL_USER:= AYaml.GetValue('database.user', 'root');
  MYSQL_PASSWORD:= AYaml.GetValue('database.password', 'root');
end;

end.

