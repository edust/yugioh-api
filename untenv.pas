{.$DEFINE LOCAL}

unit untEnv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  {$IFNDEF LOCAL}
  MYSQL_HOST = 'localhost';
  {$ELSE}
  MYSQL_HOST = '10.211.55.15';
  {$ENDIF}
  MYSQL_PORT = 3306;
  MYSQL_USER = 'root';
  MYSQL_PASSWORD = 'root';

var
  workPath: String = '';
  filePath: string = '';
  logPath: string = '';

implementation

end.

