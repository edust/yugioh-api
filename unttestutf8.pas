unit untTestUTF8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTF8StringHelper;

procedure testUTF8Helper();

implementation

procedure testUTF8Helper();
const
  STR = 'ブルーアイズ・ホワイト・ドラゴン';
var
  len: Integer;
  i: Integer;
  w: WideChar;
begin
  len := STR.ULength;
  for i:= 1 to len do begin
    w := STR.UChar[i];
    WriteLn(w);
  end;
end;

end.

