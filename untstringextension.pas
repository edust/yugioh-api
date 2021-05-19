unit untStringExtension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTF8StringHelper, RegExpr, untNoDbData;


function characterToHalf(str: string): string;
function numberToHalf(str: string): string;
function toDBC(str: string): string;
function toCardName(str: string): string;
function removeKana(str: string): string;

function effectCardNames(str: string): TStringArray;
function kana(str: string): string;
function toDBData(str: string): string;

function sortByLength(strs: TStringArray): TStringArray;

implementation

type
  { TReplaceObj }

  TReplaceObj = class
  public
    function Replace(r: TRegExpr): RegExprString;
    function ReplaceKana(r: TRegExpr): RegExprString;
    function DoPreKana(r: TRegExpr): RegExprString;
  end;

function characterToHalf(str: string): string;
var
  len: Integer;
  i: Integer;
  c: Integer;
  s: WideChar;
  ret: string = '';
  r: TRegExpr;
  ro: TReplaceObj;
begin
  len := str.ULength;
  for i := 1 to len do begin
    s := str.UChar[i];
    c := s.UCode;
    if (s = '　') then begin
      ret += ' ';
    end else if (s = '﹒') then begin
      ret += '·';
    end else if (((s = '＠') or (s = '．') or (s = '＆') or (s = '？') or (s = '！')) or ((c >= 65313) and (c <= 65338)) or ((c >= 65338) and (c <= 65370))) then begin
      ret += Char(c - 65248);
    end else begin
      ret += s.UAnsi;
    end;
  end;

  ro := TReplaceObj.Create;
  r := TRegExpr.Create('「.*?」');
  ret := r.Replace(ret, @ro.Replace);
  r.Free;
  ro.Free;

  Exit(ret);
end;

function numberToHalf(str: string): string;
var
  len: Integer;
  i: Integer;
  ret: string = '';
  s: WideChar;
  c: Integer;
begin
  len := str.ULength;
  for i := 1 to len do begin
    s := str.UChar[i];
    c := s.UCode;
    if (c >= 65296) and (c <= 65305) then begin
     ret += Char(c - 65248);
    end else begin
      ret += s.UAnsi;
    end;
  end;
  Exit(ret);
end;

{ TReplaceObj }

function TReplaceObj.Replace(r: TRegExpr): RegExprString;
var
  s: string;
begin
  s := r.Match[0];
  s := numberToHalf(s);
  Exit(s);
end;

function TReplaceObj.ReplaceKana(r: TRegExpr): RegExprString;
var
  s: string;
begin
  s := r.Match[0];
  s := s.UReplace('[', '', [rfIgnoreCase, rfReplaceAll]);
  s := s.USubstring(1, s.UIndexOf('('));
  Exit(s);
end;

function TReplaceObj.DoPreKana(r: TRegExpr): RegExprString;
var
  s: string;
begin
  s := r.Match[0];
  Exit(Format('|%s|', [s]));
end;

function toDBC(str: string): string;
var
  s: string;
begin
  s := characterToHalf(str);
  s := numberToHalf(s);
  s := s.UReplace('Ɐ', '∀', [rfIgnoreCase, rfReplaceAll]).UReplace('´', '’', [rfIgnoreCase, rfReplaceAll]);
  Exit(s);
end;

function toCardName(str: string): string;
begin
  Exit(str.UReplace('∀', 'Ɐ', [rfIgnoreCase, rfReplaceAll]).UReplace('’', '´', [rfReplaceAll, rfIgnoreCase]));
end;

function removeKana(str: string): string;
var
  r: TRegExpr;
  ro: TReplaceObj;
  retStr: string;
begin
  ro := TReplaceObj.Create;
  r := TRegExpr.Create('\[.*?\(.*?\)]');
  retStr := r.Replace(str, @ro.ReplaceKana);
  ro.Free;
  r.Free;
  Exit(retStr);
end;

function effectCardNames(str: string): TStringArray;
var
  alist: TStringList;
  tmp: string;
  start: Integer;
  leftCount: Integer;
  rightCount: Integer;
  i: Integer;
  tStr: string;
  s: string;
begin
  tmp := str;
  alist := TStringList.Create;
  while tmp.UContains('「') do begin
    start := tmp.UIndexOf('「');
    leftCount:= 1;
    rightCount:= 0;
    for i := start + 1 to tmp.ULength do begin
      s := tmp.UChar[i].UAnsi;
      if (s = '「') then begin
        leftCount += 1;
        Continue;
      end;
      if (s = '」') then begin
        rightCount += 1;
        if (rightCount = leftCount) then begin
          tStr:= tmp.USubstringLen(start + 1, i - start - 1);
          if (alist.IndexOf(tStr) = -1) then alist.Add(tStr);
          tmp := tmp.USubstringLen(i + 1, tmp.ULength - i);
          Break;
        end;
      end;
    end;
  end;
  Result := alist.ToStringArray;
  alist.Free;
end;

function KanaReplaceCallable(item: string): string;
var
  ret: string;
begin
  try
    ret := kanjiKanaMap.KeyData[item];
  except
    ret := '';
  end;
  Exit(ret);
end;

function JoinCallable(item: string): string;
var
  reg: TRegExpr;
  ro: TReplaceObj;
  ret: string;
begin
  reg := TRegExpr.Create('\[.*?\(.*?\)]');
  ro := TReplaceObj.Create;
  if (not reg.Exec(item)) then begin
    ret := item.UReplaceRegex(kanjiKanaReg, @KanaReplaceCallable);
  end else begin
    ret := item;
  end;
  ro.Free;
  reg.Free;
  Exit(ret);
end;

function kana(str: string): string;
var
  reg: TRegExpr;
  ro: TReplaceObj;
  s: string;
  ret: string;
begin
  reg := TRegExpr.Create('\[.*?\(.*?\)]');
  ro := TReplaceObj.Create;
  s := reg.Replace(str, @ro.DoPreKana);
  ret := s.USplit(['|']).Join('', @JoinCallable);
  ro.Free;
  reg.Free;
  Exit(ret);
end;

function toDBData(str: string): string;
var
  s: string;
begin
  s := toDBC(str).UReplace('．', '.', [rfIgnoreCase, rfReplaceAll]).UReplace('－', '-', [rfReplaceAll, rfIgnoreCase]).UTrim;
  if (s.UContains('Fullwidth wordwrap|')) then begin
    s := s.UReplace('Fullwidth wordwrap|', '', [rfIgnoreCase, rfReplaceAll]).UReplace('}}', '', [rfIgnoreCase, rfReplaceAll]);
  end;
  Exit(s);
end;

function sortImpl(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Exit(System.Length(List[Index2]) - System.Length([Index1]));
end;

function sortByLength(strs: TStringArray): TStringArray;
var
  alist: TStringList;
  i: Integer;
begin
  alist := TStringList.Create;
  for i := 0 to Length(strs) - 1 do begin
    alist.Add(strs[i]);
  end;
  alist.CustomSort(@sortImpl);
  Result := alist.ToStringArray;
  alist.Free;
end;

end.

