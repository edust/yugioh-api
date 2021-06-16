unit untNoDbData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, RegExpr, untEnv, fpjson, jsonparser, jsonscanner;

type
  TKanjiKanaMap = specialize TFPGMap<String, String>;

var
  kanjiKanaMap: TKanjiKanaMap = nil;
  kanjiKanaReg: TRegExpr = nil;

procedure initNoDbData();
procedure freeNoDbData();

implementation

uses ISCConsts;

function sortKeys(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Exit(List[Index2].Length - List[Index1].Length);
end;

procedure initNoDbData();
var
  parser: TJSONParser;
  json: TJSONObject;
  jsonPath: string;
  jsonstr: string;
  i: Integer;
  k: string;
  alist: TStringList;
  regStr: string;
begin
  kanjiKanaMap := TKanjiKanaMap.Create;
  jsonPath:= FILES_DIR + 'kanji-kana.json';
  with TStringList.Create do begin
    LoadFromFile(jsonPath);
    jsonstr:= Text;
    Free;
  end;
  parser := TJSONParser.Create(jsonstr, []);
  json := TJSONObject(parser.Parse);
  for i := 0 to json.Count - 1 do begin
    k := json.Names[i];
    kanjiKanaMap.Add(k, json.Strings[k]);
  end;
  json.Free;
  parser.Free;

  alist := TStringList.Create;
  for i := 0 to kanjiKanaMap.Count - 1 do begin
    alist.Add(kanjiKanaMap.Keys[i]);
  end;
  alist.CustomSort(@sortKeys);
  regstr := String.Join('|', alist.ToStringArray);
  alist.Free;
  kanjiKanaReg := TRegExpr.Create(regStr);
end;

procedure freeNoDbData();
begin
  if (kanjiKanaMap <> nil) then kanjiKanaMap.Free;
  if (kanjiKanaReg <> nil) then kanjiKanaReg.Free;
end;

end.

