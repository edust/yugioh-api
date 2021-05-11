unit untExternalExecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, untEnv, jsonparser, fpjson, jsonscanner, base64;

function doNormalKana(AName: string): string;

implementation

function doNormalKana(AName: string): string;
var
  jarPath: string;
  outstr: string;
  parser: TJSONParser;
  json: TJSONObject;
  b64: string;
  retStr: string;
begin
  jarPath:= filePath + 'DIYKana.jar';
  b64 := EncodeStringBase64(AName);
  RunCommand('java', ['-jar', jarPath, b64], outstr, [poWaitOnExit, poUsePipes]);
  parser := TJSONParser.Create(outstr, []);
  json:= TJSONObject(parser.Parse);
  retStr := json.Strings['kana'];
  retStr := DecodeStringBase64(retStr);
  json.Free;
  parser.Free;
  Exit(retStr);
end;

end.

