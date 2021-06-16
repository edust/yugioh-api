unit untExternalExecutor;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, process, untEnv, jsonparser, fpjson, jsonscanner, base64;

function doNormalKana(AName: string): string;

implementation

uses
  ISCConsts;

function doNormalKana(AName: string): string;
var
  jarPath: string;
  outstr: string;
  parser: TJSONParser;
  json: TJSONObject;
  b64: string;
  retStr: string;
begin
  jarPath:= FILES_DIR + 'DIYKana.jar';
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


(*
function doNormalKana(AName: string): string;
  function execImpl(env: PJNIEnv): string;
  var
    c: jclass;
    m: jmethodID;
    ps: jstring;
    p: jvalueArray = nil;
    s: jstring;
  begin
    c := env^^.FindClass(env, 'com/rarnu/diy/kana/KanaUtil');
    m := env^^.GetStaticMethodID(env, c, 'kana', '(Ljava/lang/String;)Ljava/lang/String;');
    SetLength(p, 1);
    ps := TJNIEnv.StringToJString(env, AName);
    p[0].l:= ps;
    s := env^^.CallStaticObjectMethodA(env, c, m, Pjvalue(p));
    Result := TJNIEnv.JStringToString(env, s);

    env^^.DeleteLocalRef(env, ps);
    env^^.DeleteLocalRef(env, s);
    env^^.DeleteLocalRef(env, c);
  end;

begin
  Exit(specialize ISCJVMExecute<string>(@execImpl, AName));
end;
*)

end.

