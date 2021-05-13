unit untDataObj;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils, fgl;

type

  { TCardData }

  TCardData = class
  private
    Fatk: Integer;
    Fattribute: Integer;
    Fdef: Integer;
    Fdesc: string;
    Fid: Integer;
    Flevel: Integer;
    Fname: string;
    Frace: Integer;
    Fsetid: string;
    Ftype: Integer;
  public
    function toJSON(): string;
  published
    property id: Integer read Fid write Fid;
    property name: string read Fname write Fname;
    property desc: string read Fdesc write Fdesc;
    property &type: Integer read Ftype write Ftype;
    property atk: Integer read Fatk write Fatk;
    property def: Integer read Fdef write Fdef;
    property level: Integer read Flevel write Flevel;
    property race: Integer read Frace write Frace;
    property attribute: Integer read Fattribute write Fattribute;
    property setid: string read Fsetid write Fsetid;
  end;

  TListCardData = specialize TFPGList<TCardData>;

  { TCardNameData }

  TCardNameData = class
  private
    Fid: Integer;
    Fname: string;
  public
    function toJSON(): string;
  published
    property id: Integer read Fid write Fid;
    property name: string read Fname write Fname;
  end;

  TListCardNameData = specialize TFPGList<TCardNameData>;

  { TRespData }

  TRespData = class
  private
    Fcode: Integer;
    Fmessage: string;
  public
    constructor Create(ACode: Integer; AMessage: string);
    function toJSON(): string; virtual;
  published
    property code: Integer read Fcode write Fcode;
    property message: string read Fmessage write Fmessage;
  end;

  { TRespJsonData }

  TRespJsonData = class(TRespData)
  private
    Fdata: string;
  public
    constructor Create(ACode: Integer; AMessage: string; AData: string);
    function toJSON(): string; override;
  published
    property data: string read Fdata write Fdata;
  end;

type

  { TListCardDataHelper }

  TListCardDataHelper = type helper for TListCardData
  public
    procedure FreeList();
    function toJSON(): string;
  end;

  { TTListCardNameDataHelper }

  TTListCardNameDataHelper = type helper for TListCardNameData
  public
    procedure FreeList();
    function toJSON(): string;
  end;

function StrToJSONEncoded(AStr: string): string;

implementation

function StrToJSONEncoded(AStr: string): string;
begin
  Exit(AStr
    .Replace('"', '\"', [rfIgnoreCase, rfReplaceAll])
    .Replace(#13#10, '\n', [rfReplaceAll, rfIgnoreCase])
    .Replace(#10, '\n', [rfIgnoreCase, rfReplaceAll])
    .Replace(#13, '\n', [rfIgnoreCase, rfReplaceAll])
    );
end;

{ TCardNameData }

function TCardNameData.toJSON(): string;
var
  str: string;
begin
  str := '{';
  str += '"id": %d,'.Format([Fid]);
  str += '"name": "%s"'.Format([StrToJSONEncoded(Fname)]);
  str += '}';
  Exit(str);
end;

{ TTListCardNameDataHelper }

procedure TTListCardNameDataHelper.FreeList();
var
  i: Integer;
begin
  for i:= 0 to self.Count - 1 do begin
    self.Items[i].Free;
  end;
  self.Free;
end;

function TTListCardNameDataHelper.toJSON(): string;
var
  str: string;
  i: Integer;
begin
  str := '[';
  for i := 0 to Self.Count - 1 do begin
    str += self.Items[i].toJSON() + ',';
  end;
  str := str.TrimRight([',']);
  str += ']';
  Exit(str);
end;

{ TRespJsonData }

constructor TRespJsonData.Create(ACode: Integer; AMessage: string; AData: string);
begin
  Inherited Create(ACode, AMessage);
  Fdata := AData;
end;

function TRespJsonData.toJSON(): string;
begin
  Exit('{"code":%d, "message": "%s", "data": %s}'.Format([Fcode, Fmessage, Fdata]));
end;

{ TCardData }

function TCardData.toJSON(): string;
var
  str: string;
begin
  str := '{';
  str += '"id": %d,'.Format([Fid]);
  str += '"name": "%s",'.Format([StrToJSONEncoded(Fname)]);
  str += '"desc":"%s",'.Format([StrToJSONEncoded(Fdesc)]);
  str += '"type": %d,'.Format([Ftype]);
  str += '"atk": %d,'.Format([Fatk]);
  str += '"def": %d,'.Format([Fdef]);
  str += '"level": %d,'.Format([Flevel]);
  str += '"race": %d,'.Format([Frace]);
  str += '"attribute": %d,'.Format([Fattribute]);
  str += '"setid": "%s"'.Format([StrToJSONEncoded(Fsetid)]);
  str += '}';
  Exit(str);
end;

{ TListCardDataHelper }

procedure TListCardDataHelper.FreeList();
var
  i: Integer;
begin
  for i:= 0 to self.Count - 1 do begin
    self.Items[i].Free;
  end;
  self.Free;
end;

function TListCardDataHelper.toJSON(): string;
var
  str: string;
  i: Integer;
begin
  str := '[';
  for i := 0 to Self.Count - 1 do begin
    str += self.Items[i].toJSON() + ',';
  end;
  str := str.TrimRight([',']);
  str += ']';
  Exit(str);
end;

{ TRespData }

constructor TRespData.Create(ACode: Integer; AMessage: string);
begin
  Fcode := ACode;
  Fmessage := AMessage;
end;

function TRespData.toJSON(): string;
begin
  Exit('{"code":%d, "message":"%s"}'.Format([Fcode, StrToJSONEncoded(Fmessage)]));
end;

end.

