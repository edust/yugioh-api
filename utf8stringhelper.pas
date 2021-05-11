unit UTF8StringHelper;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils, RegExpr, LazUTF8;

type

  { TUTF8CharHelper }

  TUTF8CharHelper = type Helper for WideChar
  private
    function GetAnsi: string;
    function GetUCode: Integer;
  public
    class function UFromCode(ACode: Integer): WideChar; static;
  public
    property UCode: Integer read GetUCode;
    property UAnsi: string read GetAnsi;
  end;

  TWideCharArray = array of WideChar;

  TStringArrayFilterCallable = Function(item: string): Boolean;
  TStringArrayMapCharCallable = Function(item: string): WideChar;
  TStringArrayJoinCallable = Function(item: string): string;

  { TUTF8StringArrayHelper }

  TUTF8StringArrayHelper = type Helper for TStringArray
  public
    function Filter(call: TStringArrayFilterCallable): TStringArray;
    function Join(const Separator: string; call: TStringArrayJoinCallable = nil): string;
    function MapToChar(call: TStringArrayMapCharCallable): TWideCharArray;
  end;

  TUTF8StringReplaceCallable = Function (AValue: String): String;

  { TUTF8StringHelper }

  TUTF8StringHelper =  type Helper for String
  private
    function GetUChar(Index: Integer): WideChar;
    function GetULength: Integer;
  public
    const Empty = '';

    (*
    Class Function Compare(const A: string; const B: string): Integer; overload; static; //inline;
    Class Function Compare(const A: string; const B: string; IgnoreCase: Boolean): Integer; overload; static; //inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: string; const B: string; Options: TCompareOptions): Integer; overload; static; // inline;
    Class Function Compare(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static; // inline;
    Class Function Compare(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt; IgnoreCase: Boolean): Integer; overload; static; // inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt; Options: TCompareOptions): Integer; overload; static;//  inline;
    Class Function CompareOrdinal(const A: string; const B: string): Integer; overload; static;
    Class Function CompareOrdinal(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static;
    Class Function CompareText(const A: string; const B: string): Integer; static; inline;
    Class Function Copy(const Str: string): string; inline; static;
    Class Function Create(AChar: Char; ACount: SizeInt): string; overload; inline; static;
    Class Function Create(const AValue: array of Char): string; overload; static;
    Class Function Create(const AValue: array of Char; StartIndex: SizeInt; ALen: SizeInt): string; overload; static;
    Class Function EndsText(const ASubText, AText: string): Boolean; static;
    Class Function Equals(const a: string; const b: string): Boolean; overload; static;
    Class Function Format(const AFormat: string; const args: array of const): string; overload; static;
    Class Function IsNullOrEmpty(const AValue: string): Boolean; static;
    Class Function IsNullOrWhiteSpace(const AValue: string): Boolean; static;
    Class Function Join(const Separator: string; const Values: array of const): string; overload; static;
    Class Function Join(const Separator: string; const Values: array of string): string; overload; static;
    Class Function Join(const Separator: string; const Values: array of string; StartIndex: SizeInt; ACount: SizeInt): string; overload; static;
    Class Function LowerCase(const S: string): string; overload; static; inline;
    Class Function Parse(const AValue: Boolean): string; overload; static; inline;
    Class Function Parse(const AValue: Extended): string; overload; static;inline;
    Class Function Parse(const AValue: Int64): string; overload; static; inline;
    Class Function Parse(const AValue: Integer): string; overload; static; inline;
    Class Function ToBoolean(const S: string): Boolean; overload; static; inline;
    Class Function ToDouble(const S: string): Double; overload; static; inline;
    Class Function ToExtended(const S: string): Extended; overload; static; inline;
    Class Function ToInt64(const S: string): Int64; overload; static; inline;
    Class Function ToInteger(const S: string): Integer; overload; static; inline;
    Class Function ToSingle(const S: string): Single; overload; static; inline;
    Class Function UpperCase(const S: string): string; overload; static; inline;
    Function CompareTo(const B: string): Integer;

    procedure CopyTo(SourceIndex: SizeInt; var destination: array of Char; DestinationIndex: SizeInt; ACount: SizeInt);
    Function CountChar(const C: Char): SizeInt;
    Function DeQuotedString: string; overload;
    Function DeQuotedString(const AQuoteChar: Char): string; overload;
    Function EndsWith(const AValue: string): Boolean; overload; inline;
    Function EndsWith(const AValue: string; IgnoreCase: Boolean): Boolean; overload;
    Function Equals(const AValue: string): Boolean; overload;
    Function Format(const args: array of const): string; overload;
    Function GetHashCode: Integer;
    Function IndexOfAny(const AnyOf: array of Char): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of Char; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String; StartIndex: SizeInt; ACount: SizeInt; Out AMatch : SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of string; StartQuote, EndQuote: Char; StartIndex: SizeInt; Out Matched: SizeInt): SizeInt; overload;
    Function Insert(StartIndex: SizeInt; const AValue: string): string;
    Function IsDelimiter(const Delimiters: string; Index: SizeInt): Boolean;
    Function IsEmpty: Boolean;
    Function LastDelimiter(const Delims: string): SizeInt;
    Function LastIndexOf(AValue: Char): SizeInt; overload;
    Function LastIndexOf(const AValue: string): SizeInt; overload;
    Function LastIndexOf(AValue: Char; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: string; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(AValue: Char; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: string; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of Char): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of Char; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of Char; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function PadLeft(ATotalWidth: SizeInt): string; overload; inline;
    Function PadLeft(ATotalWidth: SizeInt; PaddingChar: Char): string; overload; inline;
    Function PadRight(ATotalWidth: SizeInt): string; overload; inline;
    Function PadRight(ATotalWidth: SizeInt; PaddingChar: Char): string; overload; inline;
    Function QuotedString: string; overload;
    Function QuotedString(const AQuoteChar: Char): string; overload;
    Function Remove(StartIndex: SizeInt): string; overload; inline;
    Function Remove(StartIndex: SizeInt; ACount: SizeInt): string; overload; inline;

    Function StartsWith(const AValue: string): Boolean; overload; inline;
    Function StartsWith(const AValue: string; IgnoreCase: Boolean): Boolean; overload;

    Function ToBoolean: Boolean; overload; inline;
    Function ToInteger: Integer; overload; inline;
    Function ToInt64: Int64; overload; inline;
    Function ToSingle: Single; overload; inline;
    Function ToDouble: Double; overload; inline;
    Function ToExtended: Extended; overload; inline;
    Function ToCharArray: TCharArray; overload;
    Function ToCharArray(AStartIndex: SizeInt; ALen: SizeInt): TCharArray; overload;
    Function ToLower: string; overload; inline;
    Function ToLowerInvariant: string;
    Function ToUpper: string; overload; inline;
    Function ToUpperInvariant: string; inline;
    Function Trim: string; overload;
    Function TrimLeft: string; overload;
    Function TrimRight: string; overload;
    Function Trim(const ATrimChars: array of Char): string; overload;
    Function TrimLeft(const ATrimChars: array of Char): string; overload;
    Function TrimRight(const ATrimChars: array of Char): string; overload;
    *)
    Function UContains(const AValue: string): Boolean;
    Function UIndexOf(AValue: WideChar): Integer; overload; inline;
    Function UIndexOf(const AValue: string): Integer; overload; inline;
    Function UIndexOf(AValue: WideChar; StartIndex: Integer): Integer; overload;
    Function UIndexOf(const AValue: string; StartIndex: Integer): Integer; overload;
    Function UIndexOfAny(const AnyOf: array of WideChar): Integer; overload;
    Function UIndexOfAny(const AnyOf: array of WideChar; StartIndex: Integer): Integer; overload;
    Function UIndexOfAny(const AnyOf: array of String): Integer; overload;
    Function UIndexOfAny(const AnyOf: array of String; StartIndex: Integer): Integer; overload;
    Function UReplace(const OldValue: string; const NewValue: string): string; overload;
    Function UReplace(const OldValue: string; const NewValue: string; ReplaceFlags: TReplaceFlags): string; overload;
    Function UReplaceRegex(const ARegEx: string; Replacable: TUTF8StringReplaceCallable): string; overload;
    Function UReplaceRegex(const ARegEx: TRegExpr; Replacable: TUTF8StringReplaceCallable): string; overload;
    Function USubstring(AStartIndex: Integer): string; overload;
    Function USubstringLen(AStartIndex: Integer; ALen: Integer): string;
    Function USubstring(AStartIndex: Integer; AEndIndex: Integer): string; overload;
    Function USplit(const Separators: array of WideChar): TStringArray; overload;
    Function USplit(const Separators: TStringArray): TStringArray; overload;
  public
    property ULength: Integer read GetULength;
    property UChar[Index: Integer]: WideChar read GetUChar;
  end;

implementation

{ TUTF8StringArrayHelper }

function TUTF8StringArrayHelper.Filter(call: TStringArrayFilterCallable
  ): TStringArray;
var
  AList: TStringList;
  i: Integer;
begin
  AList := TStringList.Create;
  for i := 0 to Length(Self) - 1 do begin
    if (call(Self[i])) then begin
      AList.Add(Self[i]);
    end;
  end;
  Result := AList.ToStringArray;
  AList.Free;
end;

function TUTF8StringArrayHelper.Join(const Separator: string;
  call: TStringArrayJoinCallable): string;
var
  ret: string = '';
  i: Integer;
begin
  for i := 0 to Length(Self) - 1 do begin
    if (i = 0) then begin
      if (call <> nil) then begin
        ret += call(Self[0]);
      end else begin
        ret += Self[0];
      end;

    end else begin
      if (call <> nil) then begin
        ret += Separator + call(Self[i]);
      end else begin
        ret += Separator + Self[i];
      end;
    end;
  end;
  Exit(ret);
end;

function TUTF8StringArrayHelper.MapToChar(call: TStringArrayMapCharCallable
  ): TWideCharArray;
var
  len: Integer;
  i: Integer;
begin
  Result := nil;
  len := Length(Self);
  SetLength(Result, len);
  for i := 0 to len - 1 do begin
    Result[i] := call(Self[i]);
  end;
end;

{ TUTF8CharHelper }

function TUTF8CharHelper.GetAnsi: string;
begin
  Exit(String(Self));
end;

function TUTF8CharHelper.GetUCode: Integer;
begin
  Exit(Integer(Self));
end;

class function TUTF8CharHelper.UFromCode(ACode: Integer): WideChar;
begin
  Exit(WideChar(ACode));
end;

{ TUTF8StringHelper }

function TUTF8StringHelper.GetUChar(Index: Integer): WideChar;
begin
  Exit(WideString(UTF8Copy(Self, Index, 1))[1]);
end;

function TUTF8StringHelper.GetULength: Integer;
begin
  Exit(UTF8Length(Self));
end;

function TUTF8StringHelper.UContains(const AValue: string): Boolean;
begin
  Exit(UIndexOf(AValue) <> -1);
end;

function TUTF8StringHelper.UIndexOf(AValue: WideChar): Integer;
var
  idx: Integer = -1;
  i: Integer;
  c: WideChar;
begin
  for i := 1 to Self.ULength do begin
    c := Self.UChar[i];
    if (c = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOf(const AValue: string): Integer;
var
  s: string;
  idx: Integer = -1;
  i: Integer;
  le: Integer;
begin
  le := AValue.ULength;
  for i := 1 to Self.ULength do begin
    s := UTF8Copy(Self, i, le);
    if (s = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOf(AValue: WideChar; StartIndex: Integer
  ): Integer;
var
  idx: Integer = -1;
  i: Integer;
  c: WideChar;
begin
  for i := StartIndex to Self.ULength do begin
    c := Self.UChar[i];
    if (c = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOf(const AValue: string; StartIndex: Integer
  ): Integer;
var
  s: string;
  idx: Integer = -1;
  i: Integer;
  le: Integer;
begin
  le := AValue.ULength;
  for i := StartIndex to Self.ULength do begin
    s := UTF8Copy(Self, i, le);
    if (s = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function InWideCharArray(c: WideChar; AnyOf: array of WideChar): Boolean;
var
  i: Integer;
  ret: Boolean = False;
begin
  for i := 0 to Length(AnyOf) - 1 do begin
    if (c = AnyOf[i]) then begin
      ret := True;
      Break;
    end;
  end;
  Exit(ret);
end;

function TUTF8StringHelper.UIndexOfAny(const AnyOf: array of WideChar): Integer;
var
  i: Integer;
  idx: Integer = -1;
  c: WideChar;
begin
  for i := 1 to Self.ULength do begin
    c := Self.UChar[i];
    if (InWideCharArray(c, AnyOf)) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOfAny(const AnyOf: array of WideChar;
  StartIndex: Integer): Integer;
var
  i: Integer;
  idx: Integer = -1;
  c: WideChar;
begin
  for i := StartIndex to Self.ULength do begin
    c := Self.UChar[i];
    if (InWideCharArray(c, AnyOf)) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOfAny(const AnyOf: array of String): Integer;
var
  le: Integer;
  i, j: Integer;
  sj: string;
  si: string;
  idx: Integer = -1;
begin
  for i := 1 to Self.ULength do begin
    for j := 0 to Length(AnyOf) - 1 do begin
      sj := AnyOf[j];
      le := sj.ULength;
      si := UTF8Copy(Self, i, le);
      if (si = sj) then begin
        idx := i;
        Break;
      end;
    end;
    if (idx <> -1) then Break;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOfAny(const AnyOf: array of String;
  StartIndex: Integer): Integer;
var
  le: Integer;
  i, j: Integer;
  sj: string;
  si: string;
  idx: Integer = -1;
begin
  for i := StartIndex to Self.ULength do begin
    for j := 0 to Length(AnyOf) - 1 do begin
      sj := AnyOf[j];
      le := sj.ULength;
      si := UTF8Copy(Self, i, le);
      if (si = sj) then begin
        idx := i;
        Break;
      end;
    end;
    if (idx <> -1) then Break;
  end;
  Exit(idx);

end;

function TUTF8StringHelper.UReplace(const OldValue: string;
  const NewValue: string): string;
begin
  Exit(UReplace(OldValue, NewValue, [rfReplaceAll]));
end;

function TUTF8StringHelper.UReplace(const OldValue: string;
  const NewValue: string; ReplaceFlags: TReplaceFlags): string;
begin
  Exit(StringReplace(Self, OldValue, NewValue, ReplaceFlags));
end;

type

  { TRegReplacable }

  TRegReplacable = class
  private
    FCallable: TUTF8StringReplaceCallable;
  public
    constructor Create(ACall: TUTF8StringReplaceCallable);
    function doReplace(r: TRegExpr): RegExprString;
  end;

{ TRegReplacable }

constructor TRegReplacable.Create(ACall: TUTF8StringReplaceCallable);
begin
  FCallable:= ACall;
end;

function TRegReplacable.doReplace(r: TRegExpr): RegExprString;
begin
  Exit(FCallable(r.Match[0]));
end;

function TUTF8StringHelper.UReplaceRegex(const ARegEx: string;
  Replacable: TUTF8StringReplaceCallable): string;
var
  reg: TRegExpr;
  ro: TRegReplacable;
  ret: string;
begin
  reg:= TRegExpr.Create(ARegEx);
  ro := TRegReplacable.Create(Replacable);
  ret := reg.Replace(Self, @ro.doReplace);
  ro.Free;
  reg.Free;
  Exit(ret);
end;

function TUTF8StringHelper.UReplaceRegex(const ARegEx: TRegExpr;
  Replacable: TUTF8StringReplaceCallable): string;
var
  ro: TRegReplacable;
  ret: string;
begin
  ro := TRegReplacable.Create(Replacable);
  ret := ARegEx.Replace(Self, @ro.doReplace);
  ro.Free;
  Exit(ret);
end;

function TUTF8StringHelper.USubstring(AStartIndex: Integer): string;
begin
  Exit(USubstringLen(AStartIndex, Self.ULength - AStartIndex));
end;

function TUTF8StringHelper.USubstringLen(AStartIndex: Integer; ALen: Integer
  ): string;
begin
  Exit(UTF8Copy(Self, AStartIndex, ALen));
end;

function TUTF8StringHelper.USubstring(AStartIndex: Integer; AEndIndex: Integer
  ): string;
begin
  Exit(USubstringLen(AStartIndex, AEndIndex - AStartIndex));
end;

function TUTF8StringHelper.USplit(const Separators: array of WideChar
  ): TStringArray;
var
  alist: TStringList;
  tmp: string;
  start: Integer;
begin
  alist := TStringList.Create;
  tmp := Self;
  while True do begin
    start:= tmp.UIndexOfAny(Separators);
    if (start = -1) then begin
      alist.Add(tmp);
      Break;
    end;
    alist.Add(tmp.USubstring(1, start));
    tmp := tmp.USubstring(start + 1);
  end;
  Result := alist.ToStringArray;
  alist.Free;
end;

function MapToCharCallable(item: string): WideChar;
begin
  Exit(WideString(item)[1]);
end;

function TUTF8StringHelper.USplit(const Separators: TStringArray): TStringArray;
var
  arr: TWideCharArray;
begin
  arr := Separators.MapToChar(@MapToCharCallable);
  Exit(USplit(arr));
end;


end.

