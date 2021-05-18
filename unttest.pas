unit untTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untDataObj, RegExpr, LazUTF8, untStringExtension, untMySQL;

procedure testGetOneCard();
procedure testSearchCard();
procedure testJsonArray();
procedure testRegexp();
procedure testUTF8();
procedure testEffectCardName();

implementation


procedure testGetOneCard();
var
  cardinfo: TCardData;
begin
  cardinfo := doGetOneCard(43228023, 'jp');
  if (cardinfo <> nil) then begin
    WriteLn(cardinfo.toJSON());
    cardinfo.Free;
  end else begin
    WriteLn('testGetOneCard: nil');
  end;
end;

procedure testSearchCard();
var
  list: TListCardData;
begin
  list := doSearchCardData('青眼', $1, 0, 0, 0, $2000,0,'jp');
  if (list <> nil) then begin
    WriteLn(list.toJSON());
    list.freeList();
  end else begin
    WriteLn('testSearchCard: nil');
  end;
end;

procedure testJsonArray();
var
  alist: TStringList;
  list: TListCardNameData;
begin
  alist := TStringList.Create;
  alist.Add('2129638');
  alist.Add('9433350');
  alist.Add('20654247');

  list := doYdkGetNamesByIds(alist, 'jp');
  if (list <> nil) then begin
    WriteLn(list.toJSON());
    list.Free;
  end else begin
    WriteLn('testJsonArray: nil');
  end;

  alist.Free;
end;

type

  { TRegReplace }

  TRegReplace = class
  public
    function Replace(r: TRegExpr): RegExprString;
  end;

procedure testRegexp();
const
  STR1 = '[青眼の白龍(ブルーアイズ・ホワイト・ドラゴン)]';
  STR2 = '[王(おう)]の試練';
  STR3 = '[虹(にじ)]彩の[魔(ま)][術(じゅつ)][師(し)]';


var
  r: TRegExpr;
  ro: TRegReplace;
  str: string;
begin
  ro := TRegReplace.Create;
  r := TRegExpr.Create('\[.*?\(.*?\)]');
  str := r.Replace(STR1, @ro.Replace);
  WriteLn(str);

  str := r.Replace(STR2, @ro.Replace);
  WriteLn(str);

  str := r.Replace(STR3, @ro.Replace);
  WriteLn(str);
  ro.Free;

  r.Free;
end;


function AnsiToUnicode(Str: ansistring): string;
var
  s: ansistring;
  i:integer;
  j,k:string[2];
  a:array [1..1000] of  ansichar;
begin
  s:='';
  StringToWideChar(Str,@(a[1]),500);
  i:=1;
  while ((a[i]<>#0) or (a[i+1]<>#0)) do
  begin
    j:=IntToHex(Integer(a[i]),2);
    k:=IntToHex(Integer(a[i+1]),2);
    s:=s+'\u'+k+j;
    i:=i+2;
  end;
  Result:=s;
end;

procedure testUTF8();
const
  STR = 'ブルーアイズ・ホワイト・ドラゴン';
  STR4 = '１２３４';
var
  s: string = '·';
  l1, l2: Integer;
  i: Integer;
  str2: string = '';
  w: WideString;
  wi: Integer;
begin
  WriteLn(STR);
  l1 := Length(STR);
  l2 := UTF8Length(STR);

  for i := 1 to l2 do begin
    s := UTF8Copy(STR, i, 1);
    w := widestring(s);
    wi := Integer(w[1]);
    WriteLn(wi);
    str2 += s;
  end;
  WriteLn(str2);
  WriteLn('l1 = %d, l2 = %d'.Format([l1, l2]));
    WriteLn(numberToHalf(STR4));
end;

procedure testEffectCardName();
var
  // eff: string = '①：このカードが「[剣闘獣(グラディアルビースト)]」モンスターの[効(こう)][果(か)]で[特(とく)][殊(しゅ)][召(しょう)][喚(かん)]に[成(せい)][功(こう)]した[場(ば)][合(あい)]、[手(て)][札(ふだ)]から「[剣闘獣(グラディアルビースト)]」カード１[枚(まい)]を[捨(す)]てて[発(はつ)][動(どう)]できる。[自(じ)][分(ぶん)]はデッキから２[枚(まい)]ドローする。②：このカードが[戦(せん)][闘(とう)]を[行(おこな)]ったバトルフェイズ[終(しゅう)][了(りょう)][時(じ)]にこのカードを[持(も)]ち[主(ぬし)]のデッキに[戻(もど)]して[発(はつ)][動(どう)]できる。デッキから「[剣闘獣(グラディアルビースト)]サジタリィ」[以(い)][外(がい)]の「[剣闘獣(グラディアルビースト)]」モンスター１[体(たい)]を[特(とく)][殊(しゅ)][召(しょう)][喚(かん)]する。';
  // eff: string = '①：自分のメインモンスターゾーンにモンスターが存在しない場合に発動できる。自分フィールドに「閃刀姫トークン」（戦士族・闇・星１・攻／守０）１体を守備表示で特殊召喚する。このトークンはリリースできない。自分の墓地に魔法カードが３枚以上存在する場合、そのトークンの攻撃力・守備力は１５００になる。';
  eff: string = '「救世竜 セイヴァー・ドラゴン」＋ドラゴン族Ｓモンスターを含むチューナー以外のモンスター１体以上'#10'このカードはＳ召喚でのみＥＸデッキから特殊召喚できる。①：１ターンに１度、発動できる。相手フィールドの効果モンスター１体を選び、その効果を無効にする。②：このカードは通常の攻撃に加えて、自分の墓地の「スターダスト・ドラゴン」及びそのカード名が記されたＳモンスターの数まで攻撃できる。③：１ターンに１度、相手が効果を発動した時に発動できる。このカードをエンドフェイズまで除外し、その発動を無効にし除外する。';
  aname: string;
begin
  aname:= removeKana(eff);
  WriteLn(aname);
  aname:= getEffectKanjiKana(aname);
  WriteLn(aname);
end;

{ TRegReplace }

function TRegReplace.Replace(r: TRegExpr): RegExprString;
var
  s: string;
begin
  // s.value.replace("[", "").run { substring(0, indexOf("(")) }
  s := r.Match[0];
  s := s.Replace('[', '', [rfIgnoreCase, rfReplaceAll]);
  s := s.Substring(0, s.IndexOf('('));
  Exit(s);
end;


end.

