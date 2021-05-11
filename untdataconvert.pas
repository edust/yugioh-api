unit untDataConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function StrToCardType(astr: string): Integer;
function StrToAttribute(astr: string): Integer;
function StrToIcon(astr: string): Integer;
function StrToSubType(astr: string): Integer;
function StrToRace(astr: string): Integer;
function StrToMonsterType(astr: string): Integer;

implementation

function StrToCardType(astr: string): Integer;
var
  t: Integer = 0;
begin
  case astr of
    'pendulum': t := $1000000;
    'trap': t := $4;
    'spell': t := $2;
  end;
  Exit(t);
end;

function StrToAttribute(astr: string): Integer;
var
  a: Integer = 0;
begin
  case astr of
    'divine': a := $40;
    'dark': a := $20;
    'light': a := $10;
    'wind': a := $8;
    'fire': a := $4;
    'water': a := $2;
    'earch': a := $1;
  end;
  Exit(a);
end;

function StrToIcon(astr: string): Integer;
var
  i: Integer = 0;
begin
  case astr of
    'counter': i := $100000;
    'field': i := $80000;
    'equip': i := $40000;
    'continuous': i := $20000;
    'quick-play': i := $10000;
    'ritual': i := $80;
  end;
  Exit(i);
end;

function StrToSubType(astr: string): Integer;
var
  s: Integer = 0;
begin
  case astr of
    'spsummon': s := $2000000;
    'link': s := $4000000;
    'pendulum': s := $1000000;
    'xyz': s := $800000;
    'synchro': s := $2000;
    'ritual': s := $80;
    'fusion': s := $40;
    'toon': s := $400000;
    'flip': s := $200000;
    'tuner': s := $1000;
    'gemini': s := $800;
    'union': s := $400;
    'spirit': s := $200;
    'effect': s := $20;
    'normal': s := $10;
  end;
  Exit(s);
end;

function StrToRace(astr: string): Integer;
var
  r: Integer = 0;
begin
  case astr of
    'cyberse': r := $1000000;
    'wyrm': r := $800000;
    'creatorGod': r := $400000;
    'divineBeast': r := $200000;
    'psychic': r := $100000;
    'reptile': r := $80000;
    'seaSerpent': r := $40000;
    'fish': r := $20000;
    'dinosaur': r := $10000;
    'beastWarrior': r := $8000;
    'beast': r := $4000;
    'dragon': r := $2000;
    'thunder': r := $1000;
    'insect': r := $800;
    'plant': r := $400;
    'wingedBeast': r := $200;
    'rock': r := $100;
    'pyro': r := $80;
    'aqua': r := $40;
    'machine': r := $20;
    'zombie': r := $10;
    'fiend': r := $8;
    'fairy': r := $4;
    'spellcaster': r := $2;
    'warrior': r := $1;
  end;
  Exit(r);
end;

function StrToMonsterType(astr: string): Integer;
var
  m: Integer = 0;
begin
  case astr of
    'link': m := $4000000;
    'xyz': m := $800000;
    'token': m := $4000;
    'synchro': m := $2000;
    'ritual': m := $80;
    'fusion': m := $40;
    'effect': m := $20;
    'normal': m := $10;
  end;
  Exit(m);
end;

end.

