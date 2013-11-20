{
Copyright 2013 Timothy Groves <timothy.red.groves@gmail.com>

This file is part of FicMake.

FicMake is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

FicMake is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with FicMake.  If not, see <http://www.gnu.org/licenses/>.
}

unit gprofile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  tGroffFont = record
    Family,
    Font,
    Size : byte;
    Centered : boolean;
    ReserveSpace,
    SpaceAbove,
    SpaceBelow : longint;
    Borders : array [0..3] of boolean;
  end;

  tGroffFontArray = array [0..11] of tGroffFont;

  tGroffHeader = record
    Enabled : boolean;
    Left,
    Middle,
    Right : string;
  end;

  tBaseProfile = class (tObject)
    private
      t_name : string;
      ProfileType : byte;
      t_output : string;
      t_preconv : boolean;
    public
      property Name : string read t_name write t_name;
      property OutputDir : string read t_output write t_output;
      property UsePreconv : boolean read t_preconv write t_preconv;
      constructor Create;
  end;

  tPDFProfile = class (tBaseProfile)
    private
      t_fonts : tGroffFontArray;
      t_pagesize : byte;
      t_PageH,
      t_PageV : longint;
      t_landscape : boolean;
      t_columns : byte;
      t_margins : array [0..3] of longint;
      t_separator : string;
      t_H1Mode : byte;
      t_headers : array [0..3] of tGroffHeader;
      t_OneColumnTitlePage : boolean;
      procedure SetMargin (index : integer; value : longint);
      function GetMargin (index : integer) : longint;
      procedure SetHeader (index : integer; header : tGroffHeader);
      function GetHeader (index : integer) : tGroffHeader;
    public
      property GroffFonts : tGroffFontArray read t_fonts write t_fonts;
      property PageSize : byte read t_pagesize write t_pagesize;
      property PageH : longint read t_pageh write t_pageh;
      property PageV : longint read t_pagev write t_pagev;
      property Landscape : boolean read t_landscape write t_landscape;
      property Columns : byte read t_columns write t_columns;
      property OuterMargin  : longint index 0 read GetMargin write SetMargin;
      property InnerMargin  : longint index 1 read GetMargin write SetMargin;
      property TopMargin    : longint index 2 read GetMargin write SetMargin;
      property BottomMargin : longint index 3 read GetMargin write SetMargin;
      property Separator : string read t_separator write t_separator;
      property H1Mode : byte read t_H1Mode write t_H1Mode;
      property OddHeader  : tGroffHeader index 0 read GetHeader write SetHeader;
      property EvenHeader : tGroffHeader index 1 read GetHeader write SetHeader;
      property OddFooter  : tGroffHeader index 2 read GetHeader write SetHeader;
      property EvenFooter : tGroffHeader index 3 read GetHeader write SetHeader;
      property OneColumnTitlePage : boolean;
  end;

implementation

{$region tBaseProfile}

constructor tBaseProfile.Create;
begin

end;

{$endregion}

{$region tPDFProfile}

procedure tPDFProfile.SetMargin (index : integer; value : longint);
begin
  t_margins [index] := value;
end;

function tPDFProfile.GetMargin (index : integer) : longint;
begin
  GetMargin := t_margins [index];
end;

procedure tPDFProfile.SetHeader (index : integer; header : tGroffHeader);
begin
  t_headers [index] := header;
end;

function tPDFProfile.GetHeader (index : integer) : tGroffHeader;
begin
  GetHeader := t_headers [index];
end;


{$endregion}
end.

