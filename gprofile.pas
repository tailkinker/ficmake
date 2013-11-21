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
      t_output : string;
      t_preconv : boolean;
      t_separator : string;
    public
      class function ProfileType : integer; virtual;
      property Name : string read t_name write t_name;
      property OutputDir : string read t_output write t_output;
      property UsePreconv : boolean read t_preconv write t_preconv;
      property Separator : string read t_separator write t_separator;
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
      t_H1Mode : byte;
      t_headers : array [0..3] of tGroffHeader;
      t_OneColumnTitlePage : boolean;
      t_flags : array [0..3] of boolean;
      procedure SetMargin (index : integer; value : longint);
      function GetMargin (index : integer) : longint;
      procedure SetHeader (index : integer; header : tGroffHeader);
      function GetHeader (index : integer) : tGroffHeader;
      procedure SetFlag (index : integer; aflag : boolean);
      function GetFlag (index : integer) : boolean;
    public
      class function ProfileType : integer; override;
      property GroffFonts : tGroffFontArray read t_fonts write t_fonts;
      property PageSize : byte read t_pagesize write t_pagesize;
      property PageH : longint read t_pageh write t_pageh;
      property PageV : longint read t_pagev write t_pagev;
      property Columns : byte read t_columns write t_columns;
      property OuterMargin  : longint index 0 read GetMargin write SetMargin;
      property InnerMargin  : longint index 1 read GetMargin write SetMargin;
      property TopMargin    : longint index 2 read GetMargin write SetMargin;
      property BottomMargin : longint index 3 read GetMargin write SetMargin;
      property H1Mode : byte read t_H1Mode write t_H1Mode;
      property OddHeader  : tGroffHeader index 0 read GetHeader write SetHeader;
      property EvenHeader : tGroffHeader index 1 read GetHeader write SetHeader;
      property OddFooter  : tGroffHeader index 2 read GetHeader write SetHeader;
      property EvenFooter : tGroffHeader index 3 read GetHeader write SetHeader;
      property OneColumnTitlePage : boolean index 0 read SetFlag write GetFlag;
      property Landscape          : boolean index 1 read SetFlag write GetFlag;
      property UseTBL             : boolean index 2 read GetFlag write SetFlag;
      property UseEQN             : boolean index 3 read GetFlag write SetFlag;
  end;

  tHTMLProfile = class (tBaseProfile)
    private
      t_flags : array [0..5] of boolean;
      t_strings : array [0..1] of string;
      procedure SetFlag (index : integer; aflag : boolean);
      function GetFlag (index : integer) : boolean;
      procedure SetString (index : integer; astring : string);
      function GetString (index : integer) : string;
    public
      class function ProfileType : integer; override;
      property BulkHTML            : boolean index 0 read GetFlag write SetFlag;
      property DisclaimerInIndex   : boolean index 1 read GetFlag write SetFlag;
      property DisclaimerInChapter : boolean index 2 read GetFlag write SetFlag;
      property BookFilesInIndex    : boolean index 3 read GetFlag write SetFlag;
      property SingleLineBooks     : boolean index 4 read GetFlag write SetFlag;
      property SeparateBookFiles   : boolean index 5 read GetFlag write SetFlag;
      property BackLink     : string index 0 read GetString write SetString;
      property BackLinkText : string index 1 read GetString write SetString;
  end;

  tTextProfile = class (tBaseProfile)
    private
      t_flags : array [0..2] of boolean;
      procedure SetFlag (index : integer; aflag : boolean);
      function GetFlag (index : integer) : boolean;
    public
      class function ProfileType : integer; override;
      property BulkText      : boolean index 0 read GetFlag write SetFlag;
      property FFMLCompliant : boolean index 1 read GetFlag write SetFlag;
      property UseTBL        : boolean index 2 read GetFlag write SetFlag;
  end;

  tEPubProfile = class (tBaseProfile)
    private
      t_blurb : boolean;
      t_series : string;
    public
      class function ProfileType : integer; override;
      property BlurbInEPub : boolean read t_blurb write t_blurb;
      property EPubSeries : string read t_series write t_series;
  end;

const
  ptBase = 0;
  ptPDF = 1;
  ptHTML = 2;
  ptText = 3;
  ptEPub = 4;

implementation

{$region tBaseProfile}


class function tBaseProfile.ProfileType : integer;
begin
  ProfileType := 0;
end;


{$endregion}

{$region tPDFProfile}

class function tPDFProfile.ProfileType : integer;
begin
  ProfileType := ptPDF;
end;

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

procedure tPDFProfile.SetFlag (index : integer; aflag : boolean);
begin
  t_flags [index] := aflag;
end;

function tPDFProfile.GetFlag (index : integer) : boolean;
begin
	GetFlag = t_flags [index];
end;



{$endregion}

{$region tHTMLProfile}

class function tHTMLProfile.ProfileType : integer;
begin
  ProfileType := ptHTML;
end;

procedure tHTMLProfile.SetFlag (index : integer; aflag : boolean);
begin
  t_flags [index] := aflag;
end;

function tHTMLProfile.GetFlag (index : integer) : boolean;
begin
	GetFlag = t_flags [index];
end;

procedure tHTMLProfile.SetString (index : integer; astring : string);
begin
	t_strings [index] := aString;
end;

function tHTMLProfile.GetString (index : integer) : string;
begin
	GetString := t_strings [index];
end;

{$endregion}

{$region tTextProfile}

class function tTextProfile.ProfileType : integer;
begin
  ProfileType := ptText;
end;

procedure tTextProfile.SetFlag (index : integer; aflag : boolean);
begin
  t_flags [index] := aflag;
end;

function tTextProfile.GetFlag (index : integer) : boolean;
begin
	GetFlag = t_flags [index];
end;

{$endregion}

{$region tEPubProfile}

class function tEPubProfile.ProfileType : integer;
begin
  ProfileType := ptEPub;
end;

{$endregion}
end.

