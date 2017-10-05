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
{$region interface}

uses
  Classes, SysUtils,
  gstory, gchapter;

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


  {
  tGroffFont = class (tObject)
    private
      t_ffs : array [0..2] of byte;
      t_bools : array [0..4] of boolean;
      t_spaces : array [0..2] of longint;
      function GetByte (index : integer) : byte;
      procedure SetByte (a : byte; index : integer);
      function GetBool (index : integer) : boolean;
      procedure SetBool (a : boolean; index : integer);
      function GetInt (index : integer) : longint;
      procedure SetInt (a : longint; index : integer);
    public
      property Family : byte index 0 read GetByte write SetByte;
      property Font : byte index 1 read GetByte write SetByte;
      property Size : byte index 2 read GetByte write SetByte;
      property Centered : boolean index 0 read GetBoool write SetBool;
      property ReserveSpace : longint index 0 read GetInt write SetInt;
      property SpaceAbove : longint index 1 read GetInt write SetInt;
      property SpaceBelow : longint index 2 read GetInt write SetInt;
      property TopBorder : boolean index 1 read GetBool write SetBool;
      property BottomBorder : boolean index 2 read GetBool write SetBool;
      property LeftBorder : boolean index 3 read GetBool write SetBool;
      property RightBorder : boolean index 4 read GetBool write SetBool;
  end;
}
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
      t_hdtbl : boolean;
      t_separator : string;
    public
      class function ProfileType : integer; virtual;
      property Name : string read t_name write t_name;
      property OutputDir : string read t_output write t_output;
      property UsePreconv : boolean read t_preconv write t_preconv;
      property UseHDTBL : boolean read t_hdtbl write t_hdtbl;
      property Separator : string read t_separator write t_separator;
      constructor Create; virtual;
      procedure Load (var t : text); virtual;
      procedure Save (var t : text); virtual;
      procedure Edit; virtual;
      function Duplicate : tBaseProfile; virtual;
      procedure Build (aStory : tStory); virtual;
  end;

  tPDFProfile = class (tBaseProfile)
    private
      t_pagesize : byte;
      t_PageH,
      t_PageV : longint;
      t_columns,
      t_forcefirstpage : byte;
      t_margins : array [0..3] of longint;
      t_toccol,
      t_indcol,
      t_H1Mode : byte;
      t_flags : array [0..7] of boolean;
      procedure SetMargin (index : integer; value : longint);
      function GetMargin (index : integer) : longint;
      procedure SetFlag (index : integer; aflag : boolean);
      function GetFlag (index : integer) : boolean;
      procedure SetPageSize (aPageSize : byte);
    public
      OddHeader,
      EvenHeader,
      OddFooter,
      EvenFooter : tGroffHeader;
      GroffFonts : tGroffFontArray;
      class function ProfileType : integer; override;
      property PageSize : byte read t_pagesize write SetPageSize default 0;
      property PageH : longint read t_pageh write t_pageh default 612000;
      property PageV : longint read t_pagev write t_pagev default 792000;
      property Columns : byte read t_columns write t_columns;
      property OuterMargin  : longint index 0 read GetMargin write SetMargin;
      property InnerMargin  : longint index 1 read GetMargin write SetMargin;
      property TopMargin    : longint index 2 read GetMargin write SetMargin;
      property BottomMargin : longint index 3 read GetMargin write SetMargin;
      property ToCColumns   : byte read t_toccol write t_toccol default 1;
      property IndexColumns : byte read t_indcol write t_indcol default 1;
      property H1Mode : byte read t_H1Mode write t_H1Mode;
      property ForceFirstPage : byte read t_forcefirstpage
        write t_forcefirstpage default 0;
      property OneColumnTitlePage : boolean index 0 read GetFlag write SetFlag;
      property Landscape          : boolean index 1 read GetFlag write SetFlag;
      property UseTBL             : boolean index 2 read GetFlag write SetFlag;
      property UseEQN             : boolean index 3 read GetFlag write SetFlag;
      property CrossReference     : boolean index 4 read GetFlag write SetFlag;
      property UseIndex           : boolean index 5 read GetFlag write SetFlag;
      property UsePDFMark         : boolean index 6 read GetFlag write SetFlag;
      property SuppressHeader     : boolean index 7 read GetFlag write SetFlag;
      constructor Create; override;
      procedure Load (var t : text); override;
      procedure Save (var t : text); override;
      procedure Edit; override;
      function Duplicate : tPDFProfile; override;
      procedure Build (aStory : tStory); override;
  end;

  tHTMLProfile = class (tBaseProfile)
    private
      t_flags : array [0..6] of boolean;
      t_strings : array [0..1] of string;
      procedure SetFlag (index : integer; aflag : boolean);
      function GetFlag (index : integer) : boolean;
      procedure SetString (index : integer; astring : string);
      function GetString (index : integer) : string;
      procedure WriteBulk (aStory : tStory);
      procedure WriteChapters (aStory : tStory);
      procedure WriteIndex (aStory : tStory);
      procedure WriteTopSo (var x : text);
    public
      class function ProfileType : integer; override;
      property BulkHTML            : boolean index 0 read GetFlag write SetFlag;
      property DisclaimerInIndex   : boolean index 1 read GetFlag write SetFlag;
      property DisclaimerInChapter : boolean index 2 read GetFlag write SetFlag;
      property BookFilesInIndex    : boolean index 3 read GetFlag write SetFlag;
      property SingleLineBooks     : boolean index 4 read GetFlag write SetFlag;
      property SeparateBookFiles   : boolean index 5 read GetFlag write SetFlag;
      property IsFFNet             : boolean index 6 read GetFlag write SetFlag;
      property BackLink     : string index 0 read GetString write SetString;
      property BackLinkText : string index 1 read GetString write SetString;
      constructor Create; override;
      procedure Load (var t : text); override;
      procedure Save (var t : text); override;
      procedure Edit; override;
      function Duplicate : tHTMLProfile; override;
      procedure Build (aStory : tStory); override;
  end;

  tTextProfile = class (tBaseProfile)
    private
      t_flags : array [0..2] of boolean;
      procedure SetFlag (index : integer; aflag : boolean);
      function GetFlag (index : integer) : boolean;
      procedure WriteBulk (aStory : tStory);
      procedure WriteChapters (aStory : tStory);
    public
      class function ProfileType : integer; override;
      property BulkText      : boolean index 0 read GetFlag write SetFlag;
      property FFMLCompliant : boolean index 1 read GetFlag write SetFlag;
      property UseTBL        : boolean index 2 read GetFlag write SetFlag;
      constructor Create; override;
      procedure Load (var t : text); override;
      procedure Save (var t : text); override;
      procedure Edit; override;
      function Duplicate : tTextProfile; override;
      procedure Build (aStory : tStory); override;
  end;

  tEPubProfile = class (tBaseProfile)
    private
      t_blurb : boolean;
      t_series : string;
      procedure WriteTopSo (var x : text);
    public
      class function ProfileType : integer; override;
      property BlurbInEPub : boolean read t_blurb write t_blurb;
      property EPubSeries : string read t_series write t_series;
      constructor Create; override;
      procedure Load (var t : text); override;
      procedure Save (var t : text); override;
      procedure Edit; override;
      function Duplicate : tEPubProfile; override;
      procedure Build (aStory : tStory); override;
  end;

  tProfileList = class (tObject)
    private
      t_dirty : boolean;
      t_profiles : array of tBaseProfile;
      t_current_profile : tBaseProfile;
      t_basedir : string;
      function GetProfileCount : integer;
    public
      property Current : tBaseProfile read t_current_profile;
      property Count : integer read GetProfileCount;
      property Dirty : boolean read t_dirty;
      property BaseDir : string read t_basedir write t_basedir;
      procedure New (aProfileName : string; aProfileType : integer);
      procedure Add (aProfile : tBaseProfile);
      procedure Delete;
      procedure Select (aProfileName : string);
      procedure SelectAt (aIndex : integer);
      procedure Load;
      procedure Save;
      procedure Edit;
      function Name (index : integer) : string;
      procedure MarkDirty;
  end;

const
  ptBase = -1;
  ptPDF = 0;
  ptHTML = 1;
  ptText = 2;
  ptEPub = 3;

{$endregion interface}

implementation

uses
  forms, fpdfpro, fhtmlpro, ftextpro, fepubpro, flog,
  dgroff, gtools;

{$region tGroffFont }
{
function tGroffFont.GetByte (index : integer) : byte;
begin
  return (t_ffs [index])
end;

procedure tGroffFont.SetByte (a : byte; index : integer);
begin
  t_ffs [index] := a
end;

function tGroffFont.GetBool (index : integer) : boolean;
begin
  return (t_bools [index])
end;

procedure tGroffFont.SetBool (a : boolean; index : integer);
begin
  t_bools [index] := a
end;

function tGroffFont.GetInt (index : integer) : longint;
begin
  return (t_spaces [index])
end;

procedure tGroffFont.SetInt (a : longint; index : integer);
begin
  t_spaces [index] := a
end;
}
{$endregion}

{$region tBaseProfile}


class function tBaseProfile.ProfileType : integer;
begin
  ProfileType := ptBase;
end;

constructor tBaseProfile.Create;
begin
  RunError (211);
end;

procedure tBaseProfile.Load (var t : text);
begin
  RunError (211);
end;

procedure tBaseProfile.Save (var t : text);
begin
  RunError (211);
end;

procedure tBaseProfile.Edit;
begin
  RunError (211);
end;

function tBaseProfile.Duplicate : tBaseProfile;
begin
  RunError (211);
end;

procedure tBaseProfile.Build (aStory : tStory);
begin
  RunError (211);
end;

{$endregion tBaseProfile}

{$region tPDFProfile}

class function tPDFProfile.ProfileType : integer;
begin
  ProfileType := ptPDF;
end;

procedure tPDFProfile.SetPageSize (aPageSize : byte);
begin
  if (aPageSize in [0..PaperCount]) then begin
    t_pagesize := aPageSize;
    t_PageH := trunc (PaperMeasurements [aPageSize, 0]);
    t_PageV := trunc (PaperMeasurements [aPageSize, 1]);
  end;
end;

constructor tPDFProfile.Create;
begin
  Separator := '* * * * *';
  OutputDir := '';
  Useeqn := false;
  Usepreconv := false;
  Usetbl := false;
  UseHDTBL := false;
  CrossReference := false;
  UseIndex := false;
  UsePDFMark := false;
  ForceFirstPage := 0;
  ToCColumns := 1;
  IndexColumns := 1;

  // Font 0 - Plain Text
  GroffFonts [0].Family := 6; // Times New Roman
  GroffFonts [0].Font := 0; // Regular
  GroffFonts [0].Size := 10;
  GroffFonts [0].Centered := false;
  GroffFonts [0].SpaceAbove := 0;
  GroffFonts [0].SpaceBelow := 0;
  GroffFonts [0].ReserveSpace := 0;
  GroffFonts [0].Borders [0] := FALSE;
  GroffFonts [0].Borders [1] := FALSE;
  GroffFonts [0].Borders [2] := FALSE;
  GroffFonts [0].Borders [3] := FALSE;

  // Font 1 - Title
  GroffFonts [1].Family := 3; // Helvetica
  GroffFonts [1].Font := 0; // Regular
  GroffFonts [1].Size := 18;
  GroffFonts [1].Centered := true;
  GroffFonts [1].SpaceAbove := 18;
  GroffFonts [1].SpaceBelow := 18;
  GroffFonts [1].ReserveSpace := 0;
  GroffFonts [1].Borders [0] := FALSE;
  GroffFonts [1].Borders [1] := FALSE;
  GroffFonts [1].Borders [2] := FALSE;
  GroffFonts [1].Borders [3] := FALSE;

  // Font 2 - Subtitle
  GroffFonts [2].Family := 3; // Helvetica
  GroffFonts [2].Font := 0; // Regular
  GroffFonts [2].Size := 14;
  GroffFonts [2].Centered := true;
  GroffFonts [2].SpaceAbove := 18;
  GroffFonts [2].SpaceBelow := 18;
  GroffFonts [2].ReserveSpace := 0;
  GroffFonts [2].Borders [0] := FALSE;
  GroffFonts [2].Borders [1] := FALSE;
  GroffFonts [2].Borders [2] := FALSE;
  GroffFonts [2].Borders [3] := FALSE;

  // Font 3 - Heading 0
  GroffFonts [3].Family := 3; // Helvetica
  GroffFonts [3].Font := 1; // Bold
  GroffFonts [3].Size := 18;
  GroffFonts [3].Centered := true;
  GroffFonts [3].SpaceAbove := 18;
  GroffFonts [3].SpaceBelow := 18;
  GroffFonts [3].ReserveSpace := 0;
  GroffFonts [3].Borders [0] := FALSE;
  GroffFonts [3].Borders [1] := FALSE;
  GroffFonts [3].Borders [2] := FALSE;
  GroffFonts [3].Borders [3] := FALSE;

  // Font 4 - Heading 1
  GroffFonts [4].Family := 3; // Helvetica
  GroffFonts [4].Font := 1; // Bold
  GroffFonts [4].Size := 14;
  GroffFonts [4].Centered := true;
  GroffFonts [4].SpaceAbove := 0;
  GroffFonts [4].SpaceBelow := 18;
  GroffFonts [4].ReserveSpace := 0;
  GroffFonts [4].Borders [0] := FALSE;
  GroffFonts [4].Borders [1] := FALSE;
  GroffFonts [4].Borders [2] := FALSE;
  GroffFonts [4].Borders [3] := FALSE;

  // Font 5 - Heading 2
  GroffFonts [5].Family := 3; // Helvetica
  GroffFonts [5].Font := 1; // Bold
  GroffFonts [5].Size := 12;
  GroffFonts [5].Centered := true;
  GroffFonts [5].SpaceAbove := 0;
  GroffFonts [5].SpaceBelow := 0;
  GroffFonts [5].ReserveSpace := 0;
  GroffFonts [5].Borders [0] := FALSE;
  GroffFonts [5].Borders [1] := FALSE;
  GroffFonts [5].Borders [2] := FALSE;
  GroffFonts [5].Borders [3] := FALSE;

  // Font 6 - Heading 3
  GroffFonts [6].Family := 3; // Helvetica
  GroffFonts [6].Font := 2; // Italics
  GroffFonts [6].Size := 12;
  GroffFonts [6].Centered := true;
  GroffFonts [6].SpaceAbove := 0;
  GroffFonts [6].SpaceBelow := 0;
  GroffFonts [6].ReserveSpace := 0;
  GroffFonts [6].Borders [0] := FALSE;
  GroffFonts [6].Borders [1] := FALSE;
  GroffFonts [6].Borders [2] := FALSE;
  GroffFonts [6].Borders [3] := FALSE;

  // Font 7 - Heading 4
  GroffFonts [7].Family := 3; // Helvetica
  GroffFonts [7].Font := 1; // Bold
  GroffFonts [7].Size := 11;
  GroffFonts [7].Centered := false;
  GroffFonts [7].SpaceAbove := 0;
  GroffFonts [7].SpaceBelow := 0;
  GroffFonts [7].ReserveSpace := 0;
  GroffFonts [7].Borders [0] := FALSE;
  GroffFonts [7].Borders [1] := FALSE;
  GroffFonts [7].Borders [2] := FALSE;
  GroffFonts [7].Borders [3] := FALSE;

  // Font 8 - Heading 5
  GroffFonts [8].Family := 6; // Times New Roman
  GroffFonts [8].Font := 2; // Italics
  GroffFonts [8].Size := 11;
  GroffFonts [8].Centered := false;
  GroffFonts [8].SpaceAbove := 0;
  GroffFonts [8].SpaceBelow := 0;
  GroffFonts [8].ReserveSpace := 0;
  GroffFonts [8].Borders [0] := FALSE;
  GroffFonts [8].Borders [1] := FALSE;
  GroffFonts [8].Borders [2] := FALSE;
  GroffFonts [8].Borders [3] := FALSE;

  // Font 9-11 - Custom.  Defaults to Plain Text.
  GroffFonts [9] := GroffFonts [0];
  GroffFonts [10] := GroffFonts [0];
  GroffFonts [11] := GroffFonts [0];

  PageSize := 0; // Letter
  Landscape := FALSE;
  PageH := 612000; // in 1/1000 pt.
  PageV := 792000;
  Columns := 1;
  OneColumnTitlePage := FALSE;
  SuppressHeader := FALSE;

  OuterMargin := 36000; // in 1/1000 pt.
  InnerMargin := 72000;
  TopMargin := 72000;
  BottomMargin := 72000;

  // Odd Header
  OddHeader.Enabled := true;
  OddHeader.Left := '';
  OddHeader.Middle := '';
  OddHeader.Right := '%t';

  // Even Header
  EvenHeader.Enabled := true;
  EvenHeader.Left := '%c';
  EvenHeader.Middle := '';
  EvenHeader.Right := '';

  // Odd Footer
  OddFooter.Enabled := true;
  OddFooter.Left := '%a';
  OddFooter.Middle := '';
  OddFooter.Right := '%p';

  // Even Footer
  EvenFooter.Enabled := true;
  EvenFooter.Left := '%p';
  EvenFooter.Middle := '';
  EvenFooter.Right := '%a';
end;

procedure tPDFProfile.SetMargin (index : integer; value : longint);
begin
  t_margins [index] := value;
end;

function tPDFProfile.GetMargin (index : integer) : longint;
begin
  GetMargin := t_margins [index];
end;

procedure tPDFProfile.SetFlag (index : integer; aflag : boolean);
begin
  t_flags [index] := aflag;
end;

function tPDFProfile.GetFlag (index : integer) : boolean;
begin
	GetFlag := t_flags [index];
end;

procedure tPDFProfile.Load (var t : text);
var
  k,
  v,
  k1,
  v1,
  s : string;
  font : integer = -1;
  i,
  j: integer;
  r : longint;
  Done : boolean;
begin
  Done := false;
  repeat
    // Read Keyline
    readln (t, s);
    if (length (s) > 0) then
    	Trim (s);

    if (s = '[end]') then
      Done := true
    else begin
      // Find the = sign
      i := 0;
      repeat
        inc (i);
      until ((copy (s, i, 1) = '=') or (i > length (s)));

      // Split Keyline
      k := copy (s, 1, i - 1);
      v := copy (s, i + 1, length (s) - i);
      if (length (k) > 0) then
      	Trim (k);
      if (length (v) > 0) then
    	  Trim (v);

      // Select Variable
      if (k = 'Name') then
        Name := v
      else if (k = 'Output Directory') then
        OutputDir := v
      else if (k = 'Separator') then
        Separator := v

      // PDF related stuff
      else if (copy (k, 1, 10) = 'Groff Font') then begin
        k1 := copy (k, 12, 2);
        val (k1, font);

        with (GroffFonts [font]) do begin
          if (copy (v, length (v) - 8, 9) = ',Centered') then begin
            Centered := true;
            v := copy (v, 1, length (v) - 9);
          end else
            Centered := false;

          j := 0;
          while (copy (v, 1, length (FontFamilyNames [j]))
            <> FontFamilyNames [j]) and (j <= 6) do
            j += 1;
          Family := j;

          v1 := copy (v, length (FontFamilyNames [j]) + 2,
            length (v) - (length (FontFamilyNames [j]) + 1));

          j := 0;
          while (copy (v1, 1, length (FontNames [j])) <> FontNames [j])
            and (j <= 3) do
            j += 1;
          Font := j;

          v1 := copy (v1, length (FontNames [j]) + 2,
            length (v1) - (length (FontNames [j]) + 1));

          val (v1, j);
          Size := j;
        end
      end else if (k = 'Space Above') then begin
        if (font > -1) then begin
          val (v, r);
          GroffFonts [font].SpaceAbove := r;
        end
      end else if (k = 'Space Below') then begin
        if (font > -1) then begin
          val (v, r);
          GroffFonts [font].SpaceBelow := r;
        end
      end else if (k = 'Reserve Space') then begin
        if (font > -1) then begin
          val (v, r);
          GroffFonts [font].ReserveSpace := r;
        end
      end else if (k = 'Borders') then begin
        if (font > -1) then begin
          val (v, j);
          if ((j and 1) = 1) then
            GroffFonts [font].Borders [0] := true;
          if ((j and 2) = 2) then
            GroffFonts [font].Borders [1] := true;
          if ((j and 4) = 4) then
            GroffFonts [font].Borders [2] := true;
          if ((j and 8) = 8) then
            GroffFonts [font].Borders [3] := true;
        end
      end else if (k = 'Paper Size') then begin
        v1 := copy (v, length (v) - 9, 10);
        if (v1 = ' Landscape') then begin
          Landscape := true;
          v := copy (v, 1, length (v) - 10);
        end;
        j := 0;
        while ((j <= PaperCount) and (PaperSizes [j, 1] <> v)) do
          j += 1;
        PageSize := j;
        if (PageSize < PaperCount) then
          if (Landscape) then begin
            PageH := trunc (PaperMeasurements [PageSize, 1]);
            PageV := trunc (PaperMeasurements [PageSize, 0]);
          end else begin
            PageH := trunc (PaperMeasurements [PageSize, 0]);
            PageV := trunc (PaperMeasurements [PageSize, 1]);
          end;
      end else if (k = 'Page Width') then begin
        val (v, r);
        PageH := r
      end else if (k = 'Page Height') then begin
        val (v, r);
        PageV := r
      end else if (k = 'Columns') then begin
        val (v, j);
        Columns := j
      end else if (k = 'Outer Margin') then begin
        val (v, r);
        OuterMargin := r
      end else if (k = 'Inner Margin') then begin
        val (v, r);
        InnerMargin := r
      end else if (k = 'Top Margin') then begin
        val (v, r);
        TopMargin := r
      end else if (k = 'Bottom Margin') then begin
        val (v, r);
        BottomMargin := r
      end else if (k = 'H1 Mode') then begin
        val (v, r);
        H1Mode := r
      end else if (k = 'Force First Page') then begin
        if (v = 'Odd') then
          ForceFirstPage := 1
        else if (v = 'Even') then
          ForceFirstPage := 2
      end else if (k = 'Odd Header') then begin
        if (v = 'Disabled') then
          OddHeader.Enabled := false
        else begin
          OddHeader.Enabled := true;
          k1 := copy (v, 1, 1);
          v1 := '';
          j := 2;
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          OddHeader.Left := v1;
          v1 := '';
          inc (j);
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          OddHeader.Middle := v1;
          v1 := '';
          inc (j);
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          OddHeader.Right := v1
        end;
      end else if (k = 'Even Header') then begin
        if (v = 'Disabled') then
          EvenHeader.Enabled := false
        else begin
          EvenHeader.Enabled := true;
          k1 := copy (v, 1, 1);
          v1 := '';
          j := 2;
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          EvenHeader.Left := v1;
          v1 := '';
          inc (j);
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          EvenHeader.Middle := v1;
          v1 := '';
          inc (j);
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          EvenHeader.Right := v1
        end;
      end else if (k = 'Even Footer') then begin
        if (v = 'Disabled') then
          EvenFooter.Enabled := false
        else begin
          EvenFooter.Enabled := true;
          k1 := copy (v, 1, 1);
          v1 := '';
          j := 2;
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          EvenFooter.Left := v1;
          v1 := '';
          inc (j);
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          EvenFooter.Middle := v1;
          v1 := '';
          inc (j);
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          EvenFooter.Right := v1
        end;
      end else if (k = 'Odd Footer') then begin
        if (v = 'Disabled') then
          OddFooter.Enabled := false
        else begin
          OddFooter.Enabled := true;
          k1 := copy (v, 1, 1);
          v1 := '';
          j := 2;
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          OddFooter.Left := v1;
          v1 := '';
          inc (j);
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          OddFooter.Middle := v1;
          v1 := '';
          inc (j);
          while (copy (v, j, 1) <> k1) do begin
            v1 += copy (v, j, 1);
            inc (j)
          end;
          OddFooter.Right := v1
        end;
      end else if (k = 'One Column Title Page') then
        OneColumnTitlePage := TRUE
      else if (k = 'Suppress Headers') then
        SuppressHeader := TRUE

      // ToC/Index
  		else if (k = 'ToC Columns') then begin
        val (v, r);
        ToCColumns := r
      end else if (k = 'Use Cross References') then
        CrossReference := true
      else if (k = 'Use Index') then
        UseIndex := true
      else if (k = 'Index Columns') then begin
        val (v, r);
        IndexColumns := r
      end

      // Groff stuff
      else if (k = 'Call eqn') then
        Useeqn := true
      else if (k = 'Call preconv') then
        Usepreconv := true
      else if (k = 'Call tbl') then
        Usetbl := true
      else if (k = 'Use PDFMark') then
        UsePDFMark := true
      else if (k = 'Use HDTBL') then
        UseHDTBL := true
    end;
  until Done;
end;

procedure tPDFProfile.Save (var t : text);
var
  j,
  k,
  l : integer;
begin
  writeln (t, '[Profile PDF]');
  writeln (t, 'Name = ', Name);
  writeln (t, 'Output Directory = ', OutputDir);
  writeln (t, 'Separator = ', Separator);

  for j := 0 to 11 do begin
    write (t, 'Groff Font ', j:2, ' = ');
    write (t, FontFamilyNames [GroffFonts [j].Family], ',');
    write (t, FontNames [GroffFonts [j].Font], ',');
    write (t, GroffFonts [j].Size);
    if (GroffFonts [j].Centered) then
      write (t, ',Centered');
    writeln (t);
    writeln (t, 'Space Above = ', GroffFonts [j].SpaceAbove);
    writeln (t, 'Space Below = ', GroffFonts [j].SpaceBelow);
    writeln (t, 'Reserve Space = ', GroffFonts [j].ReserveSpace);
    l := 0;
    for k := 0 to 3 do
      if (GroffFonts [j].Borders [k]) then
        l += 1 shl k;
    writeln (t, 'Borders = ', l);
  end;

  write (t, 'Paper Size = ', PaperSizes [PageSize, 1]);
  if (PageSize = PaperCount) then begin
    writeln (t);
    writeln (t, '    Page Width = ', PageH);
    writeln (t, '    Page Height = ', PageV);
  end else if (Landscape) then
    writeln (t, ' Landscape')
  else
    writeln (t);

  writeln (t, 'Columns = ', Columns);
  writeln (t, 'Outer Margin = ', OuterMargin);
  writeln (t, 'Inner Margin = ', InnerMargin);
  writeln (t, 'Top Margin = ', TopMargin);
  writeln (t, 'Bottom Margin = ', BottomMargin);
  writeln (t, 'H1 Mode = ', H1Mode);
  if (ForceFirstPage > 0) then begin
    write (t, 'Force First Page = ');
    if (ForceFirstPage = 1) then
      writeln (t, 'Odd')
    else
      writeln (t, 'Even');
  end;
  if (OneColumnTitlePage) then
    writeln (t, 'One Column Title Page');
  if (SuppressHeader) then
    writeln (t, 'Suppress Headers');

  write (t, 'Odd Header = ');
  if (OddHeader.Enabled) then
    writeln (t, '*', OddHeader.Left, '*', OddHeader.Middle, '*',
      OddHeader.Right, '*')
  else
    writeln (t, 'Disabled');

  write (t, 'Even Header = ');
  if (EvenHeader.Enabled) then
    writeln (t, '*', EvenHeader.Left, '*', EvenHeader.Middle, '*',
      EvenHeader.Right, '*')
  else
    writeln (t, 'Disabled');

  write (t, 'Odd Footer = ');
  if (OddFooter.Enabled) then
    writeln (t, '*', OddFooter.Left, '*', OddFooter.Middle, '*',
      OddFooter.Right, '*')
  else
    writeln (t, 'Disabled');

  write (t, 'Even Footer = ');
  if (EvenFooter.Enabled) then
    writeln (t, '*', EvenFooter.Left, '*', EvenFooter.Middle, '*',
      EvenFooter.Right, '*')
  else
    writeln (t, 'Disabled');

  // ToC/Index
  writeln (t, 'ToC Columns = ', ToCColumns);
  if (CrossReference) then
    writeln (t, 'Use Cross References');
  if (UseIndex) then begin
    writeln (t, 'Use Index');
    writeln (t, 'Index Columns = ', IndexColumns);
  end;

  // Groff Options
  if (Useeqn) then
    writeln (t, 'Call eqn');
  if (Usepreconv) then
    writeln (t, 'Call preconv');
  if (usetbl) then
    writeln (t, 'Call tbl');
  if (UsePDFMark) then
    writeln (t, 'Use PDFMark');
  if (UseHDTBL) then
    writeln (t, 'Use HDTBL');
  writeln (t, '[end]');
  writeln (t);
end;

procedure tPDFProfile.Edit;
var
  Dialog : tfrmPDFProfile;
begin
  Dialog := TfrmPDFProfile.Create (Application);
  TfrmPDFProfile (Dialog).Profile := self;
  Dialog.ShowModal;
  Dialog.Destroy;
end;

function tPDFProfile.Duplicate : tPDFProfile;
var
  dup : tPDFProfile;
begin
  dup := tPDFProfile.Create;
  dup.Name := Name;
  dup.OutputDir := OutputDir;
  dup.UsePreconv := UsePreconv;
  dup.Separator := Separator;
  dup.GroffFonts := GroffFonts;
  dup.PageSize := PageSize;
  dup.PageH := PageH;
  dup.PageV := PageV;
  dup.Columns := Columns;
  dup.OuterMargin := OuterMargin;
  dup.InnerMargin := InnerMargin;
  dup.TopMargin := TopMargin;
  dup.BottomMargin := BottomMargin;
  dup.H1Mode := H1Mode;
  dup.ForceFirstPage := ForceFirstPage;
  dup.OneColumnTitlePage := OneColumnTitlePage;
  dup.Landscape := Landscape;
  dup.UseTBL := UseTBL;
  dup.UseEQN := UseEQN;
  dup.UseIndex := UseIndex;
  dup.CrossReference := CrossReference;
  dup.IndexColumns := IndexColumns;
  dup.ToCColumns := ToCColumns;
  dup.UsePDFMark := UsePDFMark;
  dup.SuppressHeader := SuppressHeader;
  Duplicate := dup;
end;

procedure tPDFProfile.Build (aStory : tStory);
var
  Chapters : tChapterList;

  function ExpandHeader (h : tGroffHeader) : string;
  var
    s,
    t : string;
    i : integer;
  begin
    if (h.Enabled) then begin
      t := '^';
      s := h.Left;
      i := 1;

      if (length (s) > 0) then
        repeat
          if (s [i] = '%') then begin
            i += 1;
            if (i <= length (s)) then
              case (UpCase (s [i])) of
            	  'A' :
                  t += aStory.Author;
                'C' :
                  t += Chapters.Current.Title;
                'D' :
                  t += Chapters.Current.Subtitle;
                'P' :
                  t += '%';
                'T' :
                  t += aStory.Title;
                'U' :
                  t += aStory.Subtitle;
              end;
        	  end
          else
          	t += s [i];
          i += 1;
        until (i > length (s));

      t += '^';
      s := h.Middle;
      i := 1;
      if (length (s) > 0) then
        repeat
          if (s [i] = '%') then begin
            i += 1;
            if (i <= length (s)) then
              case (UpCase (s [i])) of
            	  'A' :
                  t += aStory.Author;
                'C' :
                  t += Chapters.Current.Title;
                'D' :
                  t += Chapters.Current.Subtitle;
                'P' :
                  t += '%';
                'T' :
                  t += aStory.Title;
                'U' :
                  t += aStory.Subtitle;
              end;
        	  end
          else
          	t += s [i];
          i += 1;
        until (i > length (s));

      t += '^';
      s := h.Right;
      i := 1;
      if (length (s) > 0) then
        repeat
          if (s [i] = '%') then begin
            i += 1;
            if (i <= length (s)) then
              case (UpCase (s [i])) of
            	  'A' :
                  t += aStory.Author;
                'C' :
                  t += Chapters.Current.Title;
                'D' :
                  t += Chapters.Current.Subtitle;
                'P' :
                  t += '%';
                'T' :
                  t += aStory.Title;
                'U' :
                  t += aStory.Subtitle;
              end;
        	  end
          else
          	t += s [i];
          i += 1;
        until (i > length (s));

      t += '^';
    end else
    	t := '^^^^';
    ExpandHeader := t
  end;

var
  UsesBooks : boolean = FALSE;
  UsesBorders : boolean = FALSE;
  indx : integer;
  s,
  pathname,
  filename,
  cfilename,
  ofilename : string;
  x : text;
  pagelength,
  linelength,
  colwidth,
  indcolwidth,
  toccolwidth : real;
begin
  // Mark the Log
  s := ' -> Creating PDF for Profile ''' + Name + '''';
  frmLog.txtLog.Lines.Add (s);

  // Load Chapters List
  Chapters := tChapterList.Create;
  Chapters.BaseDir := aStory.SourceDir;
  Chapters.Load;

  // Are any of the Chapters Book Dividers?
  indx := 0;
  repeat
    Chapters.SelectAt (indx);
    UsesBooks := (Chapters.Current.IsABook) OR UsesBooks;
    indx += 1;
  until (indx >= Chapters.Count);

  // Do any of the Styles use Borders?
  UsesBorders := false;
  for indx := 1 to 8 do begin
    UsesBorders := UsesBorders OR GroffFonts [indx].Borders[0];
    UsesBorders := UsesBorders OR GroffFonts [indx].Borders[1];
    UsesBorders := UsesBorders OR GroffFonts [indx].Borders[2];
    UsesBorders := UsesBorders OR GroffFonts [indx].Borders[3];
  end;
  Usetbl := Usetbl OR UsesBorders;

  pathname := aStory.SourceDir + '/';
  ofilename := StripSpaces (aStory.LongName + '-' + Name);
  filename := pathname + '/' + ofilename + '.ms';

  // Page Size Information
	pagelength := PageV / 1000;
  linelength := (PageH - (InnerMargin + OuterMargin)) / 1000;
  colwidth := (linelength - 36 * (Columns - 1)) / Columns;
  toccolwidth := (linelength - 36 * (ToCColumns - 1)) / ToCColumns;
  indcolwidth := (linelength - 36 * (IndexColumns - 1)) / IndexColumns;

  // Create Output File
  assign (x, filename);
  rewrite (x);

  // Calling HDTBL must be done early
  if (UseHDTBL) then
    writeln (x, '.mso hdtbl.tmac');

  // Page Dimensions
  writeln (x, '.hlm 0');
  writeln (x, '.de BT');
  writeln (x, '.ie o .tl \\*[pg*OF]');
  writeln (x, '.el .tl \\*[pg*EF]');
  writeln (x, '.ie e .nr PO ', (InnerMargin / 1000):0:3, 'p');
  writeln (x, '.el .nr PO ', (OuterMargin / 1000):0:3, 'p');
  writeln (x, '..');
  writeln (x, '.ps ', GroffFonts [0].Size);
  writeln (x, '.fam ', T_Family [GroffFonts [0].Family]);
  writeln (x, '.open TOC ', ofilename, '.toc');
  writeln (x, '.write TOC .LP');
  writeln (x, '.write TOC .ta ', toccolwidth:0:3, 'pR');
  writeln (x, '.write TOC .tc .');
  writeln (x, '.ps ', GroffFonts [0].size);
  writeln (x, '.nr PS ', GroffFonts [0].size);
  writeln (x, '.ds FAM ', T_Family [GroffFonts [0].family]);
  writeln (x, '.po ', (InnerMargin / 1000):0:3, 'p');
  writeln (x, '.ll ', linelength:0:3, 'p');
  writeln (x, '.hm ', (TopMargin / 1000):0:3, 'p');
  writeln (x, '.fm ', (BottomMargin / 1000):0:3, 'p');
  writeln (x, '.pl ', pagelength:0:3, 'p');
  writeln (x, '.nr PO ', (InnerMargin / 1000):0:3, 'p');
  writeln (x, '.nr LL ', linelength:0:3, 'p');
  writeln (x, '.nr HM ', (TopMargin / 1000):0:3, 'p');
  writeln (x, '.nr FM ', (BottomMargin / 1000):0:3, 'p');
  writeln (x, '.nr PL ', pagelength:0:3, 'p');
  writeln (x, '.EH ****');
  writeln (x, '.OH ****');
  writeln (x, '.EF ****');
  writeln (x, '.OF ****');

  { Cross-Reference and Index support }
  if (CrossReference) then
    writeln (x, '.so ', ofilename, '.ref');
  if (UseIndex) then
    writeln (x, '.so ', ofilename, '.idx');
  if (CrossReference OR UseIndex) then begin
    writeln (x, '.de XREFSTART');
	  if (CrossReference) then begin
      writeln (x, '.open XREFS ', ofilename, '.ref');
		  writeln (x, '.write XREFS ".de XREF');
    end;
		if (UseIndex) then begin
      writeln (x, '.open INDEX ', ofilename, '.idx');
		  writeln (x, '.write INDEX ".de IDXP');
		  writeln (x, '.write INDEX ".br');
    end;
	  writeln (x, '..');
    writeln (x, '.de XREFSTOP');
    if (CrossReference) then begin
      writeln (x, '.write XREFS "..');
	    writeln (x, '.close XREFS');
    end;
    writeln (x, '..');
  end;
  if (CrossReference) then begin
	  writeln (x, '.de BOOKMARK');
  	writeln (x, '.write XREFS .if ''\\\\\\\\$1''\\$1'' \\n[%]\\\\\\\\$2');
  	if (UseIndex) then
      writeln (x, '.write INDEX .if ''\\\\\\\\$1''\\$1''\\\\\\\\$2', #9, '\\n[%]');
	  writeln (x, '..');
  end;
  {
	if (UseIndex) then begin
	  writeln (x, '.de IDX');
  	writeln (x, '.write INDEX .if ''\\\\\\\\$1''\\$1''\\\\\\\\$2', #9, '\\n[%]');
	  writeln (x, '..');
  end;
  }
  if (CrossReference OR UseIndex) then
    writeln (x, '.XREFSTART ', ofilename);
  if (UseIndex) then begin
    writeln (x, '.de INDEXSTOP ..');
    writeln (x, '.write INDEX "..');
    writeln (x, '.close INDEX');
    writeln (x, '...');
  end;
  writeln (x, '.eo');

  { Header Zero }
  if (UsesBooks) then begin
    writeln (x, '.de H0 ..');
    writeln (x, '.EH ****');
    writeln (x, '.OH ****');
    writeln (x, '.EF ****');
    writeln (x, '.OF ****');
    writeln (x, '.LP');
    writeln (x, '.sp ', GroffFonts [3].SpaceAbove, 'p');
    writeln (x, '.fam ', T_Family [GroffFonts [3].family]);
    writeln (x, '.ps ', GroffFonts [3].size);
    writeln (x, '.vs ', GroffFonts [3].size + 2);
    if (UsePDFMark) then
      writeln (x, '.pdfhref O 1 \$1');
    if (UsesBorders) then begin
      writeln (x, '.TS');
      writeln (x, ';');
      if (GroffFonts [3].Borders [2]) then
        write (x, '| ');
      if (GroffFonts [3].Centered) then
        write (x, 'cx')
      else
        write (x, 'lx');
      if (GroffFonts [3].Borders [3]) then
        write (x, ' |');
      writeln (x, '.');
      if (GroffFonts [3].Borders [0]) then
        writeln (x, '_');
      write (x, X_Fonts [GroffFonts [3].Font]);
		  writeln (x, '\$1');
      if (GroffFonts [3].Borders [1]) then
        writeln (x, '_');
      writeln (x, '.TE');
    end else begin
      if (GroffFonts [3].Centered) then
	      writeln (x, '.ce');
      write (x, X_Fonts [GroffFonts [3].Font]);
		  writeln (x, '\$1');
    end;
		writeln (x, '.write TOC .br');
		writeln (x, '.ie !''\$2'''' \');
		writeln (x, '.write TOC \fB\$1:  \$2', #9, '\n[%]\fR');
		writeln (x, '.LP');
	  writeln (x, '.sp ', GroffFonts [3].SpaceBelow, 'p');
		writeln (x, '.ps ', GroffFonts [3].size);
		writeln (x, '.ce');
		writeln (x, '\$2');
		writeln (x, '.el \');
		writeln (x, '.write TOC \fB\$1', #9, '\n[%]\fR');
	  writeln (x, '.sp ', GroffFonts [3].SpaceBelow, 'p');
		writeln (x, '...');
	end;

  { Header One }
  writeln (x, '.de H1 ..');
  writeln (x, '.EH ****');
  writeln (x, '.OH ****');
  writeln (x, '.LP');
  writeln (x, '.ne 1i');
  writeln (x, '\&');
  if (UsePDFMark) then
	  if (UsesBooks) then
      writeln (x, '.pdfhref O 2 \$1')
    else
    	writeln (x, '.pdfhref O 1 \$1');
  writeln (x, '.sp ', GroffFonts [4].SpaceAbove, 'p');
  {
  if (HeadGraphic <> '') then
	  begin
		  write (x, '.PSPIC ');
		  if (Desc.H1CenterMode > 1) then
			  write (x, '-L ');
		  writeln (x, Desc.HeadGraphic);
		  writeln (x, '.sp -', GroffFonts [1].size + 5, 'p');
	  end;
  }
  writeln (x, '.fam ', T_Family [GroffFonts [4].family]);
  writeln (x, '.ps ', GroffFonts [4].size);
  writeln (x, '.vs ', GroffFonts [4].size + 2);
//	if ((Columns > 1) and (H1Mode in [0, 2])) then
//		writeln (x, '.MC ', colwidth:0:3, 'p 36p')
//  else
//    writeln (x, '.1C');
  if (UsesBorders) then begin
  	writeln (x, '.TS');
    writeln (x, ';');
    if (GroffFonts [4].Borders [2]) then
      write (x, '| ');
    if (H1Mode > 1) then
      write (x, 'cx')
    else
      write (x, 'lx');
    if (GroffFonts [4].Borders [3]) then
      write (x, ' |');
    writeln (x, '.');
    if (GroffFonts [4].Borders [0]) then
    	writeln (x, '_');
    write (x, X_Fonts [GroffFonts [4].Font]);
    writeln (x, '\$1');
    if (GroffFonts [4].Borders [1]) then
    	writeln (x, '_');
    writeln (x, '.TE');
  end else begin
    if (H1Mode > 1) then
	    writeln (x, '.ce');
    write (x, X_Fonts [GroffFonts [4].Font]);
    writeln (x, '\$1\fR');
  end;
	writeln (x, '.sp ', GroffFonts [4].SpaceBelow, 'p');
  writeln (x, '.write TOC .br');
  if (UsesBooks) then
	  writeln (x, '.write TOC \ \ \ \ \$1', #9, '\n[%]')
  else
	  writeln (x, '.write TOC \fB\$1', #9, '\n[%]\fR');

  writeln (x, '...');

  { Header Two }
  writeln (x, '.de H2 ..');
  writeln (x, '.LP');
  writeln (x, '.ne 1i');
  writeln (x, '.sp ', GroffFonts [5].SpaceAbove, 'p');
  writeln (x, '.fam ', T_Family [GroffFonts [5].family]);
  writeln (x, '.ps ', GroffFonts [5].size);
  writeln (x, '.vs ', GroffFonts [5].size + 2);
  if (UsePDFMark) then
	  if (UsesBooks) then
      writeln (x, '.pdfhref O 3 \$1')
    else
    	writeln (x, '.pdfhref O 2 \$1');
  if (UsesBorders) then begin
  	writeln (x, '.TS');
    writeln (x, 'tab(|);');
    if (GroffFonts [5].Borders [2]) then
      write (x, '| ');
    if (GroffFonts [5].Centered) then
      write (x, 'cx r')
    else
      write (x, 'lx r');
    if (GroffFonts [5].Borders [3]) then
      write (x, ' |');
    writeln (x, '.');
    if (GroffFonts [5].Borders [0]) then
    	writeln (x, '_');
    write (x, X_Fonts [GroffFonts [5].Font]);
    writeln (x, '\$1|\$2');
    if (GroffFonts [5].Borders [1]) then
    	writeln (x, '_');
    writeln (x, '.TE');
  end else begin
    if (GroffFonts [5].Centered) then
	    write (x, '.tl ~~')
    else
      write (x, '.tl ~');
    write (x, X_Fonts [GroffFonts [5].Font]);
    write (x, '\$1\fR');
    if (GroffFonts [5].Centered) then
	    write (x, '.tl ~')
    else
      write (x, '.tl ~~');
    writeln (x, '\$2~');
  end;
  writeln (x, '.sp ', GroffFonts [5].SpaceBelow, 'p');
  writeln (x, '.write TOC .br');
  if (UsesBooks) then
	  writeln (x, '.write TOC \ \ \ \ \ \ \ \ \$1', #9, '\n[%]')
  else
	  writeln (x, '.write TOC \ \ \ \ \$1', #9, '\n[%]');
  writeln (x, '...');

  { Header Three }
  writeln (x, '.de H3 ..');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp ', GroffFonts [6].SpaceAbove, 'p');
  writeln (x, '.fam ', T_Family [GroffFonts [6].family]);
  writeln (x, '.ps ', GroffFonts [6].size);
  writeln (x, '.vs ', GroffFonts [6].size + 2);
  if (UsePDFMark) then
	  if not (UsesBooks) then
      writeln (x, '.pdfhref O 3 \$1');
  if (UsesBorders) then begin
  	writeln (x, '.TS');
    writeln (x, 'tab(|);');
    if (GroffFonts [6].Borders [2]) then
      write (x, '| ');
    if (GroffFonts [6].Centered) then
      write (x, 'cx r')
    else
      write (x, 'lx r');
    if (GroffFonts [6].Borders [3]) then
      write (x, ' |');
    writeln (x, '.');
    if (GroffFonts [6].Borders [0]) then
    	writeln (x, '_');
    write (x, X_Fonts [GroffFonts [6].Font]);
    writeln (x, '\$1|\$2');
    if (GroffFonts [6].Borders [1]) then
    	writeln (x, '_');
    writeln (x, '.TE');
  end else begin
    if (GroffFonts [6].Centered) then
	    write (x, '.tl ~~')
    else
      write (x, '.tl ~');
    write (x, X_Fonts [GroffFonts [6].Font]);
    write (x, '\$1\fR');
    if (GroffFonts [6].Centered) then
	    write (x, '.tl ~')
    else
      write (x, '.tl ~~');
    writeln (x, '\$2~');
  end;
  writeln (x, '.sp ', GroffFonts [6].SpaceBelow, 'p');
  writeln (x, '...');

  { Header Four }
  writeln (x, '.de H4 ..');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp ', GroffFonts [7].SpaceAbove, 'p');
  writeln (x, '.fam ', T_Family [GroffFonts [7].family]);
  writeln (x, '.ps ', GroffFonts [7].size);
  writeln (x, '.vs ', GroffFonts [7].size + 2);
  if (UsesBorders) then begin
    writeln (x, '.TS');
    writeln (x, 'tab(|);');
    if (GroffFonts [7].Borders [2]) then
      write (x, '| ');
    if (GroffFonts [7].Centered) then
      write (x, 'cx r')
    else
      write (x, 'lx r');
    if (GroffFonts [7].Borders [3]) then
      write (x, ' |');
    writeln (x, '.');
    if (GroffFonts [7].Borders [0]) then
      writeln (x, '_');
    write (x, X_Fonts [GroffFonts [7].Font]);
	  writeln (x, '\$1|\$2');
    if (GroffFonts [7].Borders [1]) then
      writeln (x, '_');
    writeln (x, '.TE');
  end else begin
    if (GroffFonts [7].Centered) then
	    write (x, '.tl ~~')
    else
      write (x, '.tl ~');
    write (x, X_Fonts [GroffFonts [7].Font]);
    write (x, '\$1\fR');
    if (GroffFonts [7].Centered) then
	    write (x, '.tl ~')
    else
      write (x, '.tl ~~');
    writeln (x, '\$2~');
  end;
  writeln (x, '.sp ', GroffFonts [7].SpaceBelow, 'p');
  writeln (x, '...');

  { Header Five }
  writeln (x, '.de H5 ..');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp ', GroffFonts [8].SpaceAbove, 'p');
  writeln (x, '.fam ', T_Family [GroffFonts [8].family]);
  writeln (x, '.ps ', GroffFonts [8].size);
  writeln (x, '.vs ', GroffFonts [8].size + 2);
  if (UsesBorders) then begin
    writeln (x, '.TS');
    writeln (x, 'tab(|);');
    if (GroffFonts [8].Borders [2]) then
      write (x, '| ');
    if (GroffFonts [8].Centered) then
      write (x, 'cx r')
    else
      write (x, 'lx r');
    if (GroffFonts [8].Borders [3]) then
      write (x, ' |');
    writeln (x, '.');
    if (GroffFonts [8].Borders [0]) then
      writeln (x, '_');
    write (x, X_Fonts [GroffFonts [8].Font]);
	  writeln (x, '\$1|\$2');
    if (GroffFonts [8].Borders [1]) then
      writeln (x, '_');
    writeln (x, '.TE');
  end else begin
    if (GroffFonts [8].Centered) then
	    write (x, '.tl ~~')
    else
      write (x, '.tl ~');
    write (x, X_Fonts [GroffFonts [8].Font]);
    write (x, '\$1\fR');
    if (GroffFonts [8].Centered) then
	    write (x, '.tl ~')
    else
      write (x, '.tl ~~');
    writeln (x, '\$2~');
  end;
  writeln (x, '.sp ', GroffFonts [8].SpaceBelow, 'p');
  writeln (x, '...');
  writeln (x, '.ec');

  { Custom One }
  writeln (x, '.de CP1');
  writeln (x, '.fam ', T_Family [GroffFonts [9].family]);
  writeln (x, '.ps ', GroffFonts [9].size);
	if (GroffFonts [9].Centered) then
		writeln (x, '.ce');
  writeln (x, R_Fonts [GroffFonts [9].Font]);
  writeln (x, '..');

  { Custom Two }
  writeln (x, '.de CP2');
  writeln (x, '.fam ', T_Family [GroffFonts [10].family]);
  writeln (x, '.ps ', GroffFonts [10].size);
  if (GroffFonts [10].Centered) then
  	writeln (x, '.ce');
  writeln (x, R_Fonts [GroffFonts [10].Font]);
  writeln (x, '..');

  { Custom Three }
  writeln (x, '.de CP3');
  writeln (x, '.fam ', T_Family [GroffFonts [11].family]);
  writeln (x, '.ps ', GroffFonts [11].size);
  if (GroffFonts [11].Centered) then
  	writeln (x, '.ce');
  writeln (x, R_Fonts [GroffFonts [11].Font]);
  writeln (x, '..');

  { Separator }
  writeln (x, '.de SEP');
  writeln (x, '.LP');
  writeln (x, '.ce');
  writeln (x, Separator);
  writeln (x, '');
  writeln (x, '..');

  // Primitive support for JPEG
  writeln (x, '.de JPEG');
  writeln (x, '.sy jpegtopnm \\$1 | pnmcrop | pnmtops -noturn -nosetpage > \\$1.ps');
  writeln (x, '.PSPIC \\$1.ps');
  writeln (x, '..');


  // This is where actual entries start being written - everything before this
  // point was a macro.


  // Title Page
  writeln (x, '.tc');
	if (Columns > 1) then
	  if not (OneColumnTitlePage) then
      writeln (x, '.MC ', colwidth:0:3, 'p 36p');

  writeln (x, '.ad b');
  writeln (x, '.sp ', GroffFonts [1].SpaceAbove, 'p');
  writeln (x, '.fam ', T_Family [GroffFonts [1].family]);
  writeln (x, '.ps ', GroffFonts [1].size);

  s := aStory.TitlePicture;
  if (FileExists (s) and (s <> '')) then
	  writeln (x, '.JPEG ', s);

  if (aStory.SuppressTitles = FALSE) then
	  begin
		  writeln (x, '.ce');
		  writeln (x, aStory.Title);
      writeln (x, '.sp ', GroffFonts [1].SpaceBelow, 'p');
      if (aStory.Subtitle <> '') then begin
        writeln (x, '.ps ', GroffFonts [2].size);
  	    writeln (x, '.fam ', T_Family [GroffFonts [2].family]);
  	    writeln (x, '.ce');
  	    writeln (x, aStory.Subtitle);
        writeln (x, '.sp ', GroffFonts [2].SpaceBelow, 'p');
      end;
	  end;
  writeln (x, '.sp ', GroffFonts [1].SpaceBelow, 'p');

  if (aStory.SuppressAuthor = FALSE) then begin
    writeln (x, '.ps ', GroffFonts [2].size);
    writeln (x, '.fam ', T_Family [GroffFonts [2].family]);
    writeln (x, '.sp ', GroffFonts [2].SpaceBelow, 'p');
    writeln (x, '.ce');
    writeln (x, 'by ', aStory.Author);
  end;

  writeln (x, '.sp ', GroffFonts [2].SpaceBelow, 'p');
  writeln (x, '.PP');
  if (FileExists (pathname + 'blurb.so')) then begin
    writeln (x, '.so blurb.so');
    writeln (x, '.PP');
  end;

  if (FileExists (pathname + 'disclaimer.so')) then begin
    writeln (x, '.B1');
    writeln (x, '.so disclaimer.so');
    writeln (x, '.B2');
  end;

  if (FileExists (pathname + 'credits.so')) then
	  begin
		  writeln (x, '.LP');
		  writeln (x, '.so credits.so');
	  end;


  // Table of Contents

  writeln (x, '.LP');
  writeln (x, '.ne 9i');
  writeln (x, '.1C');
  writeln (x, '.ps ', GroffFonts [2].size);
  writeln (x, '.ce');
  writeln (x, 'Table of Contents');
  writeln (x, '.ps ', GroffFonts [0].size);
  if (ToCColumns <> Columns) then
  	writeln (x, '.MC ', toccolwidth:0:3, 'p 36p')
  else if (Columns > 1) then
    writeln (x, '.MC ', colwidth:0:3, 'p 36p');
  writeln (x, '.so ', ofilename, '.toc');
  if (Columns = 1) then
    writeln (x, '.1C')
  else if (ToCColumns <> Columns) then
	  writeln (x, '.MC ', colwidth:0:3, 'p 36p');

  // Now write the chapters

  for indx := 0 to (Chapters.Count - 1) do begin
  	Chapters.SelectAt (indx);
    if ((Chapters.Current.IsIndex) and (UseIndex = FALSE)) then
      frmLog.txtLog.Lines.Add
        ('WARNING:  Index Chapter found and no Index created');

    writeln (x, '.LP');
    {
    Suppress headers on first page - changed 04 October 2017
    }
    if (SuppressHeader) then begin
      writeln (x, '.EH ****');
      writeln (x, '.OH ****');
    end else begin
      writeln (x, '.EH ' + ExpandHeader (EvenHeader));
      writeln (x, '.OH ' + ExpandHeader (OddHeader));
    end;

    writeln (x, '.bp');
    if (ForceFirstPage = 1) then begin
      writeln (x, '.if e');
      writeln (x, '\ ');
      writeln (x, '.bp');
      writeln (x, '..');
    end else if (ForceFirstPage = 2) then begin
      writeln (x, '.if o');
      writeln (x, '\ ');
      writeln (x, '.bp');
      writeln (x, '..');
    end;

    //Columns for Header 1 - bugfix 08 April 2017
    if ((Columns > 1) and (H1Mode in [0, 2])) then
    	writeln (x, '.MC ', colwidth:0:3, 'p 36p')
    else
      writeln (x, '.1C');

    cfilename := Chapters.Current.Filename;
    if (FileExists (pathname + '/' + cfilename + '.co')) then begin
      writeln (x, '.so ' + cfilename + '.co');
    end;
    if not (Chapters.Current.SuppressTitle) then
      if (Chapters.Current.IsABook) then begin
        if ((Chapters.Current.SubtitleFirst) and (Chapters.Current.Subtitle <> '')) then
          writeln (x, '.H0 "', Chapters.Current.Subtitle, '" "', Chapters.Current.Title, '"')
        else
          writeln (x, '.H0 "', Chapters.Current.Title, '" "', Chapters.Current.Subtitle, '"')
      end else begin
        if ((Chapters.Current.SubtitleFirst) and (Chapters.Current.Subtitle <> '')) then
          writeln (x, '.H3 "', Chapters.Current.Subtitle, '"');
        writeln (x, '.H1 "', Chapters.Current.Title, '"');
        if (not (Chapters.Current.SubtitleFirst) and (Chapters.Current.Subtitle <> '')) then
          writeln (x, '.H3 "', Chapters.Current.Subtitle, '"');
      end;

    if ((UseIndex) and (Chapters.Current.IsIndex)) then begin
      if ((IndexColumns > 1) and (H1Mode in [1, 3])) then
        writeln (x, '.MC ', indcolwidth:0:3, 'p 36p');
        writeln (x, '.INDEXSTOP');
        writeln (x, '.LP');
        writeln (x, '.ta ', indcolwidth:0:3, 'pR');
        writeln (x, '.tc .');
    end else if ((Columns > 1) and (H1Mode in [1, 3])) then
      writeln (x, '.MC ', colwidth:0:3, 'p 36p');

    writeln (x, '.EH ' + ExpandHeader (EvenHeader));
    writeln (x, '.OH ' + ExpandHeader (OddHeader));
    writeln (x, '.EF ' + ExpandHeader (EvenFooter));
    writeln (x, '.OF ' + ExpandHeader (OddFooter));

    writeln (x, '.ps ', GroffFonts [0].Size);
    writeln (x, '.so ', cfilename + '.so');

    if (FileExists (pathname + '/' + cfilename + '.tr')) then begin
      writeln (x, '.LP');
      writeln (x, '.SEP');
      writeln (x, '.LP');
      writeln (x, '.H3 "' + aStory.TrailerHeader + '"');
      writeln (x, '.so ' + cfilename + '.tr');
    end;

    if (FileExists (pathname + '/' + cfilename + '.an')) then begin
      writeln (x, '.LP');
      writeln (x, '.SEP');
      writeln (x, '.LP');
      writeln (x, '.H3 "Author''s Notes"');
      writeln (x, '.so ' + cfilename + '.an');
    end;

    if (FileExists (pathname + '/' + cfilename + '.om')) then begin
      writeln (x, '.LP');
      writeln (x, '.SEP');
      writeln (x, '.LP');
      writeln (x, '.H3 "' + aStory.OmakeHeader + '"');
      writeln (x, '.so ' + cfilename + '.om');
    end;
  end;

  if (CrossReference OR UseIndex) then
    writeln (X, '.XREFSTOP');
	writeln (x, '.close TOC');

  close (x);
  Chapters.Free;
end;

{$endregion}

{$region tHTMLProfile}

class function tHTMLProfile.ProfileType : integer;
begin
  ProfileType := ptHTML;
end;

constructor tHTMLProfile.Create;
begin
  Separator := '* * * * *';
  OutputDir := '';
  Usepreconv := false;
  UseHDTBL := false;
  {
  // Font 0 - Plain Text
  GroffFonts [0].Family := 6; // Times New Roman
  GroffFonts [0].Font := 0; // Regular
  GroffFonts [0].Size := 12;
  GroffFonts [0].Centered := false;
  GroffFonts [0].SpaceAbove := 0;
  GroffFonts [0].SpaceBelow := 0;
  GroffFonts [0].ReserveSpace := 0;
  GroffFonts [0].Borders [0] := FALSE;
  GroffFonts [0].Borders [1] := FALSE;
  GroffFonts [0].Borders [2] := FALSE;
  GroffFonts [0].Borders [3] := FALSE;

  // Font 1 - Title
  GroffFonts [1].Family := 3; // Helvetica
  GroffFonts [1].Font := 0; // Regular
  GroffFonts [1].Size := 28;
  GroffFonts [1].Centered := true;
  GroffFonts [1].SpaceAbove := 18;
  GroffFonts [1].SpaceBelow := 18;
  GroffFonts [1].ReserveSpace := 0;
  GroffFonts [1].Borders [0] := FALSE;
  GroffFonts [1].Borders [1] := FALSE;
  GroffFonts [1].Borders [2] := FALSE;
  GroffFonts [1].Borders [3] := FALSE;

  // Font 2 - Subtitle
  GroffFonts [2].Family := 3; // Helvetica
  GroffFonts [2].Font := 0; // Regular
  GroffFonts [2].Size := 18;
  GroffFonts [2].Centered := true;
  GroffFonts [2].SpaceAbove := 18;
  GroffFonts [2].SpaceBelow := 18;
  GroffFonts [2].ReserveSpace := 0;
  GroffFonts [2].Borders [0] := FALSE;
  GroffFonts [2].Borders [1] := FALSE;
  GroffFonts [2].Borders [2] := FALSE;
  GroffFonts [2].Borders [3] := FALSE;

  // Font 3 - Heading 0
  GroffFonts [3].Family := 3; // Helvetica
  GroffFonts [3].Font := 1; // Bold
  GroffFonts [3].Size := 28;
  GroffFonts [3].Centered := true;
  GroffFonts [3].SpaceAbove := 18;
  GroffFonts [3].SpaceBelow := 18;
  GroffFonts [3].ReserveSpace := 0;
  GroffFonts [3].Borders [0] := FALSE;
  GroffFonts [3].Borders [1] := FALSE;
  GroffFonts [3].Borders [2] := FALSE;
  GroffFonts [3].Borders [3] := FALSE;

  // Font 4 - Heading 1
  GroffFonts [4].Family := 3; // Helvetica
  GroffFonts [4].Font := 1; // Bold
  GroffFonts [4].Size := 18;
  GroffFonts [4].Centered := true;
  GroffFonts [4].SpaceAbove := 0;
  GroffFonts [4].SpaceBelow := 18;
  GroffFonts [4].ReserveSpace := 0;
  GroffFonts [4].Borders [0] := FALSE;
  GroffFonts [4].Borders [1] := FALSE;
  GroffFonts [4].Borders [2] := FALSE;
  GroffFonts [4].Borders [3] := FALSE;

  // Font 5 - Heading 2
  GroffFonts [5].Family := 3; // Helvetica
  GroffFonts [5].Font := 1; // Bold
  GroffFonts [5].Size := 14;
  GroffFonts [5].Centered := true;
  GroffFonts [5].SpaceAbove := 0;
  GroffFonts [5].SpaceBelow := 0;
  GroffFonts [5].ReserveSpace := 0;
  GroffFonts [5].Borders [0] := FALSE;
  GroffFonts [5].Borders [1] := FALSE;
  GroffFonts [5].Borders [2] := FALSE;
  GroffFonts [5].Borders [3] := FALSE;

  // Font 6 - Heading 3
  GroffFonts [6].Family := 3; // Helvetica
  GroffFonts [6].Font := 1; // Bold
  GroffFonts [6].Size := 12;
  GroffFonts [6].Centered := true;
  GroffFonts [6].SpaceAbove := 0;
  GroffFonts [6].SpaceBelow := 0;
  GroffFonts [6].ReserveSpace := 0;
  GroffFonts [6].Borders [0] := FALSE;
  GroffFonts [6].Borders [1] := FALSE;
  GroffFonts [6].Borders [2] := FALSE;
  GroffFonts [6].Borders [3] := FALSE;

  // Font 7 - Heading 4
  GroffFonts [7].Family := 3; // Helvetica
  GroffFonts [7].Font := 2; // Italics
  GroffFonts [7].Size := 12;
  GroffFonts [7].Centered := false;
  GroffFonts [7].SpaceAbove := 0;
  GroffFonts [7].SpaceBelow := 0;
  GroffFonts [7].ReserveSpace := 0;
  GroffFonts [7].Borders [0] := FALSE;
  GroffFonts [7].Borders [1] := FALSE;
  GroffFonts [7].Borders [2] := FALSE;
  GroffFonts [7].Borders [3] := FALSE;

  // Font 8 - Heading 5
  GroffFonts [8].Family := 6; // Times New Roman
  GroffFonts [8].Font := 1; // Bold
  GroffFonts [8].Size := 12;
  GroffFonts [8].Centered := false;
  GroffFonts [8].SpaceAbove := 0;
  GroffFonts [8].SpaceBelow := 0;
  GroffFonts [8].ReserveSpace := 0;
  GroffFonts [8].Borders [0] := FALSE;
  GroffFonts [8].Borders [1] := FALSE;
  GroffFonts [8].Borders [2] := FALSE;
  GroffFonts [8].Borders [3] := FALSE;

  // Font 9-11 - Custom.  Defaults to Plain Text.
  GroffFonts [9] := GroffFonts [0];
  GroffFonts [10] := GroffFonts [0];
  GroffFonts [11] := GroffFonts [0];
  }

  BulkHTML := false;
  DisclaimerInIndex := false;
  DisclaimerInChapter := false;
  IsFFNet := false;
  BookFilesInIndex := false;
  SingleLineBooks := false;
  SeparateBookFiles := false;
  BackLink := 'index.htm';
  BackLinkText := 'Return to Index Page';
end;

procedure tHTMLProfile.SetFlag (index : integer; aflag : boolean);
begin
  t_flags [index] := aflag;
end;

function tHTMLProfile.GetFlag (index : integer) : boolean;
begin
	GetFlag := t_flags [index];
end;

procedure tHTMLProfile.SetString (index : integer; astring : string);
begin
	t_strings [index] := aString;
end;

function tHTMLProfile.GetString (index : integer) : string;
begin
	GetString := t_strings [index];
end;

procedure tHTMLProfile.Load (var t : text);
var
  k,
  v,
  //k1,
  //v1,
  s : string;
  //font : integer = -1;
  i : integer;
  Done : boolean;
begin
  Done := false;
  repeat
    // Read Keyline
    readln (t, s);
    if (length (s) > 0) then
    	Trim (s);

    if (s = '[end]') then
      Done := true
    else begin
      // Find the = sign
      i := 0;
      repeat
        inc (i);
      until ((copy (s, i, 1) = '=') or (i > length (s)));

      // Split Keyline
      k := copy (s, 1, i - 1);
      v := copy (s, i + 1, length (s) - i);
      if (length (k) > 0) then
      	Trim (k);
      if (length (v) > 0) then
    	  Trim (v);

      // Select Variable
      if (k = 'Name') then
        Name := v
      else if (k = 'Output Directory') then
        OutputDir := v
      else if (k = 'Separator') then
        Separator := v

      // Font related stuff
      else if (copy (k, 1, 10) = 'Groff Font') then begin
        {
        k1 := copy (k, 12, 2);
        val (k1, font);

        with (GroffFonts [font]) do begin
          if (copy (v, length (v) - 8, 9) = ',Centered') then begin
            Centered := true;
            v := copy (v, 1, length (v) - 9);
          end else
            Centered := false;

          j := 0;
          while (copy (v, 1, length (FontFamilyNames [j]))
            <> FontFamilyNames [j]) and (j <= 6) do
            j += 1;
          Family := j;

          v1 := copy (v, length (FontFamilyNames [j]) + 2,
            length (v) - (length (FontFamilyNames [j]) + 1));

          j := 0;
          while (copy (v1, 1, length (FontNames [j])) <> FontNames [j])
            and (j <= 3) do
            j += 1;
          Font := j;

          v1 := copy (v1, length (FontNames [j]) + 2,
            length (v1) - (length (FontNames [j]) + 1));

          val (v1, j);
          Size := j;
        end
        }
      end

      // HTML stuff
      else if (k = 'Bulk HTML') then
        BulkHTML := true
      else if (k = 'Disclaimer In Index') then
        DisclaimerInIndex := true
      else if (k = 'Disclaimer In Chapter') then
        DisclaimerInChapter := true
      else if (k = 'Is Fanfiction.net Compliant') then
        IsFFNet := true
      else if (k = 'Book Files In Index') then
        BookFilesInIndex := true
      else if (k = 'Single Line Books') then
        SingleLineBooks := true
      else if (k = 'Separate Book Files') then
        SeparateBookFiles := true
      else if (k = 'Back Link') then
        BackLink := v
      else if (k = 'Back Link Text') then
        BackLinkText := v

      // Groff stuff
      else if (k = 'Call preconv') then
        Usepreconv := true
      else if (k = 'Call HDTBL') then
        UseHDTBL := true
    end;
  until Done;
end;

procedure tHTMLProfile.Save (var t : text);
{
var
  j,
  k,
  l : integer;
}
begin
  writeln (t, '[Profile HTML]');
  writeln (t, 'Name = ', Name);
  writeln (t, 'Output Directory = ', OutputDir);
  writeln (t, 'Separator = ', Separator);
  {
  for j := 0 to 11 do begin
    write (t, 'Groff Font ', j:2, ' = ');
    write (t, FontFamilyNames [GroffFonts [j].Family], ',');
    write (t, FontNames [GroffFonts [j].Font], ',');
    write (t, GroffFonts [j].Size);
    if (GroffFonts [j].Centered) then
      write (t, ',Centered');
    writeln (t);
    writeln (t, 'Space Above = ', GroffFonts [j].SpaceAbove);
    writeln (t, 'Space Below = ', GroffFonts [j].SpaceBelow);
    writeln (t, 'Reserve Space = ', GroffFonts [j].ReserveSpace);
    l := 0;
    for k := 0 to 3 do
      if (GroffFonts [j].Borders [k]) then
        l += 1 shl k;
    writeln (t, 'Borders = ', l);
  end;
  }
  if (BulkHTML) then
    writeln (t, 'Bulk HTML');
  if (DisclaimerInIndex) then
    writeln (t, 'Disclaimer In Index');
  if (DisclaimerInChapter) then
    writeln (t, 'Disclaimer In Chapter');
  if (IsFFNet) then
    writeln (t, 'Is Fanfiction.net Compliant');
  if (BookFilesInIndex) then
    writeln (t, 'Book Files In Index');
  if (SingleLineBooks) then
    writeln (t, 'Single Line Books');
  if (SeparateBookFiles) then
    writeln (t, 'Separate Book Files');
  if (BackLink <> '') then
    writeln (t, 'Back Link = ', BackLink);
  if (BackLinkText <> '') then
    writeln (t, 'Back Link Text = ', BackLinkText);

  // Groff Options
  if (Usepreconv) then
    writeln (t, 'Call preconv');
  if (UseHDTBL) then
    writeln (t, 'Call HDTBL');

  writeln (t, '[end]');
  writeln (t);
end;

procedure tHTMLProfile.Edit;
var
  Dialog : tfrmHTMLProfile;
begin
  Dialog := TfrmHTMLProfile.Create (Application);
  TfrmHTMLProfile (Dialog).Profile := self;
  Dialog.ShowModal;
  Dialog.Destroy;
end;

function tHTMLProfile.Duplicate : tHTMLProfile;
var
  dup : tHTMLProfile;
begin
  dup := tHTMLProfile.Create;
  dup.Name := Name;
  dup.OutputDir := OutputDir;
  dup.UsePreconv := UsePreconv;
  dup.Separator := Separator;

  dup.BulkHTML := BulkHTML;
  dup.DisclaimerInIndex := DisclaimerInIndex;
  dup.DisclaimerInChapter := DisclaimerInChapter;
  dup.BookFilesInIndex := BookFilesInIndex;
  dup.SingleLineBooks := SingleLineBooks;
  dup.SeparateBookFiles := SeparateBookFiles;
  dup.IsFFNet := IsFFNet;
  dup.BackLink := BackLink;
  dup.BackLinkText := BackLinkText;

  Duplicate := dup;
end;

procedure tHTMLProfile.WriteTopSo (var x : text);
begin
  if (UseHDTBL) then
    writeln (x, '.mso hdtbl.tmac');
  writeln (x, '.hlm 0');
  writeln (x, '.HTML <body style="font-family: verdana, sans-serif">');
  writeln (x, '.de H0');
  writeln (x, '.LP');
  writeln (x, '.HTML <font color="0000D0" size=+4>');
  writeln (x, '\\$1');
  writeln (x, '.HTML </font>');
  writeln (x, '..');

  { Header One }
  writeln (x, '.de H1');
  writeln (x, '.LP');
  writeln (x, '.HTML <font color="0000D0" size=+3>');
  writeln (x, '\\$1');
  writeln (x, '.HTML </font>');
  writeln (x, '.sp 0.15i');
  writeln (x, '..');

  { Header Two }
  writeln (x, '.de H2');
  writeln (x, '.LP');
  writeln (x, '.ne 1i');
  writeln (x, '.sp 0.1i');
  writeln (x, '.HTML <font color="0000D0" size=+2>');
  writeln (x, '\\$1');
  writeln (x, '.HTML </font>');
  writeln (x, '..');

  { Header Three }
  writeln (x, '.de H3');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp 0.1i');
  writeln (x, '.HTML <font color="0000D0" size=+1>');
  writeln (x, '\\$1');
  writeln (x, '.HTML </font>');
  writeln (x, '..');

  { Header Four }
  writeln (x, '.de H4');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp 0.1i');
  writeln (x, '.HTML <font color="0000D0">');
  writeln (x, '\\$1');
  writeln (x, '.HTML </font>');
  writeln (x, '..');

  { Header Five }
  writeln (x, '.de H5');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp 0.1i');
  writeln (x, '.HTML <font color="0000D0">');
  writeln (x, '\\$1');
  writeln (x, '.HTML </font>');
  writeln (x, '..');

  { Custom One }
  writeln (x, '.de CP1');
  writeln (x, '..');

  { Custom Two }
  writeln (x, '.de CP2');
  writeln (x, '..');

  { Custom Three }
  writeln (x, '.de CP3');
  writeln (x, '..');

  { Separator }
  writeln (x, '.de SEP');
  writeln (x, '.LP');
  writeln (x, '.ce');
  if (IsFFNet) then
    writeln (x, '.HTML "<HR>"')
  else
  	writeln (x, Separator);
  writeln (x, '..');

  // Primitive support for JPEG
  writeln (x, '.de JPEG');
  writeln (x, '.sy cp \\$1 ', OutputDir, '/\\$1');
  writeln (x, '.HTML <IMG SRC="\\$1" NAME="\\$1" WIDTH=50% ALIGN=RIGHT>');
  writeln (x, '..');
end;

procedure tHTMLProfile.WriteBulk (aStory : tStory);
var
  index : integer;
  s,
  pathname,
  filename,
  cfilename,
  ofilename: string;
  x : text;
  Chapters : tChapterList;
begin
  // Mark the Log
  s := ' -> Creating Bulk HTML for Profile ''' + Name + '''';
  frmLog.txtLog.Lines.Add (s);

  Chapters := tChapterList.Create;
  Chapters.BaseDir := aStory.SourceDir;
  Chapters.Load;

  pathname := aStory.SourceDir + '/';
  ofilename := StripSpaces (aStory.LongName + '-' + Name);
  filename := pathname + '/' + ofilename + '.ms';

  assign (x, filename);
  rewrite (x);

  WriteTopSo (x);

  if ((aStory.Subtitle <> '') and (aStory.SubtitleFirst)) then begin
  	writeln (x, '.HTML <font color="0000D0" size=+3>');
  	writeln (x, '.sp 0.5i');
  	writeln (x, aStory.Subtitle);
    writeln (x, '.HTML </font>');
  end;

  writeln (x, '.ad b');
  writeln (x, '.sp 0.2i');
  writeln (x, '.HTML <font color="0000D0" size=+4>');
  writeln (x, '.ce');
  writeln (x, aStory.Title);
  writeln (x, '.HTML </font>');

  if ((aStory.Subtitle <> '') and not (aStory.SubtitleFirst)) then begin
  	writeln (x, '.HTML <font color="0000D0" size=+3>');
  	writeln (x, '.sp 0.5i');
  	writeln (x, aStory.Subtitle);
    writeln (x, '.HTML </font>');
  end;

  if not (aStory.SuppressAuthor) then begin
    writeln (x, '.HTML <font color="0000D0" size=+3>');
    writeln (x, '.sp 0.5i');
    writeln (x, 'by ', aStory.Author);
    writeln (x, '.HTML </font>');
  end;

  if (FileExists (pathname + 'blurb.so')) then begin
  	writeln (x, '.so blurb.so');
  	writeln (x, '.PP');
  end;

  if (FileExists (pathname + 'disclaimer.so')) then begin
  	writeln (x, '.so disclaimer.so');
  end;

  if (FileExists (pathname + 'credits.so')) then begin
  	writeln (x, '.LP');
  	writeln (x, '.so credits.so');
  end;

  writeln (x, '.sp 0.5i');
  writeln (x, '.PP');

  // Now write the chapters

  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);

    writeln (x, '.LP');
    writeln (x, '.SEP');

    with (Chapters.Current) do begin
      cfilename := Filename;
      if (FileExists (pathname + '/' + cfilename + '.co')) then begin
        writeln (x, '.so ' + cfilename + '.co');
      end;
      if not (SuppressTitle) then begin
        if ((SubtitleFirst) and (Subtitle <> '')) then
          writeln (x, '.H3 "', Subtitle, '"');
        writeln (x, '.H1 "', Title, '"');
        if (not (SubtitleFirst) and (Subtitle <> '')) then
          writeln (x, '.H3 "', Subtitle, '"');
      end;
      writeln (x, '.so ', cfilename + '.so');

      if (FileExists (pathname + '/' + cfilename + '.tr')) then begin
        writeln (x, '.LP');
        writeln (x, '.SEP');
        writeln (x, '.LP');
        writeln (x, '.H3 "' + aStory.TrailerHeader + '"');
        writeln (x, '.so ' + cfilename + '.tr');
      end;

      if (FileExists (pathname + '/' + cfilename + '.an')) then begin
        writeln (x, '.LP');
        writeln (x, '.SEP');
        writeln (x, '.LP');
        writeln (x, '.H3 "Author''s Notes"');
        writeln (x, '.so ' + cfilename + '.an');
      end;

      if (FileExists (pathname + '/' + cfilename + '.om')) then begin
        writeln (x, '.LP');
        writeln (x, '.SEP');
        writeln (x, '.LP');
        writeln (x, '.H3 "' + aStory.OmakeHeader + '"');
        writeln (x, '.so ' + cfilename + '.om');
      end;
    end;
  end;
  close (x);
  Chapters.Free;
end;

procedure tHTMLProfile.WriteChapters (aStory : tStory);
var
  index : integer;
  s,
  pathname,
  filename,
  cfilename,
  ofilename: string;
  x : text;
  Chapters : tChapterList;
begin
  // Mark the Log
  s := ' -> Creating HTML Chapters for Profile ''' + Name + '''';
  frmLog.txtLog.Lines.Add (s);

  Chapters := tChapterList.Create;
  Chapters.BaseDir := aStory.SourceDir;
  Chapters.Load;

  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);

    pathname := aStory.SourceDir + '/';
    ofilename := StripSpaces (Chapters.Current.FileName);
    filename := pathname + ofilename + '-' + StripSpaces (Name) + '.ms';

    // Start creating the output file
    assign (x, filename);
    rewrite (x);

    WriteTopSo (x);
    writeln (x, '.de LINKS');
    writeln (x, '.HTML <table style="border:1px 0 1px 0; width=100%">');
    writeln (x, '.HTML <tr>');
    writeln (x, '.HTML <td style="text-align:left">&href="\\$2">\\$1</a></td>');
    writeln (x, '.HTML <td style="text-align:center">&href="', BackLink,
      '">Index</a></td>');
    writeln (x, '.HTML <td style="text-align:right">&href="\\$4">\\$3</a></td>');
    writeln (x, '.HTML </tr>');
    writeln (x, '.HTML </table>');
    writeln (x, '..');

	  writeln (x, '.ad b');

    if ((aStory.Subtitle <> '') and (aStory.SubtitleFirst)) then begin
    	writeln (x, '.HTML <font color="0000D0" size=+2>');
    	writeln (x, '.sp 0.5i');
    	writeln (x, aStory.Subtitle);
      writeln (x, '.HTML </font>');
    end;

	  writeln (x, '.sp 0.2i');
	  writeln (x, '.HTML <font color="0000D0" size=+3>');
	  writeln (x, aStory.Title);
		writeln (x, '.HTML </font>');

    if ((aStory.Subtitle <> '') and not (aStory.SubtitleFirst)) then begin
	    writeln (x, '.HTML <font color="0000D0" size=+2>');
	    writeln (x, '.sp 0.5i');
	    writeln (x, aStory.Subtitle);
  		writeln (x, '.HTML </font>');
    end;

	  writeln (x, '.HTML <font color="0000D0" size=+2>');
	  writeln (x, '.sp 0.5i');
	  writeln (x, 'by ', aStory.Author);
		writeln (x, '.HTML </font>');

	  writeln (x, '.sp 0.5i');
	  writeln (x, '.PP');

    if (FileExists (pathname + 'blurb.so')) then begin
	    writeln (x, '.so blurb.so');
	    writeln (x, '.PP');
    end;

    if (FileExists (pathname + 'disclaimer.so')) then begin
	    writeln (x, '.so disclaimer.so');
    end;

	  if (FileExists (pathname + 'credits.so')) then begin
			writeln (x, '.LP');
			writeln (x, '.so credits.so');
		end;

    writeln (x, '.LP');
    writeln (x, '.SEP');

    with (Chapters.Current) do begin
      cfilename := Filename;
      if (FileExists (pathname + '/' + cfilename + '.co')) then begin
        writeln (x, '.so ' + cfilename + '.co');
      end;

      if not (SuppressTitle) then begin
        if ((SubtitleFirst) and (Subtitle <> '')) then
          writeln (x, '.H3 "', Subtitle, '"');
        writeln (x, '.H1 "', Title, '"');
        if (not (SubtitleFirst) and (Subtitle <> '')) then
          writeln (x, '.H3 "', Subtitle, '"');
      end;
      writeln (x, '.so ', cfilename + '.so');

      if (FileExists (pathname + '/' + cfilename + '.tr')) then begin
        writeln (x, '.LP');
        writeln (x, '.SEP');
        writeln (x, '.LP');
        writeln (x, '.H3 "' + aStory.TrailerHeader + '"');
        writeln (x, '.so ' + cfilename + '.tr');
      end;

      if (FileExists (pathname + '/' + cfilename + '.an')) then begin
        writeln (x, '.LP');
        writeln (x, '.SEP');
        writeln (x, '.LP');
        writeln (x, '.H3 "Author''s Notes"');
        writeln (x, '.so ' + cfilename + '.an');
      end;

      if (FileExists (pathname + '/' + cfilename + '.om')) then begin
        writeln (x, '.LP');
        writeln (x, '.SEP');
        writeln (x, '.LP');
        writeln (x, '.H3 "' + aStory.OmakeHeader + '"');
        writeln (x, '.so ' + cfilename + '.om');
      end;
    end;
    close (x);
  end;
  Chapters.Free;
end;

procedure tHTMLProfile.WriteIndex (aStory : tStory);
var
  index : integer;
  s,
  pathname,
  filename,
  cfilename,
  ofilename: string;
  indexfile : text;
  LastWasBook : boolean;
  Chapters : tChapterList;
begin
  // Mark the Log
  s := ' -> Creating HTML Index for Profile ''' + Name + '''';
  frmLog.txtLog.Lines.Add (s);

  Chapters := tChapterList.Create;
  Chapters.BaseDir := aStory.SourceDir;
  Chapters.Load;

  pathname := aStory.SourceDir + '/';
  ofilename := StripSpaces (aStory.LongName + '-' + Name);
  filename := pathname + '/' + ofilename + '.ms';

  assign (indexfile, filename);
  rewrite (indexfile);
  WriteTopSo (indexfile);

  // Title Pic
  cfilename := aStory.TitlePicture;
  if (FileExists (cfilename)) then begin
  	writeln (indexfile, '.sy cp ', cfilename, ' ', cfilename);
  	writeln (indexfile, '.HTML <p><IMG SRC="', cfilename, '" NAME="',
      cfilename, '" WIDTH=50% ALIGN=CENTER></p>')
  end else
  	writeln (indexfile, '.H1 "', aStory.Title, '"');

  if (FileExists (pathname + 'blurb.so')) then begin
    writeln (indexfile, '.PP');
    writeln (indexfile, '.so blurb.so');
  end;
  if ((DisclaimerInIndex) and (FileExists (pathname + 'disclaimer.so'))) then begin
    writeln (indexfile, '.PP');
  	writeln (indexfile, '.so disclaimer.so');
  end;
  writeln (indexfile, '.HR');

  // Now write the chapters
  LastWasBook := FALSE;

  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);
    if (not (SingleLineBooks)) then
  		  writeln (indexfile, '.LP');

    with (Chapters.Current) do begin
      cfilename := aStory.LongName + '/' + Filename + '.htm';
      if (IsABook) then begin
        if (not (SingleLineBooks)) then
  			  writeln (indexfile);
  		  writeln (indexfile, '.H2 "', Title, '"');
  		  if ((BookFilesInIndex) and
          (FileExists (pathname + Filename + '.so'))) then begin
          writeln (indexfile, '.LP');
  			  writeln (indexfile, '.so ', Filename + '.so');
  		  end
      end else begin
  			if ((not (LastWasBook)) AND (SingleLineBooks)) then
  				writeln (indexfile, ' ,');
  			write (indexfile, '.URL ', aStory.LongName, '/', Filename,
          '.htm "', Title, '"');
  			if (not (SingleLineBooks)) then
  				writeln (indexfile);
  		end;
  	  LastWasBook := IsABook;
  	  writeln (indexfile);
    end
  end;

  if (FileExists(pathname + 'credits.so')) then begin
    writeln (indexfile, '.HR');
    writeln (indexfile, '.so credits.so');
  end;
  writeln (indexfile, '.HR');
  writeln (indexfile, '.URL ', BackLink, ' "', BackLinkText, '"');
  close (indexfile);
end;

procedure tHTMLProfile.Build (aStory : tStory);
begin
  if (BulkHTML) then
    WriteBulk (aStory)
  else begin
    WriteChapters (aStory);
    WriteIndex (aStory)
  end
end;

{$endregion}

{$region tTextProfile}

class function tTextProfile.ProfileType : integer;
begin
  ProfileType := ptText;
end;

constructor tTextProfile.Create;
begin
  Separator := '* * * * *';
  OutputDir := '';
  Usepreconv := false;
  Usetbl := false;
  UseHDTBL := false;

  BulkText := FALSE;
  FFMLCompliant := FALSE;
end;

procedure tTextProfile.SetFlag (index : integer; aflag : boolean);
begin
  t_flags [index] := aflag;
end;

function tTextProfile.GetFlag (index : integer) : boolean;
begin
	GetFlag := t_flags [index];
end;

procedure tTextProfile.Load (var t : text);
var
  k,
  v,
  s : string;
  i : integer;
  Done : boolean;
begin
  Done := false;
  repeat
    // Read Keyline
    readln (t, s);
    if (length (s) > 0) then
    	Trim (s);

    if (s = '[end]') then
      Done := true
    else begin
      // Find the = sign
      i := 0;
      repeat
        inc (i);
      until ((copy (s, i, 1) = '=') or (i > length (s)));

      // Split Keyline
      k := copy (s, 1, i - 1);
      v := copy (s, i + 1, length (s) - i);
      if (length (k) > 0) then
      	Trim (k);
      if (length (v) > 0) then
    	  Trim (v);

      // Select Variable
      if (k = 'Name') then
        Name := v
      else if (k = 'Output Directory') then
        OutputDir := v
      else if (k = 'Separator') then
        Separator := v

      // Text Stuff
      else if (k = 'Bulk Text') then
        BulkText := true
      else if (k = 'FFML Compliant') then
        FFMLCompliant := true

      // Groff stuff
      else if (k = 'Call preconv') then
        Usepreconv := true
      else if (k = 'Call tbl') then
        Usetbl := true
      else if (k = 'Call HDTBL') then
        UseHDTBL := true

    end;
  until Done;
end;

procedure tTextProfile.Save (var t : text);
begin
  writeln (t, '[Profile Text]');
  writeln (t, 'Name = ', Name);
  writeln (t, 'Output Directory = ', OutputDir);
  writeln (t, 'Separator = ', Separator);

  if (BulkText) then
    writeln (t, 'Bulk Text');
  If (FFMLCompliant) then
    writeln (t, 'FFML Compliant');

  // Groff Options
  if (Usepreconv) then
    writeln (t, 'Call preconv');
  if (usetbl) then
    writeln (t, 'Call tbl');
  if (UseHDTBL) then
    writeln (t, 'Call HDTBL');

  writeln (t, '[end]');
  writeln (t);
end;

procedure tTextProfile.Edit;
var
  Dialog : tfrmTextProfile;
begin
  Dialog := TfrmTextProfile.Create (Application);
  Dialog.Profile := self;
  Dialog.ShowModal;
  Dialog.Destroy;
end;

function tTextProfile.Duplicate : tTextProfile;
var
  dup : tTextProfile;
begin
  dup := tTextProfile.Create;
  dup.Name := Name;
  dup.OutputDir := OutputDir;
  dup.UsePreconv := UsePreconv;
  dup.Separator := Separator;

  dup.BulkText := BulkText;
  dup.FFMLCompliant := FFMLCompliant;
  dup.UseTBL := UseTBL;

  Duplicate := dup;
end;

procedure tTextProfile.WriteBulk (aStory : tStory);
var
  index : integer;
  s,
  pathname,
  filename,
  cfilename,
  tfilename,
  ofilename: string;
  x : text;
  linelength,
  colwidth : real;
  UsesBooks : boolean = false;
  Chapters : tChapterList;
begin
  // Mark the Log
  s := ' -> Creating Bulk Text for Profile ''' + Name + '''';
  frmLog.txtLog.Lines.Add (s);

  Chapters := tChapterList.Create;
  Chapters.BaseDir := aStory.SourceDir;
  Chapters.Load;

  // Check to see if any chapter files are book dividers
  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);
    if (Chapters.Current.IsABook) then
      UsesBooks := true;
  end;

  pathname := aStory.SourceDir + '/';
  ofilename := StripSpaces (aStory.LongName + '-' + Name);
  filename := pathname + '/' + ofilename + '.ms';

  // Sizes are expressed in points
  linelength := 540;
  colwidth := linelength;

  assign (x, filename);
  rewrite (x);

  // HDTBL
  if (UseHDTBL) then
    writeln (x, '.mso hdtbl.tmac');
  // Page Dimensions
  writeln (x, '.hlm 0');
  writeln (x, '.open TOC ', ofilename, '.toc');
  writeln (x, '.write TOC .LP');
  writeln (x, '.write TOC .ta ', colwidth:0:3, 'pR');
  writeln (x, '.write TOC .tc .');
  if (FFMLCompliant) then begin
    writeln (x, '.nr PO 0p');
    writeln (x, '.po 0p');
    writeln (x, '.hm 0p');
    writeln (x, '.fm 0p');
    writeln (x, '.nr HM 0p');
    writeln (x, '.nr FM 0p');
    writeln (x, '.EH ****');
    writeln (x, '.OH ****');
    writeln (x, '.EF ****');
    writeln (x, '.OF ****')
  end else begin
    writeln (x, '.po 36p');
    writeln (x, '.nr PO 36p');
    writeln (x, '.hm 36p');
    writeln (x, '.fm 36p');
    writeln (x, '.nr HM 36p');
    writeln (x, '.nr FM 36p');
    writeln (x, '.EH *', aStory.Title, '***');
    writeln (x, '.OH ***', aStory.Author, '*');
    writeln (x, '.EF **%**');
    writeln (x, '.OF **%**');
  end;
  writeln (x, '.ll 540p');
  writeln (x, '.pl 792p');
  writeln (x, '.nr LL 540p');
  writeln (x, '.nr PL 792p');

  { Anti-Formatting }
  writeln (x, '.de B');
  writeln (x, '..');
  writeln (x, '.de I');
  writeln (x, '..');
  writeln (x, '.de BI');
  writeln (x, '..');

  { Cross-Reference support }
  writeln (x, '.so ', ofilename, '.ref');
  writeln (x, '.de XREFSTART');
  writeln (x, '.open XREFS \\$1');
  writeln (x, '.write XREFS ".de XREF');
  writeln (x, '..');
  writeln (x, '.de XREFSTOP');
  writeln (x, '.write XREFS "..');
  writeln (x, '.close XREFS');
  writeln (x, '..');
  writeln (x, '.de BOOKMARK');
  writeln (x, '.write XREFS .if ''\\\\\\\\$1''\\$1'' \\n[%]\\\\\\\\$2');
  writeln (x, '..');
  writeln (x, '.XREFSTART ', ofilename, '.ref');

  { Header Zero }
  if (UsesBooks) then
  	begin
  		writeln (x, '.de H0');
  		writeln (x, '.LP');
  		writeln (x, '.sp 0.5i');
  		writeln (x, '\\$1');
  		writeln (x, '.write TOC .br');
  		writeln (x, '.ie !''\\$2'''' \');
  		writeln (x, '.write TOC \\$1:  \\$2', #9, '\\n[%]');
  		writeln (x, '.el \');
  		writeln (x, '.write TOC \\$1', #9, '\\n[%]');
  		writeln (x, '.LP');
  		writeln (x, '.sp 0.5i');
  		writeln (x, '.ps 28');
  		writeln (x, '.ce');
  		writeln (x, '\\$2');
  		writeln (x, '..');
  	end;

  { Header One }
  writeln (x, '.de H1');
  writeln (x, '.LP');
  writeln (x, '.ne 1i');
  writeln (x, '.sp 0.2i');
  writeln (x, '\\$1');
  writeln (x, '.sp 0.15i');
  writeln (x, '.write TOC .br');
  if (UsesBooks) then
  	writeln (x, '.write TOC \ \ \ \ \\$1', #9, '\\n[%]')
  else
  	writeln (x, '.write TOC \\$1', #9, '\\n[%]');
  writeln (x, '..');

  { Header Two }
  writeln (x, '.de H2');
  writeln (x, '.LP');
  writeln (x, '.ne 1i');
  writeln (x, '.sp 0.1i');
  writeln (x, '\\$1');
  writeln (x, '.write TOC .br');
  if (UsesBooks) then
  	writeln (x, '.write TOC \ \ \ \ \ \ \ \ \\$1', #9, '\\n[%]')
  else
  	writeln (x, '.write TOC \ \ \ \ \\$1', #9, '\\n[%]');
  writeln (x, '..');

  { Header Three }
  writeln (x, '.de H3');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp 0.1i');
  writeln (x, '\\$1');
  writeln (x, '..');

  { Header Four }
  writeln (x, '.de H4');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp 0.1i');
  writeln (x, '\\$1');
  writeln (x, '..');

  { Header Five }
  writeln (x, '.de H5');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp 0.1i');
  writeln (x, '\\$1');
  writeln (x, '..');

  { Custom One }
  writeln (x, '.de CP1');
  writeln (x, '..');

  { Custom Two }
  writeln (x, '.de CP2');
  writeln (x, '..');

  { Custom Three }
  writeln (x, '.de CP3');
  writeln (x, '..');

  { Separator }
  writeln (x, '.de SEP');
  writeln (x, '.LP');
  writeln (x, '.ce');
  writeln (x, Separator);
  writeln (x, '');
  writeln (x, '..');

  // No support for JPEG, but we add the request so it doesn't bork.
  writeln (x, '.de JPEG');
  writeln (x, '..');

  if ((aStory.Subtitle <> '') AND (aStory.SubtitleFirst)) then begin
    writeln (x, '.sp 0.5i');
    writeln (x, '.ce');
    writeln (x, aStory.Subtitle);
  end;

  writeln (x, '.ad b');
  writeln (x, '.sp 0.2i');
  writeln (x, '.ce');
  writeln (x, aStory.Title);

  if ((aStory.Subtitle <> '') AND (not aStory.SubtitleFirst)) then begin
  	writeln (x, '.sp 0.5i');
  	writeln (x, '.ce');
  	writeln (x, aStory.Subtitle);
  end;

  writeln (x, '.sp 0.5i');
  writeln (x, '.ce');
  writeln (x, 'by ', aStory.Author);

  writeln (x, '.sp 0.5i');
  writeln (x, '.PP');
  tfilename := pathname + '/blurb.so';
  if (FileExists (tfilename)) then begin
  	writeln (x, '.so blurb.so');
  	writeln (x, '.PP');
  end;

  tfilename := pathname + '/disclaimer.so';
  if (FileExists (tfilename)) then begin
    writeln (x, '.B1');
  	writeln (x, '.so disclaimer.so');
  	writeln (x, '.B2');
  end;

  tfilename := pathname + '/credits.so';
  if (FileExists (tfilename)) then
  	begin
  		writeln (x, '.LP');
  		writeln (x, '.so credits.so');
  	end;

  writeln (x, '.LP');
  writeln (x, '.ne 9i');
  writeln (x, '.ce');
  writeln (x, 'Table of Contents');
  writeln (x, '.so ', ofilename, '.toc');

  // Now write the chapters

  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);

    writeln (x, '.LP');
    writeln (x, '.bp');

    cfilename := Chapters.Current.Filename;
    if (FileExists (pathname + '/' + cfilename + '.co')) then begin
      writeln (x, '.so ' + cfilename + '.co');
    end;

    if not (Chapters.Current.SuppressTitle) then begin
      if ((Chapters.Current.SubtitleFirst) and (Chapters.Current.Subtitle <> '')) then
        writeln (x, '.H3 "', Chapters.Current.Subtitle, '"');
      writeln (x, '.H1 "', Chapters.Current.Title, '"');
      if (not (Chapters.Current.SubtitleFirst) and (Chapters.Current.Subtitle <> '')) then
        writeln (x, '.H3 "', Chapters.Current.Subtitle, '"');
    end;
    writeln (x, '.so ', cfilename + '.so');

    if (FileExists (pathname + '/' + cfilename + '.tr')) then begin
      writeln (x, '.LP');
      writeln (x, '.SEP');
      writeln (x, '.LP');
      writeln (x, '.H3 "' + aStory.TrailerHeader + '"');
      writeln (x, '.so ' + cfilename + '.tr');
    end;

    if (FileExists (pathname + '/' + cfilename + '.om')) then begin
      writeln (x, '.LP');
      writeln (x, '.SEP');
      writeln (x, '.LP');
      writeln (x, '.H3 "' + aStory.OmakeHeader + '"');
      writeln (x, '.so ' + cfilename + '.om');
    end;

    if (FileExists (pathname + '/' + cfilename + '.an')) then begin
      writeln (x, '.LP');
      writeln (x, '.SEP');
      writeln (x, '.LP');
      writeln (x, '.H3 "Author''s Notes"');
      writeln (x, '.so ' + cfilename + '.an');
    end;
  end;
  writeln (X, '.XREFSTOP');
  writeln (x, '.close TOC');
  close (x);
end;

procedure tTextProfile.WriteChapters (aStory : tStory);
var
  index : integer;
  s,
  pathname,
  filename,
  cfilename,
  ofilename: string;
  x : text;
  UsesBooks : boolean = false;
  Chapters : tChapterList;
begin
  // Mark the Log
  s := ' -> Creating Text Chapters for Profile ''' + Name + '''';
  frmLog.txtLog.Lines.Add (s);

  Chapters := tChapterList.Create;
  Chapters.BaseDir := aStory.SourceDir;
  Chapters.Load;

  pathname := '  ->  Text Profile "' + Name + '"...';

  // Check to see if any chapter files are book dividers
  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);
    if (Chapters.Current.IsABook) then
      UsesBooks := true;
  end;

	for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);
    pathname := aStory.SourceDir + '/';
    ofilename := StripSpaces (Chapters.Current.Filename + '-' + Name);
    filename := pathname + '/' + ofilename + '.ms';
    cfilename := Chapters.Current.Filename;

    assign (x, filename);
    rewrite (x);

    // Page Dimensions
	  writeln (x, '.hlm 0');
	  writeln (x, '.ad l');
	  if (FFMLCompliant) then begin
    	writeln (x, '.nr PO 0p');
      writeln (x, '.po 0p');
  	  writeln (x, '.hm 0p');
  	  writeln (x, '.fm 0p');
  	  writeln (x, '.nr HM 0p');
  	  writeln (x, '.nr FM 0p');
  	  writeln (x, '.EH ****');
  	  writeln (x, '.OH ****');
  	  writeln (x, '.EF ****');
  	  writeln (x, '.OF ****')
    end else begin
      writeln (x, '.po 36p');
      writeln (x, '.nr PO 36p');
  	  writeln (x, '.hm 36p');
  	  writeln (x, '.fm 36p');
  	  writeln (x, '.nr HM 36p');
  	  writeln (x, '.nr FM 36p');
  	  writeln (x, '.EH *', aStory.Title, '***');
  	  writeln (x, '.OH ***', aStory.Author, '*');
  	  writeln (x, '.EF **%**');
  	  writeln (x, '.OF **%**');
    end;
		writeln (x, '.ll 540p');
	  writeln (x, '.pl 792p');
	  writeln (x, '.nr LL 540p');
	  writeln (x, '.nr PL 792p');

    { Anti-Formatting }
    writeln (x, '.de B');
    writeln (x, '..');
    writeln (x, '.de I');
    writeln (x, '..');
    writeln (x, '.de BI');
    writeln (x, '..');

	  { Header Zero }
	  if (UsesBooks) then
		  begin
			  writeln (x, '.de H0');
			  writeln (x, '.LP');
			  writeln (x, '.sp 0.5i');
			  writeln (x, '\\$1');
			  writeln (x, '.LP');
			  writeln (x, '.sp 0.5i');
			  writeln (x, '.ps 28');
			  writeln (x, '.ce');
			  writeln (x, '\\$2');
			  writeln (x, '..');
		  end;

	  { Header One }
	  writeln (x, '.de H1');
	  writeln (x, '.LP');
	  writeln (x, '.ne 1i');
	  writeln (x, '.sp 0.2i');
	  writeln (x, '\\$1');
	  writeln (x, '.sp 0.15i');
	  writeln (x, '..');

	  { Header Two }
	  writeln (x, '.de H2');
	  writeln (x, '.LP');
	  writeln (x, '.ne 1i');
	  writeln (x, '.sp 0.1i');
	  writeln (x, '\\$1');
	  writeln (x, '..');

	  { Header Three }
	  writeln (x, '.de H3');
	  writeln (x, '.LP');
	  writeln (x, '.ne 0.5i');
	  writeln (x, '.sp 0.1i');
	  writeln (x, '\\$1');
	  writeln (x, '..');

	  { Header Four }
	  writeln (x, '.de H4');
	  writeln (x, '.LP');
	  writeln (x, '.ne 0.5i');
	  writeln (x, '.sp 0.1i');
	  writeln (x, '\\$1');
	  writeln (x, '..');

	  { Header Five }
	  writeln (x, '.de H5');
	  writeln (x, '.LP');
	  writeln (x, '.ne 0.5i');
	  writeln (x, '.sp 0.1i');
	  writeln (x, '\\$1');
	  writeln (x, '..');

	  { Custom One }
	  writeln (x, '.de CP1');
	  writeln (x, '..');

	  { Custom Two }
	  writeln (x, '.de CP2');
	  writeln (x, '..');

	  { Custom Three }
	  writeln (x, '.de CP3');
	  writeln (x, '..');

	  { Separator }
	  writeln (x, '.de SEP');
	  writeln (x, '.LP');
	  writeln (x, '.ce');
	  writeln (x, Separator);
	  writeln (x, '');
	  writeln (x, '..');

    // No support for JPEG, but we add the request so it doesn't bork.
	  writeln (x, '.de JPEG');
	  writeln (x, '..');

	  writeln (x, '.ad b');
	  writeln (x, '.sp 0.2i');
		writeln (x, '.ce');
		writeln (x, aStory.Title);

    if (aStory.Subtitle <> '') then begin
	    writeln (x, '.sp 0.5i');
	    writeln (x, '.ce');
	    writeln (x, aStory.Subtitle);
    end;

	  writeln (x, '.sp 0.5i');
	  writeln (x, '.ce');
	  writeln (x, 'by ', aStory.Author);

	  writeln (x, '.sp 0.5i');

    {
    if (FileExists ('blurb.so')) then begin
	    writeln (x, '.so blurb.so');
	    writeln (x, '.PP');
    end;
    }
    if (FileExists (pathname + 'disclaimer.so')) then begin
  	  writeln (x, '.PP');
      writeln (x, '.B1');
	    writeln (x, '.so disclaimer.so');
	    writeln (x, '.B2');
    end;
    {
	  if (FileExists (pathname + 'credits.so')) then
		  begin
			  writeln (x, '.LP');
			  writeln (x, '.so credits.so');
		  end;
    }

    if (FileExists (pathname + '/' + cfilename + '.co')) then begin
      writeln (x, '.so ' + cfilename + '.co');
    end;

    if not (Chapters.Current.SuppressTitle) then begin
      if ((Chapters.Current.SubtitleFirst) and (Chapters.Current.Subtitle <> '')) then
        writeln (x, '.H3 "', Chapters.Current.Subtitle, '"');
      writeln (x, '.H1 "', Chapters.Current.Title, '"');
      if (not (Chapters.Current.SubtitleFirst) and (Chapters.Current.Subtitle <> '')) then
        writeln (x, '.H3 "', Chapters.Current.Subtitle, '"');
    end;
    writeln (x, '.so ', cfilename + '.so');

    if (FileExists (pathname + '/' + cfilename + '.tr')) then begin
      writeln (x, '.LP');
      writeln (x, '.SEP');
      writeln (x, '.LP');
      writeln (x, '.H3 "' + aStory.TrailerHeader + '"');
      writeln (x, '.so ' + cfilename + '.tr');
    end;

    if (FileExists (pathname + '/' + cfilename + '.om')) then begin
      writeln (x, '.LP');
      writeln (x, '.SEP');
      writeln (x, '.LP');
      writeln (x, '.H3 "' + aStory.OmakeHeader + '"');
      writeln (x, '.so ' + cfilename + '.om');
    end;

    if (FileExists (pathname + '/' + cfilename + '.an')) then begin
      writeln (x, '.LP');
      writeln (x, '.SEP');
      writeln (x, '.LP');
      writeln (x, '.H3 "Author''s Notes"');
      writeln (x, '.so ' + cfilename + '.an');
    end;

    close (x);
  end;
end;


procedure tTextProfile.Build (aStory : tStory);
begin
  if (BulkText) then
    WriteBulk (aStory)
  else
    WriteChapters (aStory);
end;

{$endregion}

{$region tEPubProfile}

class function tEPubProfile.ProfileType : integer;
begin
  ProfileType := ptEPub;
end;

constructor tEPubProfile.Create;
begin
  Separator := '* * * * *';
  OutputDir := '';
  Usepreconv := false;
  UseHDTBL := false;
  BlurbInEPub := false;
  EPubSeries := '';
end;

procedure tEPubProfile.Load (var t : text);
var
  k,
  v,
  s : string;
  i : integer;
  Done : boolean;
begin
  Done := false;
  repeat
    // Read Keyline
    readln (t, s);
    if (length (s) > 0) then
    	Trim (s);

    if (s = '[end]') then
      Done := true
    else begin
      // Find the = sign
      i := 0;
      repeat
        inc (i);
      until ((copy (s, i, 1) = '=') or (i > length (s)));

      // Split Keyline
      k := copy (s, 1, i - 1);
      v := copy (s, i + 1, length (s) - i);
      if (length (k) > 0) then
      	Trim (k);
      if (length (v) > 0) then
    	  Trim (v);

      // Select Variable
      if (k = 'Name') then
        Name := v
      else if (k = 'Output Directory') then
        OutputDir := v
      else if (k = 'Separator') then
        Separator := v

      // EPub stuff
      else if (k = 'Blurb Files In EPub') then
        BlurbInEPub := true
      else if (k = 'EPub Series') then
        EPubSeries := v

      // Groff stuff
      else if (k = 'Call preconv') then
        Usepreconv := true
      else if (k = 'Call HDTBL') then
        useHDTBL := true
    end;
  until Done;
end;

procedure tEPubProfile.Save (var t : text);
begin
  writeln (t, '[Profile EPub]');
  writeln (t, 'Name = ', Name);
  writeln (t, 'Output Directory = ', OutputDir);
  writeln (t, 'Separator = ', Separator);

  if (BlurbInEPub) then
    writeln (t, 'Blurb File In EPub');
  if (EPubSeries <> '') then
    writeln (t, 'EPub Series = ', EPubSeries);

  // Groff Options
  if (Usepreconv) then
    writeln (t, 'Call preconv');
  if (UseHDTBL) then
    writeln (t, 'Call HDTBL');
  writeln (t, '[end]');
  writeln (t);
end;

procedure tEPubProfile.Edit;
var
  Dialog : tfrmEPubProfile;
begin
  Dialog := TfrmEPubProfile.Create (Application);
  Dialog.Profile := self;
  Dialog.ShowModal;
  Dialog.Destroy;
end;

function tEPubProfile.Duplicate : tEPubProfile;
var
  dup : tEPubProfile;
begin
  dup := tEPubProfile.Create;
  dup.Name := Name;
  dup.OutputDir := OutputDir;
  dup.UsePreconv := UsePreconv;
  dup.Separator := Separator;

  dup.BlurbInEPub := BlurbInEPub;
  dup.EPubSeries := EPubSeries;

  Duplicate := dup;
end;

procedure tEPubProfile.WriteTopSo (var x : text);
begin
  if (UseHDTBL) then
    writeln (x, '.mso hdtbl.tmac');
  writeln (x, '.de H0');
  writeln (x, '.LP');
  writeln (x, '\fB\\$1\fR');
  writeln (x, '..');

  { Header One }
  writeln (x, '.de H1');
  writeln (x, '.LP');
  writeln (x, '\fB\\$1\fR');
  writeln (x, '.sp 0.15i');
  writeln (x, '..');

  { Header Two }
  writeln (x, '.de H2');
  writeln (x, '.LP');
  writeln (x, '.sp 0.1i');
  writeln (x, '\fB\\$1\fR');
  writeln (x, '..');

  { Header Three }
  writeln (x, '.de H3');
  writeln (x, '.LP');
  writeln (x, '.sp 0.1i');
  writeln (x, '\fB\\$1\fR');
  writeln (x, '..');

  { Header Four }
  writeln (x, '.de H4');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp 0.1i');
  writeln (x, '\fI\\$1\fR');
  writeln (x, '..');

  { Header Five }
  writeln (x, '.de H5');
  writeln (x, '.LP');
  writeln (x, '.ne 0.5i');
  writeln (x, '.sp 0.1i');
  writeln (x, '\fB\\$1\fR');
  writeln (x, '..');

  { Custom One }
  writeln (x, '.de CP1');
  writeln (x, '..');

  { Custom Two }
  writeln (x, '.de CP2');
  writeln (x, '..');

  { Custom Three }
  writeln (x, '.de CP3');
  writeln (x, '..');

  { Separator }
  writeln (x, '.de SEP');
  writeln (x, '.LP');
  writeln (x, '.ce');
  writeln (x, Separator);
  writeln (x, '..');

  // Primitive support for JPEG
  writeln (x, '.de JPEG');
  writeln (x, '..');
end;

procedure tEPubProfile.Build (aStory : tStory);
var
  playorder,
  index : integer;
  s,
  t,
  pathname,
  outpath,
  filename,
  cfilename,
  ofilename: string;
  x,
  y: text;
  Chapters : tChapterList;
begin
  // Mark the Log
  s := ' -> Creating EPub for Profile ''' + Name + '''';
  frmLog.txtLog.Lines.Add (s);

  Chapters := tChapterList.Create;
  Chapters.BaseDir := aStory.SourceDir;
  Chapters.Load;

  pathname := aStory.SourceDir + '/';
  outpath := StripSpaces (OutputDir + '/' + aStory.LongName);
  ofilename := StripSpaces (aStory.LongName + '-' + Name);
  filename := pathname + '/' + ofilename + '.ms';

  if (not (DirectoryExists (outpath))) then begin
    mkdir (outpath);

    t := outpath + '/OEBPS';
    if (not (DirectoryExists (t))) then
      mkdir (t);

    t := outpath + '/OEBPS/Text';
    if (not (DirectoryExists (t))) then
      mkdir (t);

    t := outpath + '/META-INF';
    if (not (DirectoryExists (t))) then
      mkdir (t);
  end;

  // Next, write the supporting files

	// mimetype
  assign (x, outpath + '/mimetype');
  rewrite (x);
  write (x, 'application/epub+zip');
  close (x);

	// container.xml
  assign (x, outpath + '/META-INF/container.xml');
  rewrite (x);
  writeln (x, '<?xml version="1.0"?>');
	writeln (x, '<container version="1.0" ',
  	'xmlns="urn:oasis:names:tc:opendocument:xmlns:container">');
  writeln (x, #9, '<rootfiles>');
  writeln (x, #9, #9, '<rootfile full-path="OEBPS/content.opf" ',
  	'media-type="application/oebps-package+xml"/>');
  writeln (x, #9, '</rootfiles>');
	writeln (x, '</container>');
  close (x);

  // content.opf
  assign (x, outpath + '/OEBPS/content.opf');
  rewrite (x);

  writeln (x, '<?xml version="1.0"?>');
  writeln (x, '<package xmlns="http://www.idpf.org/2007/opf" ',
  	'unique-identifier="BookID" version="2.0">');
  writeln (x, #9, '<metadata xmlns:dc="http://purl.org/dc/elements/1.1/" ',
    'xmlns:opf="http://www.idpf.org/2007/opf">');
  writeln (x, #9, #9, '<dc:title>', aStory.Title, '</dc:title>');
  if (FileExists (pathname + '/blurb.so')) then begin
    writeln (x, #9, #9, '<dc:description>');
    assign (y, pathname + '/blurb.so');
    reset (y);
    while (not (eof (y))) do begin
      readln (y, s);
      writeln (x, s);
    end;
    writeln (x, '</dc:description>');
    close (y);
  end;
  writeln (x, #9, #9, '<dc:language>en</dc:language>');
  writeln (x, #9, #9, '<dc:identifier id="BookId" ',
  	'opf:scheme="ISBN">123456789X</dc:identifier>');
  writeln (x, #9, #9, '<dc:creator opf:role="aut">', aStory.Author,
    '</dc:creator>');
  if (EPubSeries <> '') then
    writeln (x, #9, #9, '<meta name="calibre:series" content="', EPubSeries,
      '"/>');
  writeln (x, #9, '</metadata>');

  writeln (x, #9, '<manifest>');
	writeln (x, #9, #9, '<item id="ncx" href="toc.ncx" ',
    'media-type="application/x-dtbncx+xml"/>');
  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);
    writeln (x, #9, #9, '<item id="', Chapters.Current.Filename, '" ',
	    'href="Text/', Chapters.Current.Filename, '.xhtml" ',
  	  'media-type="application/xhtml+xml"/>');
  end;
  writeln (x, #9, '</manifest>');
  writeln (x, #9, '<spine toc="ncx">');
  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);
    writeln (x, #9, #9, '<itemref idref="', Chapters.Current.Filename, '"/>');
  end;
  writeln (x, #9, '</spine>');
  writeln (x, '</package>');
  close (x);

  // toc.ncx
  playorder := 1;
  assign (x, outpath + '/OEBPS/toc.ncx');
  rewrite (x);
  writeln (x, '<?xml version="1.0" encoding="UTF-8"?>');
  writeln (x, '<!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">');
  writeln (x, '');
  writeln (x, '<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">');
  writeln (x, #9, '<head>');
  writeln (x, #9, #9, '<meta name="dtb:uid" content="123456789X"/>');
  writeln (x, #9, #9, '<meta name="dtb:depth" content="1"/>');
  writeln (x, #9, #9, '<meta name="dtb:totalPageCount" content="0"/>');
  writeln (x, #9, #9, '<meta name="dtb:maxPageNumber" content="0"/>');
  writeln (x, #9, '</head> ');
  writeln (x, #9, '<docTitle>');
  writeln (x, #9, #9, '<text>', aStory.Title, '</text>');
  writeln (x, #9, '</docTitle>');
  writeln (x, #9, '<navMap>');

  playorder := 1;
  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);
    writeln (x, #9, #9, '<navPoint id="', Chapters.Current.Filename,
      '" playOrder="', playorder, '">');
    writeln (x, #9, #9, #9, '<navLabel>');
    writeln (x, #9, #9, #9, #9, '<text>', Chapters.Current.Title, '</text>');
    writeln (x, #9, #9, #9, '</navLabel>');
    writeln (x, #9, #9, #9, '<content src="Text/', Chapters.Current.Filename,
      '.xhtml"/>');
    writeln (x, #9, #9, '</navPoint>');
    playorder += 1;
  end;
  writeln (x, #9, '</navMap>');
  writeln (x, '</ncx>');
  close (x);

  // Now write the individual chapter files
  for index := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index);

    pathname := aStory.SourceDir + '/';
    ofilename := StripSpaces (Chapters.Current.FileName);
    filename := pathname + ofilename + '-' + StripSpaces (Name) + '.ms';

    assign (x, filename);
    rewrite (x);

    WriteTopSo (x);
    writeln (x, '.ad b');

    with (Chapters.Current) do begin
      cfilename := Filename;

      if not (SuppressTitle) then begin
        if ((SubtitleFirst) and (Subtitle <> '')) then
          writeln (x, '.H3 "', Subtitle, '"');
        writeln (x, '.H1 "', Title, '"');
        if (not (SubtitleFirst) and (Subtitle <> '')) then
          writeln (x, '.H3 "', Subtitle, '"');
      end;
      if (FileExists (pathname + '/' + cfilename + '.co')) then begin
        writeln (x, '.so ' + cfilename + '.co');
        writeln (x, '.SEP');
      end;
    end;
    writeln (x, '.so ', cfilename + '.so');
    close (x);
  end;
end;

{$endregion}

{$region tProfileList}

function tProfileList.GetProfileCount : integer;
begin
	GetProfileCount := length (t_Profiles);
end;

procedure tProfileList.New (aProfileName : string; aProfileType : integer);
var
  index : integer;
begin
	index := length (t_Profiles);
  SetLength (t_Profiles, index + 1);
  case (aProfileType) of
  	ptPDF  : t_Profiles [index] := tPDFProfile.Create;
    ptHTML : t_Profiles [index] := tHTMLProfile.Create;
    ptText : t_Profiles [index] := tTextProfile.Create;
    ptEPub : t_Profiles [index] := tEPubProfile.Create;
  end;
  t_current_Profile := t_Profiles [index];
  t_current_Profile.Name := aProfileName;
  MarkDirty;
end;

procedure tProfileList.Add (aProfile : tBaseProfile);
var
  index : integer;
begin
	index := length (t_Profiles);
  SetLength (t_Profiles, index + 1);
  t_Profiles [index] := aProfile;
  t_current_Profile := t_Profiles [index];
  MarkDirty;
end;

procedure tProfileList.Delete;
var
  index,
  last : integer;
begin
  last := length (t_Profiles) - 1;
  index := 0;
  while ((t_Profiles [index] <> t_current_Profile) and (index < last)) do
    inc (index);
  t_Profiles [index].Destroy;
  if (index < last) then
    t_Profiles [index] := t_Profiles [last];
  SetLength (t_Profiles, last);
  MarkDirty;
end;

procedure tProfileList.Select (aProfileName : string);
var
  found : boolean;
  index : integer;
begin
	// Simple linear search
  index := 0;
  found := false;
  while ((index < length (t_Profiles)) and not found) do begin
    if (aProfileName = t_Profiles [index].Name) then
      found := true
    else
    	index += 1;
  end;
  if found then
    t_current_Profile := t_Profiles [index];
end;

procedure tProfileList.SelectAt (aIndex : integer);
begin
  t_current_Profile := t_Profiles [aIndex];
end;

procedure tProfileList.Load;
var
  filename,
	s : string;
  Done : boolean;
  t : text;
  index : integer;
begin
  // Verify that the directory exists, and create it if it doesn't
	filename := t_basedir;
  if not (DirectoryExists(filename)) then
    CreateDir (filename);
  filename += '/profiles.fic';

  // Verify that profiles.fic exists
  if (FileExists (filename)) then begin
    // Open it, and start scanning
    assign (t, filename);
    reset (t);
    Done := false;

    repeat
      repeat
        readln (t, s);
        if (length (s) > 0) then
          Trim (s);

        if (s = '[end list]') then
          Done := true;
      until ((copy (s, 1, 8)  = '[Profile') or Done);

      if (s = '[Profile]') then
        repeat
          // Version 1.0.x Profiles List cannot be easily read and converted at
          // this time.
      	  readln (t, s);
        until (s = '[end]')
      else if (s = '[Profile PDF]') then begin
      	index := length (t_Profiles);
        SetLength (t_Profiles, index + 1);
        t_Profiles [index] := tPDFProfile.Create;
        t_Profiles [index].Load (t);
      end else if (s = '[Profile HTML]') then begin
      	index := length (t_Profiles);
        SetLength (t_Profiles, index + 1);
        t_Profiles [index] := tHTMLProfile.Create;
        t_Profiles [index].Load (t);
      end else if (s = '[Profile Text]') then begin
      	index := length (t_Profiles);
        SetLength (t_Profiles, index + 1);
        t_Profiles [index] := tTextProfile.Create;
        t_Profiles [index].Load (t);
      end else if (s = '[Profile EPub]') then begin
      	index := length (t_Profiles);
        SetLength (t_Profiles, index + 1);
        t_Profiles [index] := tEPubProfile.Create;
        t_Profiles [index].Load (t);
      end;
    until Done;

    close (t);
  end;
end;

procedure tProfileList.Save;
var
  t : text;
  filename : string;
  index : integer;
begin
  if Dirty then begin
    // Verify that the target directory exists
  	filename := t_basedir;
    if (not (DirectoryExists (filename))) then
      CreateDir (filename);

    // Open the profiles file
    filename += '/profiles.fic';
    assign (t, filename);
    rewrite (t);

    // Go through all the profiles, and save them
    for index := 0 to (length (t_Profiles) - 1) do
      t_Profiles [index].Save (t);

    // Close out the list
    writeln (t, '[end list]');
    close (t);
    t_dirty := FALSE;
  end;
end;

function tProfileList.Name (index : integer) : string;
begin
	Name := t_Profiles [index].Name;
end;

procedure tProfileList.Edit;
begin
  t_current_profile.Edit;
end;

procedure tProfileList.MarkDirty;
begin
	t_dirty := true;
end;

{$endregion}

end.

