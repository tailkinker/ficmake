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
  Classes, SysUtils,
  gstory;

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
      constructor Create; virtual;
      procedure Make (aStory : tStory); virtual;
      procedure Load (var t : text); virtual;
      procedure Save (var t : text); virtual;
      procedure Edit; virtual;
  end;

  tPDFProfile = class (tBaseProfile)
    private
      t_fonts : tGroffFontArray;
      t_pagesize : byte;
      t_PageH,
      t_PageV : longint;
      t_columns : byte;
      t_margins : array [0..3] of longint;
      t_H1Mode : byte;
      t_flags : array [0..3] of boolean;
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
      class function ProfileType : integer; override;
      property GroffFonts : tGroffFontArray read t_fonts write t_fonts;
      property PageSize : byte read t_pagesize write SetPageSize default 0;
      property PageH : longint read t_pageh write t_pageh default 612000;
      property PageV : longint read t_pagev write t_pagev default 792000;
      property Columns : byte read t_columns write t_columns;
      property OuterMargin  : longint index 0 read GetMargin write SetMargin;
      property InnerMargin  : longint index 1 read GetMargin write SetMargin;
      property TopMargin    : longint index 2 read GetMargin write SetMargin;
      property BottomMargin : longint index 3 read GetMargin write SetMargin;
      property H1Mode : byte read t_H1Mode write t_H1Mode;
      property OneColumnTitlePage : boolean index 0 read GetFlag write SetFlag;
      property Landscape          : boolean index 1 read GetFlag write SetFlag;
      property UseTBL             : boolean index 2 read GetFlag write SetFlag;
      property UseEQN             : boolean index 3 read GetFlag write SetFlag;
      constructor Create; override;
      procedure Make (aStory : tStory); override;
      procedure Load (var t : text); override;
      procedure Save (var t : text); override;
      procedure Edit; override;
  end;

  tHTMLProfile = class (tBaseProfile)
    private
      t_fonts : tGroffFontArray;
      t_flags : array [0..6] of boolean;
      t_strings : array [0..1] of string;
      procedure SetFlag (index : integer; aflag : boolean);
      function GetFlag (index : integer) : boolean;
      procedure SetString (index : integer; astring : string);
      function GetString (index : integer) : string;
    public
      class function ProfileType : integer; override;
      property GroffFonts : tGroffFontArray read t_fonts write t_fonts;
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
      procedure Make (aStory : tStory); override;
      procedure Load (var t : text); override;
      procedure Save (var t : text); override;
      procedure Edit; override;
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
      constructor Create; override;
      procedure Make (aStory : tStory); override;
      procedure Load (var t : text); override;
      procedure Save (var t : text); override;
      procedure Edit; override;
  end;

  tEPubProfile = class (tBaseProfile)
    private
      t_blurb : boolean;
      t_series : string;
    public
      class function ProfileType : integer; override;
      property BlurbInEPub : boolean read t_blurb write t_blurb;
      property EPubSeries : string read t_series write t_series;
      constructor Create; override;
      procedure Make (aStory : tStory); override;
      procedure Load (var t : text); override;
      procedure Save (var t : text); override;
      procedure Edit; override;
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

implementation

uses
  forms, fpdfpro,
  dgroff;

{$region tBaseProfile}


class function tBaseProfile.ProfileType : integer;
begin
  ProfileType := ptBase;
end;

constructor tBaseProfile.Create;
begin
  RunError (211);
end;

procedure tBaseProfile.Make (aStory : tStory);
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

{$endregion}

{$region tPDFProfile}

class function tPDFProfile.ProfileType : integer;
begin
  ProfileType := ptPDF;
end;

procedure tPDFProfile.SetPageSize (aPageSize : byte);
begin
  if (aPageSize in [0..PaperCount]) then begin
    t_pagesize := aPageSize;
    t_PageH := PaperMeasurements [aPageSize, 0];
    t_PageV := PaperMeasurements [aPageSize, 1];
  end;
end;

constructor tPDFProfile.Create;
begin
  Separator := '* * * * *';
  OutputDir := '';
  Useeqn := false;
  Usepreconv := false;
  Usetbl := false;

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

  PageSize := 0; // Letter
  Landscape := FALSE;
  PageH := 612000; // in 1/1000 pt.
  PageV := 792000;
  Columns := 1;
  OneColumnTitlePage := FALSE;

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

procedure tPDFProfile.Make (aStory : tStory);
begin

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

      // Groff stuff
      else if (k = 'Call eqn') then
        Useeqn := true
      else if (k = 'Call preconv') then
        Usepreconv := true
      else if (k = 'Call tbl') then
        Usetbl := true

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
  if (OneColumnTitlePage) then
    writeln (t, 'One Column Title Page');

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

  // Groff Options
  if (Useeqn) then
    writeln (t, 'Call eqn');
  if (Usepreconv) then
    writeln (t, 'Call preconv');
  if (usetbl) then
    writeln (t, 'Call tbl');

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

procedure tHTMLProfile.Make (aStory : tStory);
begin

end;

procedure tHTMLProfile.Load (var t : text);
var
  k,
  v,
  k1,
  v1,
	s : string;
  font : integer = -1;
  i,
  j: integer;
  Done : boolean;
begin
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
    end;
  until Done;
end;

procedure tHTMLProfile.Save (var t : text);
var
  j,
  k,
  l : integer;
begin
  writeln (t, '[Profile HTML]');
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

  writeln (t, '[end]');
  writeln (t);
end;

procedure tHTMLProfile.Edit;
begin
  RunError (211);
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

procedure tTextProfile.Make (aStory : tStory);
begin

end;

procedure tTextProfile.Load (var t : text);
var
  k,
  v,
	s : string;
  i : integer;
  Done : boolean;
begin
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

    end;
  until Done;
end;

procedure tTextProfile.Save (var t : text);
var
  j,
  k,
  l : integer;
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

  writeln (t, '[end]');
  writeln (t);
end;

procedure tTextProfile.Edit;
begin
  RunError (211);
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

  BlurbInEPub := false;
  EPubSeries := '';
end;

procedure tEPubProfile.Make (aStory : tStory);
begin

end;

procedure tEPubProfile.Load (var t : text);
var
  k,
  v,
	s : string;
  i : integer;
  Done : boolean;
begin
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
    end;
  until Done;
end;

procedure tEPubProfile.Save (var t : text);
var
  j,
  k,
  l : integer;
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

  writeln (t, '[end]');
  writeln (t);
end;

procedure tEPubProfile.Edit;
begin
  RunError (211);
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

      if (s = '[Profile]') then repeat
        // Version 1.0.x Profiles List cannot be easily read and converted at
        // this time.
      	readln (s);
      until (s = '[end]') else if (s = '[Profile PDF]') then begin
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

