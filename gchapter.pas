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

unit gchapter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  tChapter = class (tObject)
    private
      t_strings : array [0..2] of string;
      t_book,
      t_subfirst,
      t_notitle,
      t_isindex : boolean;
      procedure SetString (index : integer; aData : string);
      function GetString (index : integer) : string;
    public
      property Title    : string index 0 read GetString write SetString;
      property Subtitle : string index 1 read GetString write SetString;
      property Filename : string index 2 read GetString write SetString;
      property IsABook : boolean read t_book write t_book;
      property SubtitleFirst : boolean read t_subfirst write t_subfirst;
      property SuppressTitle : boolean read t_notitle write t_notitle;
      property IsIndex : boolean read t_isindex write t_isindex;
      procedure Load (var t : text);
      procedure Save (var t : text);
  end;

  tChapterList = class (tObject)
    private
      t_index : integer;
      t_dirty : boolean;
      t_chapters : array of tChapter;
      t_current_chapter : tChapter;
      t_basedir : string;
      function GetChapterCount : integer;
    public
      property Current : tChapter read t_current_chapter;
      property Count : integer read GetChapterCount;
      property Dirty : boolean read t_dirty;
      property BaseDir : string read t_basedir write t_basedir;
      property CurrentIndex : integer read t_index;
      procedure New (aChapterName : string);
      procedure Delete;
      procedure Select (aChapterName : string);
      procedure SelectAt (aChapterIndex : integer);
      procedure Load;
      procedure Save;
      function Name (index : integer) : string;
      procedure MarkDirty;
      procedure MoveChapterUp;
      procedure MoveChapterDown;
  end;

implementation

uses
  gtools;

{$region tChapter}

procedure tChapter.SetString (index : integer; aData : string);
begin
	t_strings [index] := aData;
end;

function tChapter.GetString (index : integer) : string;
begin
	GetString := t_strings [index];
end;

procedure tChapter.Load (var t : text);
var
  k,
  v,
	s : string;
  i : integer;
  Done : boolean;
begin
  Done := false;
  IsABook := FALSE;
  SubtitleFirst := FALSE;
  SuppressTitle := FALSE;

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
      if (k = 'Title') then
        Title := v
      else if (k = 'Subtitle') then
        Subtitle := v
      else if (k = 'Filename') then
        Filename := v
      else if (k = 'Book Divider') then begin
        if not (Upcase (v) = 'FALSE') then
          IsABook := TRUE
      end else if (k = 'Subtitle First') then begin
        if not (Upcase (v) = 'FALSE') then
          SubtitleFirst := TRUE
      end else if (k = 'Is Index') then begin
        IsIndex := TRUE
      end else if (k = 'Suppress Title') then
        SuppressTitle := TRUE;
    end;
  until Done;
end;

procedure tChapter.Save (var t : text);
begin
  writeln (t, '[Chapter]');
  writeln (t, 'Title = ', Title);
  if (Subtitle <> '') then
    writeln (t, 'Subtitle = ', Subtitle);
  writeln (t, 'Filename = ', Filename);
  if (IsABook) then
    writeln (t, 'Book Divider');
  if (SubtitleFirst) then
    writeln (t, 'Subtitle First');
  if (SuppressTitle) then
    writeln (t, 'Suppress Title');
  if (IsIndex) then
    writeln (t, 'Is Index');
  writeln (t, '[end]');
end;

{$endregion}

{$region tChapterList}

function tChapterList.GetChapterCount : integer;
begin
	GetChapterCount := length (t_chapters);
end;

procedure tChapterList.New (aChapterName : string);
var
  index : integer;
begin
	index := length (t_chapters);
  SetLength (t_chapters, index + 1);
  t_chapters [index] := tChapter.Create;
  t_current_chapter := t_chapters [index];
  t_index := index;
  t_current_chapter.Title := aChapterName;
  MarkDirty;
end;

procedure tChapterList.Delete;
var
  index,
  last : integer;
begin
  last := length (t_chapters) - 1;
  index := 0;
  while ((t_chapters [index] <> t_current_chapter) and (index < last)) do
    inc (index);
  t_chapters [index].Destroy;
  if (index < last) then
    t_chapters [index] := t_chapters [last];
  SetLength (t_chapters, last);
  MarkDirty;
  t_current_chapter := nil;
  t_index := -1; // No chapter selected after a Delete;
end;

procedure tChapterList.Select (aChapterName : string);
var
  found : boolean;
  index : integer;
begin
	// Simple linear search
  index := 0;
  found := false;
  while ((index < length (t_chapters)) and not found) do begin
    if (aChapterName = t_chapters [index].Title) then
      found := true
    else
    	index += 1;
  end;
  if found then begin
    t_current_chapter := t_chapters [index];
    t_index := index
  end;
end;

procedure tChapterList.Load;
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
  filename += '/chapters.fic';

  // Verify that stories.fic exists
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
      until ((s = '[Chapter]') or Done);

      if (s = '[Chapter]') then begin
      	index := length (t_chapters);
        SetLength (t_chapters, index + 1);
        t_chapters [index] := tChapter.Create;
        t_chapters [index].Load (t);
      end;
    until Done;

    close (t);
  end;
  t_index := -1; // No selection after a load.
end;

procedure tChapterList.Save;
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

    // Open the stories file
    filename += '/chapters.fic';
    assign (t, filename);
    rewrite (t);

    // Go through all the stories, and save them
    if (length(t_chapters) > 0) then
      for index := 0 to (length (t_chapters) - 1) do
        t_chapters [index].Save (t);

    // Close out the list
    writeln (t, '[end list]');
    close (t);
    t_dirty := FALSE;
  end;
end;

procedure tChapterList.SelectAt (aChapterIndex : integer);
begin
  t_current_chapter := t_chapters [aChapterIndex];
  t_index := aChapterIndex;
end;

function tChapterList.Name (index : integer) : string;
begin
	Name := t_chapters [index].Title;
end;

procedure tChapterList.MarkDirty;
begin
	t_dirty := true;
end;

procedure tChapterList.MoveChapterUp;
var
  tempChapter : tChapter;
begin
  if (t_index > 0) then begin
    tempChapter := t_chapters [t_index];
    t_chapters [t_index] := t_chapters [t_index - 1];
    t_chapters [t_index - 1] := tempChapter;
    t_index -= 1;
  end;
end;

procedure tChapterList.MoveChapterDown;
var
  tempChapter : tChapter;
begin
  if (t_index < (Count - 1)) then begin
    tempChapter := t_chapters [t_index];
    t_chapters [t_index] := t_chapters [t_index + 1];
    t_chapters [t_index + 1] := tempChapter;
    t_index += 1;
  end;
end;

{$endregion}

end.

