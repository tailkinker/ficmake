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

unit gstory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$region definitions}
type
  tStory = class (tObject)
    private
      t_strings : array [0..9] of string;
      t_notitle,
      t_noauthor,
      t_subfirst: boolean;
      procedure SetString (index : integer; aData : string);
      function GetString (index : integer) : string;
    public
      property Title         : string index 0 read getstring write setstring;
      property Subtitle      : string index 1 read getstring write setstring;
      property Author        : string index 2 read getstring write setstring;
      property LongName      : string index 3 read getstring write setstring;
      property ShortName     : string index 4 read getstring write setstring;
      property SourceDir     : string index 5 read getstring write setstring;
      property OmakeHeader   : string index 6 read getstring write setstring;
      property TrailerHeader : string index 7 read getstring write setstring;
      property TitlePicture  : string index 8 read getstring write setstring;
      property CoverPicture  : string index 9 read getstring write setstring;
      property SuppressTitles : boolean read t_notitle write t_notitle;
      property SuppressAuthor : boolean read t_noauthor write t_noauthor;
      property SubtitleFirst  : boolean read t_subfirst write t_subfirst;
      procedure Load (var t : text);
      procedure Save (var t : text);
  end;

  tStoryList = class (tObject)
    private
      t_dirty : boolean;
      t_stories : array of tStory;
      t_current_story : tStory;
      t_basedir : string;
      function GetStoryCount : integer;
    public
    	property Current : tStory read t_current_story;
      property Count : integer read GetStoryCount;
      property BaseDir : string read t_basedir write t_basedir;
      property Dirty : boolean read t_dirty;
      procedure SetDirty;
      procedure NewStory (aTitle : string);
      procedure DeleteStory;
      procedure SelectStory (aTitle : string);
      procedure LoadStoryList;
      procedure SaveStoryList;
      function StoryTitle (index : integer) : string;
  end;
{$endregion}

implementation

uses
  gtools;

{$region tStory}

procedure tStory.SetString (index : integer; aData : string);
begin
  t_strings [index] := aData;
end;

function tStory.GetString (index : integer) : string;
begin
  GetString := t_strings [index];
end;

procedure tStory.Load (var t : text);
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
      until ((copy (s, i, 1) = '=') OR (i = length (s)));

      // Split Keyline
      if (i = length (s)) then begin
        k := s;
        v := '';
      end else begin
        k := copy (s, 1, i - 1);
        v := copy (s, i + 1, length (s) - i);
      end;
      if (length (k) > 0) then
  	    Trim (k);
      if (length (v) > 0) then
	      Trim (v);

      // Select Variable
      if (k = 'Title') then
        Title := v
      else if (k = 'Subtitle') then
        Subtitle := v
      else if (k = 'Author') then
        Author := v
      else if (k = 'Output Name') then
        LongName := v
      else if (k = 'Input Name') then
        ShortName := v
      else if (k = 'Source Dir') then
        SourceDir := v
      else if (k = 'Omake Header') then
        OmakeHeader := v
      else if (k = 'Trailer Header') then
        TrailerHeader := v
      else if (k = 'Title Picture') then
        TitlePicture := v
      else if (k = 'Cover Picture') then
        CoverPicture := v
      else if (k = 'Suppress Titles') then
        SuppressTitles := TRUE
      else if (k = 'Suppress Author') then
        SuppressAuthor := TRUE
      else if (k = 'Subtitle Above Title') then
        SubtitleFirst := TRUE
    end;
  until Done;
end;

procedure tStory.Save (var t : text);
begin
  writeln (t, '[Story]');
  writeln (t, 'Title = ', Title);
  if (Subtitle <> '') then
  	writeln (t, 'Subtitle = ', Subtitle);
  writeln (t, 'Author = ', Author);
  writeln (t, 'Output Name = ', LongName);
  writeln (t, 'Input Name = ', ShortName);
  writeln (t, 'Source Dir = ', SourceDir);
  if (OmakeHeader <> '') then
    writeln (t, 'Omake Header = ', OmakeHeader);
  if (TrailerHeader <> '') then
    writeln (t, 'Trailer Header = ', TrailerHeader);
  if (TitlePicture <> '') then
    writeln (t, 'Title Picture = ', TitlePicture);
  if (CoverPicture <> '') then
    writeln (t, 'Cover Picture = ', CoverPicture);
  if (SuppressTitles) then
    writeln (t, 'Suppress Titles');
  if (SuppressAuthor) then
    writeln (t, 'Suppress Author');
  if (SubtitleFirst) then
    writeln (t, 'Subtitle Above Title');
  writeln (t, '[end]');
end;

{$endregion}

{$region tStoryList}
function tStoryList.GetStoryCount : integer;
begin
	GetStoryCount := length (t_stories);
end;

procedure tStoryList.SetDirty;
begin
	t_dirty := true;
end;

procedure tStoryList.NewStory (aTitle : string);
var
  index : integer;
begin
  index := length (t_stories);
  SetLength (t_stories, index + 1);
  t_stories [index] := tStory.Create;
  t_current_story := t_stories [index];
  t_current_story.Title := aTitle;
  SetDirty;
end;

procedure tStoryList.DeleteStory;
var
  index,
  last : integer;
begin
  last := length (t_stories) - 1;
  index := 0;
  while ((t_stories [index] <> t_current_story) and (index < last)) do
  	inc (index);
  t_stories [index].Destroy;
  if (index < last) then
    t_stories [index] := t_stories [last];
  SetLength (t_stories, last);
  SetDirty;
end;

procedure tStoryList.SelectStory (aTitle : string);
var
  found : boolean;
  index : integer;
begin
	// Simple linear search
  index := 0;
  found := false;
  while ((index < length (t_stories)) and not found) do begin
    if (aTitle = t_stories [index].Title) then
      found := true
    else
    	index += 1;
  end;
  if found then
    t_current_story := t_stories [index];
end;

procedure tStoryList.LoadStoryList;
var
  filename,
	s : string;
  index : integer;
  Done : boolean;
  t : text;
begin
  // Verify that the directory exists, and create it if it doesn't
	filename := BaseDir;
  if not (DirectoryExists(filename)) then
    CreateDir (filename);
  filename += '/stories.fic';

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
      until ((s = '[Story]') or Done);

      if (s = '[Story]') then begin
        index := length (t_stories);
        SetLength (t_stories, index + 1);
        t_stories [index] := tStory.Create;
        t_stories [index].Load (t);
      end;
    until Done;

    close (t);
  end;
end;

procedure tStoryList.SaveStoryList;
var
  t : text;
  filename : string;
  index : integer;
begin
  if Dirty then begin
    // Verify that the target directory exists
  	filename := BaseDir;
    if (not (DirectoryExists (filename))) then
      CreateDir (filename);

    // Open the stories file
    filename += '/stories.fic';
    assign (t, filename);
    rewrite (t);

    // Go through all the stories, and save them
    for index := 0 to (length (t_stories) - 1) do begin
      t_stories [index].Save (t);
      writeln (t);
    end;

    // Close out the list
    writeln (t, '[end list]');
    close (t);
    t_dirty := FALSE;
  end;
end;

function tStoryList.StoryTitle (index : integer) : string;
begin
	StoryTitle := t_stories [index].Title;
end;

{$endregion}
end.

