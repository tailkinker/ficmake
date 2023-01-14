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

unit gvolume;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$region type}
type
  tVolume = class (tObject)
    private
      t_volname,
      t_basedir,
      t_author : string;
      t_index : boolean;
    public
      property VolumeName : string read t_volname write t_volname;
      property BaseDir : string read t_basedir write t_basedir;
      property Author : string read t_author write t_author;
      property GenerateIndexFile : boolean read t_index write t_index default false;
      procedure Load (var t : text);
      procedure Save (var t : text);
  end;

  tVolumeList = class (tObject)
    private
      t_dirty : boolean;
      t_volumes : array of tVolume;
      t_current_volume : tVolume;
      function GetVolumeCount : integer;
    public
      property Current : tVolume read t_current_volume;
      property Count : integer read GetVolumeCount;
      property Dirty : boolean read t_dirty;
      procedure NewVolume (aVolumeName : string);
      procedure DeleteVolume;
      procedure SelectVolume (aVolumeName : string);
      procedure LoadVolumeList;
      procedure SaveVolumeList;
      function VolumeName (index : integer) : string;
      procedure MarkDirty;
  end;
{$endregion}

implementation

uses
  gtools;

{$region tVolume}

procedure tVolume.Load (var t : text);
var
  VolDone : boolean;
  i : integer;
  s,
  k,
  v : string;
begin
	VolDone := false;
  GenerateIndexFile := FALSE;
  repeat
    // Read Keyline
    readln (t, s);
    if (length (s) > 0) then
      Trim (s);

    if (s = '[end]') then
      VolDone := true
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
      if (k = 'Volume') then
      	VolumeName := v
      else if (k = 'Index') then
        GenerateIndexFile := TRUE
      else if (k = 'BaseDir') then
        BaseDir := v
      else if (k = 'Author') then
        Author := v;
    end;
  until VolDone;
end;

procedure tVolume.Save (var t : text);
begin
  writeln (t, '[Volume]');
  writeln (t, 'Volume = ', VolumeName);
  writeln (t, 'BaseDir = ', BaseDir);
  writeln (t, 'Author = ', Author);
  if (GenerateIndexFile) then
    writeln (t, 'Index');
  writeln (t, '[end]');
end;

{$endregion}

{$region tVolumeList}
function tVolumeList.GetVolumeCount : integer;
begin
  GetVolumeCount := length (t_volumes);
end;

procedure tVolumeList.NewVolume (aVolumeName : string);
var
  index : integer;
begin
  index := length (t_volumes);
  SetLength (t_volumes, index + 1);
  t_volumes [index] := tVolume.Create;
  t_current_volume := t_volumes [index];
  t_current_volume.VolumeName := aVolumeName;
  t_dirty := true;
end;

procedure tVolumeList.DeleteVolume;
var
  index,
  last : integer;
begin
  last := length (t_volumes) - 1;
  index := 0;
  while ((t_volumes [index] <> t_current_volume) and (index < last)) do
  	inc (index);
  t_volumes [index].Destroy;
  if (index < last) then
    t_volumes [index] := t_volumes [last];
  SetLength (t_volumes, last);
  t_dirty := true;
end;

procedure tVolumeList.SelectVolume (aVolumeName : string);
var
  found : boolean;
  index : integer;
begin
	// Simple linear search
  index := 0;
  found := false;
  while ((index < length (t_volumes)) and not found) do begin
    if (aVolumeName = t_volumes [index].VolumeName) then
      found := true
    else
    	index += 1;
  end;
  if found then
    t_current_volume := t_volumes [index];
end;

procedure tVolumeList.LoadVolumeList;
var
  filename,
  s : string;
  index : integer;
  Done : boolean;
  t : text;
begin
  // Verify that the directory exists, and create it if it doesn't
	filename := GetEnvironmentVariable ('HOME') + '/.config';
  if not (DirectoryExists(filename)) then
    CreateDir (filename);
  filename += '/ficmake';
  if not (DirectoryExists(filename)) then
    CreateDir (filename);
  filename += '/volumes.fic';

  // Verify that volumes.fic exists
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
      until ((s = '[Volume]') or Done);

      if (s = '[Volume]') then begin
        index := length (t_volumes);
        SetLength (t_volumes, index + 1);
        t_volumes [index] := tVolume.Create;
        t_volumes [index].Load (t);
      end;
    until Done;

    close (t);
  end;
  t_dirty := false;
end;

procedure tVolumeList.SaveVolumeList;
var
  filename : string;
  index : integer;
  t : text;
begin
  // Verify that the directory exists, and create it if it doesn't
	filename := GetEnvironmentVariable ('HOME') + '/.config';
  if not (DirectoryExists(filename)) then
    CreateDir (filename);
  filename += '/ficmake';
  if not (DirectoryExists(filename)) then
    CreateDir (filename);
  filename += '/volumes.fic';

  // Open the Volumes file
  assign (t, filename);
  rewrite (t);

  // Go through all the volumes, and save them
  for index := 0 to (length (t_volumes) - 1) do begin
    t_volumes [index].Save (t);
    writeln (t);
  end;

  // Close out the list
  writeln (t, '[end list]');
  close (t);
  t_dirty := false;
end;

function tVolumeList.VolumeName (index : integer) : string;
begin
  VolumeName := t_volumes [index].VolumeName;
end;

procedure tVolumeList.MarkDirty;
begin
  t_dirty := true;
end;

{$endregion}

end.

