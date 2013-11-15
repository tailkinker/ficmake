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
      procedure Make;
  end;

  tVolumeList = class (tObject)
    private
      t_volumes : array of tVolume;
      function GetVolumeCount : integer;
    public
      Current : tVolume;
      property Count : integer read GetVolumeCount;
      procedure NewVolume (aVolumeName : string);
      procedure DeleteVolume;
      procedure SelectVolume (aVolumeName : string);
      procedure LoadVolumeList;
      procedure SaveVolumeList;
  end;

implementation

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
    system.readln (t, s);
    if (length (s) > 0) then                          ,
      Trim (s);

    if (s = '[end]') then
      VolDone := true
    else begin
      // Find the = sign
      i := 0;
      repeat
        inc (i);
      until (copy (s, i, 1) = '=');

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
  if (VolumeList [i].GenerateIndexFile) then
    writeln (t, 'Index');
  writeln (t, '[end]');
end;

procedure tVolume.Make;
begin

end;
{$endregion}

{$region tVolumeList}
function tVolumeList.GetVolumeCount : integer;
begin
  GetVolumeCount := length (t_volumes);
end;

procedure tVolumeList.NewVolume (aVolumeName : string);
begin
  SetLength (t_volumes, length (t_volumes) + 1);
  Current := t_volumes [length (t_volumes)];
  Current := tVolume.Create;
end;

procedure tVolumeList.DeleteVolume;
begin

end;

procedure tVolumeList.SelectVolume (aVolumeName : string);
begin

end;

procedure tVolumeList.LoadVolumeList;
begin

end;

procedure tVolumeList.SaveVolumeList;
begin

end;

{$end region}

end.

