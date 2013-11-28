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

unit doption;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  ScaleType : byte = 0;
  ScaleMult : real = 72000.0;
  InitialX,
  InitialY : integer;
  optScreenSize : integer = 0;
  optFontSize : integer = 12;

procedure LoadOptions;
procedure SaveOptions;
procedure SetScale (aScale : byte);

implementation

uses
  dtools,
  foptions;

procedure LoadOptions;
var
  filename,
  k,
  v,
	s : string;
  i : integer;
  t : text;
begin
  // Verify that the directory exists, and create it if it doesn't
	filename := GetEnvironmentVariable ('HOME') + '/.ficmake';
  if not (DirectoryExists(filename)) then
    CreateDir (filename);
  filename += '/ficmake.rc';

  // Verify that stories.fic exists
  if (FileExists (filename)) then begin
    // Open it, and start scanning
    assign (t, filename);
    reset (t);

    repeat
      readln (t, s);
      if (length (s) > 0) then
        Trim (s);

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
      if (k = 'Measurement') then begin
        if (v = 'in') then
        	SetScale (0)
        else if (v = 'cm') then
          SetScale (1)
        else if (v = 'pt') then
          SetScale (2);
      end else if (k = 'Font Size') then begin
        val (v, i);
        optFontSize := i;
      end;
    until (eof (t));

    close (t);
  end;

end;

procedure SaveOptions;
var
  filename : string;
  t : text;
begin
  // Verify that the proper directory exists
	filename := GetEnvironmentVariable ('HOME') + '/.ficmake';
  if not (DirectoryExists(filename)) then
    CreateDir (filename);
  filename += '/ficmake.rc';

  // Open the Volumes file
  assign (t, filename);
  rewrite (t);

  // Save all Options
  write (t, 'Measurement = ');
  case ScaleType of
    0:
      writeln (t, 'in');
    1:
      writeln (t, 'cm');
    2:
      writeln (t, 'pt');
  end;
  write (t, 'Font Size = ', optFontSize);

  // Close out the list
  close (t);
end;

procedure SetScale (aScale : byte);
begin
	ScaleType := aScale;
  case ScaleType of
  	0 : // inches
      ScaleMult := 72000;
    1 : // centimeters
      ScaleMult := 28346.457;
    2 : // Points
      ScaleMult := 1000;
  end;
end;

end.


