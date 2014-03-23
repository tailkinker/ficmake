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
  optInitialX,
  optInitialY : integer;
  optScreenSize : integer = 0;
  optFontSize : integer = 12;
  optAssumeMakeOnSave : boolean = FALSE;
  optOpenLogOnStart : boolean = FALSE;

const
  MaxResolution = 9;
  Initials : array [0..MaxResolution, 0..1] of integer = (
	  (  800,  480 ), // 800x480
	  (  800,  600 ), // 800x600
	  ( 1024,  600 ), // 1024x600
	  ( 1024,  768 ), // 1024x768
	  ( 1280,  720 ), // 1280x720
	  ( 1280, 1024 ), // 1280x1024
	  ( 1440,  900 ), // 1440x900
	  ( 1600,  900 ), // 1600x900
	  ( 1920, 1080 ), // 1920x1080
	  ( 1024, 1280 )  // 1024x1280
  );

procedure LoadOptions;
procedure SaveOptions;
procedure SetScale (aScale : byte);
procedure SetScreenSize;

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
      if (k = 'Screen Size') then begin
        val (v, optScreenSize);
        SetScreenSize;
      end else if (k = 'Measurement') then begin
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
  writeln (t, 'Screen Size = ', optScreenSize);
  write (t, 'Measurement = ');
  case ScaleType of
    0:
      writeln (t, 'in');
    1:
      writeln (t, 'cm');
    2:
      writeln (t, 'pt');
  end;
  writeln (t, 'Font Size = ', optFontSize);
  writeln (t, 'Open Log On Start = ', optOpenLogOnStart);
  writeln (t, 'Assume Make On Save = ', optAssumeMakeOnSave);
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

procedure SetScreenSize;
begin
  optInitialX := ((Initials [optScreenSize, 0] div 10) - 2) * 10;
  optInitialY := ((Initials [optScreenSize, 1] div 10) - 8) * 10;
end;

end.


