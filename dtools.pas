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

unit dtools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
	CommaChar : char = ',';

procedure Trim (var T : String);
function BasicReal (r : double) : string;
function InsertCommas (T : LongInt) : String;
function StripSpaces (s : string) : string;
function BaseFileName (path : string) : string;

implementation

procedure Trim (var T : String);
{
This routine strips whitespace from the front and end of a string.
}
begin
	while (T [Length (T)] = #32) do
		delete (T, length (T), 1);
	while (T [1] = #32) do
		Delete (T, 1, 1);
end;

function BasicReal (r : double) : string;
{
Returns a string representation of a double precision floating point number.
The formatting of the FP number is done in a manner similar to the BASIC
language;  no scientific notation, and insignificant digits are dropped from the
right side of the decimal place.  If the number is whole, the decimal place is
also dropped.  The routine has a maximum precision of 24 digits to the left of
the decimal place, and 12 to the right.
}
var
	s : string;
begin
	str (r:24:12, s);
	while (s [1] = #32) do
		delete (s, 1, 1);
	while (s [length (s)] = '0') do
		delete (s, length (s), 1);
	if (s [length (s)] = '.') then
		delete (s, length (s), 1);
	BasicReal := s;
end;

function InsertCommas (T : LongInt) : String;
{
Returns a string representation of a longint, with commas placed every three
digits.  The CommaChar variable defaults to a standard comma, but can be
replaced with any other character.
}
var
	R,
	S : String;
	U : LongInt;
begin
	if (T = 0) then
		S := '0'
	else
		begin
			S := '';
			U := T;
			repeat
			  Str (U mod 1000, R);
			  Trim (R);
			  if (U mod 1000) = 0 then
				  R := '000';
			  if (U div 1000) > 0 then
				  while Length (R) < 3 do
					  R := '0' + R;
			  S := R + S;
			  IF (U >= 1000) then
				  S := CommaChar + S;
			  U := U div 1000;
			until (U = 0);
		end;
	InsertCommas := S;
end;

function StripSpaces (s : string) : string;
{
Strips all spaces and quote characters from a string.  Useful for turning a
string into a useable filename.
}
var
  index : integer;
begin
  index := 1;
  repeat
  	if (s [index] in [#32, '''', '"']) then
      delete (s, index, 1)
    else
      inc (index);
  until (index > length (s));
  StripSpaces := s;
end;

function BaseFileName (path : string) : string;
var
  index : integer;
begin
  index := length (path);
  if (length (path) > 2) then begin
    repeat
      dec (index);
    until ((index = 2) or (path [index - 1] in ['/', '\']));
    if (index > 2) then
  	  BaseFileName := copy (path, index, length (path) - index + 1)
    else
      BaseFileName := path;
  end else
		BaseFileName := path;
end;

end.


