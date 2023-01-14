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

unit dgroff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  PaperCount = 13;
	PaperSizes : array [0..PaperCount, 1..2] of string =
		(
		( 'Letter', '-pletter' ),
		( 'Legal', '-plegal' ),
		( 'Statement', '-pstatement' ),
		( 'Tabloid', '-ptabloid '),
		( 'Trade', '-p9i,6i'),
		( 'Executive', '-pexecutive' ),
		( 'A0', '-pa0' ),
		( 'A1', '-pa1' ),
		( 'A2', '-pa2' ),
		( 'A3', '-pa3' ),
		( 'A4', '-pa4' ),
		( 'A5', '-pa5' ),
		( 'A6', '-pa6' ),
		( 'Custom...', '') // Always the last
		);

  PaperMeasurements : array [0..PaperCount, 0..1] of longint =
    (
    (  612000,   792000 ),
    (  612000,  1008000 ),
    (  396000,   612000 ),
    (  792000,  1224000 ),
    (  432000,   648000 ),
    (  522000,   756000 ),
    ( 2383200,  3369600 ),
    ( 1684799,  2383200 ),
    ( 1188000,  1684800 ),
    (  842400,  1188000 ),
    (  597600,   842400 ),
    (  417600,   597600 ),
    (  295200,   417600 ),
    (  612000,   792000 )
    );

  FontFamilyNames : array [0..6] of string = (
    'Avant-Garde',
	  'Bookman',
    'Courier',
    'Helvetica',
    'New Century Schoolbook',
    'Palatino',
    'Times New Roman'
    );

  FontNames : array [0..3] of string = (
    'Normal',
    'Bold',
    'Italics',
    'Bold Italics'
    );

implementation

end.

