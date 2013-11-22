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

unit foptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmOptions }

  TfrmOptions = class (TForm )
    btnGuess : TButton;
    btnOk : TButton;
    radScreen : TRadioGroup;
    procedure btnGuessClick (Sender : TObject );
    procedure FormCreate (Sender : TObject );
    procedure FormResize (Sender : TObject );
    procedure radScreenClick (Sender : TObject );
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  InitialX,
  InitialY : integer;
  optScreenSize : integer = 0;

implementation

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

{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.FormResize (Sender : TObject );
begin
  btnOk.Left := (Width - btnOk.Width) - 8;
  btnOk.Top := (Height - btnOk.Height) - 8;
  btnGuess.Top := (radScreen.Height + 16);
end;

procedure TfrmOptions.FormCreate (Sender : TObject );
var
  index : integer;
  s,
  t : string;
  r : real;
begin
  radScreen.Items.Clear;
  for index := 0 to MaxResolution do begin
		str (Initials [index, 0], s);
    str (Initials [index, 1], t);
    s += 'x' + t;
    r := Initials [index, 0] / Initials [index, 1];
    if (r > 1.4) then
    	s += ' (W)'
    else if (r < 1) then
      s += ' (P)';
    radScreen.Items.Add (s);
  end;
  Width := radScreen.Width + 16;
  Height := radScreen.Height + 48;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmOptions.btnGuessClick (Sender : TObject );
var
  ScreenMP,
  ThisMP,
  DiffMP : Int64;
  ScreenRatio,
  ThisRatio,
  DiffRatio : real;
  index,
  closest : integer;
begin
	ScreenMP := Screen.Width * Screen.Height;
  ScreenRatio := Screen.Width / Screen.Height;
  DiffMP := 300000000;

  closest := 0;
  for index := 0 to 9 do begin
    ThisMP := Initials [index, 0] * Initials [index, 1];
    ThisRatio := Initials [index, 0] / Initials [index, 1];
    DiffRatio := abs (ThisRatio - ScreenRatio);
    if ((abs (ThisMP - ScreenMP) < DiffMP) and (DiffRatio < 0.2)) then begin
      DiffMP := abs (ThisMP - ScreenMP);
      closest := index;
    end;
  end;
  radScreen.ItemIndex := closest;
end;

procedure TfrmOptions.radScreenClick (Sender : TObject );
begin
  optScreenSize := radScreen.ItemIndex;
  InitialX := ((Initials [optScreenSize, 0] div 10) - 2) * 10;
  InitialY := ((Initials [optScreenSize, 1] div 10) - 8) * 10;
end;

end.

