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
    chkAssumeMakeOnSave: TCheckBox;
    chkOpenLogOnStart: TCheckBox;
    txtLoremIpsum: TMemo;
    txtFontSize: TLabeledEdit;
    radScreen : TRadioGroup;
    procedure btnGuessClick (Sender : TObject );
    procedure btnOkClick(Sender: TObject);
    procedure chkAssumeMakeOnSaveChange(Sender: TObject);
    procedure chkOpenLogOnStartChange(Sender: TObject);
    procedure FormCreate (Sender : TObject );
    procedure FormResize (Sender : TObject );
    procedure txtFontSizeChange(Sender: TObject);
    procedure radScreenClick (Sender : TObject );
    procedure txtLoremIpsumChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses
  doption;


{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.FormResize (Sender : TObject );
begin
  btnOk.Left := (Width - btnOk.Width) - 8;
  btnOk.Top := (Height - btnOk.Height) - 8;
  btnGuess.Top := (radScreen.Height + 16);
  txtLoremIpsum.Width := (Width - 16);
  txtFontSize.Left := (Width - txtFontSize.Width) - 8;
end;

procedure TfrmOptions.txtFontSizeChange(Sender: TObject);
var
  v : integer;
begin
  val (txtFontSize.Text, v);
  if (v = 0) then begin
    txtFontSize.Color := clRed;
    btnOK.Enabled := FALSE;
  end else begin
    optFontSize := v;
    txtFontSize.Color := clDefault;
    btnOK.Enabled := TRUE;
    txtLoremIpsum.Font.Size := v;
  end;
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
  radScreen.ItemIndex := optScreenSize;

  // Check Boxes
  chkAssumeMakeOnSave.Checked := optAssumeMakeOnSave;
  chkOpenLogOnStart.Checked := optOpenLogOnStart;

  // Font Size
  str (optFontSize, s);
  txtFontSize.Text := s;

  Width := radScreen.Width + 36 + chkOpenLogOnStart.Width;
  Height := (radScreen.Height + btnGuess.Height
    + txtLoremIpsum.Height + btnOK.Height) + 48;
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

procedure TfrmOptions.btnOkClick(Sender: TObject);
begin
  SaveOptions;
end;

procedure TfrmOptions.chkAssumeMakeOnSaveChange(Sender: TObject);
begin
  optAssumeMakeOnSave := chkAssumeMakeOnSave.Checked;
end;

procedure TfrmOptions.chkOpenLogOnStartChange(Sender: TObject);
begin
  optOpenLogOnStart := chkOpenLogOnStart.Checked;
end;

procedure TfrmOptions.radScreenClick (Sender : TObject );
begin
  optScreenSize := radScreen.ItemIndex;
  SetScreenSize;
end;

procedure TfrmOptions.txtLoremIpsumChange(Sender: TObject);
begin

end;

// Everything from here...
begin
  optInitialX := 780;
  optInitialY := 420;
// ..to here is to be deleted from production code.
end.

