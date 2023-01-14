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

unit fbaredit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TfrmBareEditor }

  TfrmBareEditor = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    txtEditor: TMemo;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure txtEditorChange(Sender: TObject);
    procedure txtEditorKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    t_filename : string;
    procedure SaveCurrentEdit;
    procedure LoadFileName (aFileName : string);
  public
    { public declarations }
    Dirty : boolean;
    property FileName : string read t_filename write LoadFileName;
  end;

var
  frmBareEditor: TfrmBareEditor;

implementation

{$R *.lfm}

uses
  LCLType,
  doption;

{ TfrmBareEditor }

procedure TfrmBareEditor.btnOKClick(Sender: TObject);
begin
  SaveCurrentEdit;
  ModalResult := mrOK;
end;

procedure TfrmBareEditor.btnCancelClick(Sender: TObject);
begin
  if ((NOT Dirty) OR (Application.MessageBox ('Close this editor without saving?', 'Cancel',
    MB_ICONQUESTION + MB_YESNO) = IDYES)) then
    ModalResult := mrCancel;
end;

procedure TfrmBareEditor.FormCreate(Sender: TObject);
begin
  txtEditor.Font.Size := optFontSize;
  Width := optInitialX;
  Height := optInitialY;
  case (optAnchorWindow) of
    0 :
      begin
        Left := 0;
        Top := 0;
      end;
    1 :
      begin
        Left := 0;
        Top := (Screen.Height - Height) div 2;
      end;
    2 :
      begin
        Left := (Screen.Width - Width) div 2;
        Top := Screen.Height - Height;
      end;
    3 :
      begin
        Left := (Screen.Width - Width) div 2;
        Top := 0;
      end;
    4 :
      begin
        Left := (Screen.Width - Width) div 2;
        Top := (Screen.Height - Height) div 2;
      end;
    5 :
      begin
        Left := (Screen.Width - Width) div 2;
        Top := Screen.Height - Height;
      end;
    6 :
      begin
        Left := Screen.Width - Width;
        Top := 0;
      end;
    7 :
      begin
        Left := Screen.Width - Width;
        Top := (Screen.Height - Height) div 2;
      end;
    8 :
      begin
        Left := Screen.Width - Width;
        Top := Screen.Height - Height;
      end;
  end;
  if (optMaximize) then
    WindowState := wsMaximized;
end;

procedure TfrmBareEditor.txtEditorChange(Sender: TObject);
begin
  Dirty := TRUE;
end;

procedure TfrmBareEditor.txtEditorKeyPress(Sender: TObject; var Key: char);
var
  s : string;
  SavePos : TPoint;
begin
  with (txtEditor) do begin
	  if (Key = #2) then
      if (SelLength > 0) then begin
        s := '\fB' + SelText + '\fR';
        SelText := s;
      end else begin
    	  SavePos := CaretPos;
    	  s := Lines [CaretPos.Y];
        s := copy (s, 1, CaretPos.X)
      	  + '\fB'
          + copy (s, CaretPos.X + 1, length (s) - CaretPos.X);
        Lines [CaretPos.Y] := s;
        SavePos.X += 3;
        CaretPos := SavePos
      end;
    if (Key = #13) then
      if (CaretPos.X = 0) then begin
        Lines.Insert (CaretPos.Y, '.PP');
        Key := #0
      end;
	  if (Key = #14) then
      if (SelLength > 0) then begin
        s := '\fI' + SelText + '\fR';
        SelText := s;
      end else begin
    	  SavePos := CaretPos;
    	  s := Lines [CaretPos.Y];
        s := copy (s, 1, CaretPos.X)
      	  + '\fI'
          + copy (s, CaretPos.X + 1, length (s) - CaretPos.X);
        Lines [CaretPos.Y] := s;
        SavePos.X += 3;
        CaretPos := SavePos
      end;
	  if (Key = #18) then begin
    	SavePos := CaretPos;
    	s := Lines [CaretPos.Y];
      s := copy (s, 1, CaretPos.X)
      	+ '\fR'
        + copy (s, CaretPos.X + 1, length (s) - CaretPos.X);
      Lines [CaretPos.Y] := s;
      SavePos.X += 3;
      CaretPos := SavePos
    end;
//    if (Key = #23) then
//      frmMain.btnWordCountClick (frmMain.txtEditor);
    if (Key = #26) then
      if (CanUndo) then
        Undo;
  end;
  if (Key = #19) then
  	SaveCurrentEdit;
end;

procedure TfrmBareEditor.SaveCurrentEdit;
begin
  txtEditor.Lines.SaveToFile (FileName);
  Dirty := FALSE;
end;

procedure TfrmBareEditor.LoadFileName (aFileName : string);
begin
  t_filename := aFileName;
  if (FileExists (aFileName)) then
    txtEditor.Lines.LoadFromFile (aFileName);
  Dirty := FALSE;
end;

end.

