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

unit fvolume;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons,
  gvolume;

type

  { TfrmVolume }

  TfrmVolume = class(TForm)
    btnAddVolume : TBitBtn;
    btnDeleteVolume : TBitBtn;
    btnSelectDir: TBitBtn;
    chkHTMLIndex: TCheckBox;
    Label1: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    txtVolumeName: TLabeledEdit;
    lstVolumes: TListBox;
    txtDirectory: TLabeledEdit;
    txtAuthor: TLabeledEdit;
    procedure btnSelectDirClick(Sender: TObject);
    procedure FormCreate (Sender : TObject );
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmVolume: TfrmVolume;

implementation

{$R *.lfm}

{ TfrmVolume }

procedure TfrmVolume.FormResize(Sender: TObject);
begin
  lstVolumes.Width := (frmVolume.Width - 24) div 2;
  lstVolumes.Height := frmVolume.Height - 236;

  txtVolumeName.Width := (frmVolume.Width - 24) div 2;

  txtDirectory.Width := (frmVolume.Width - 24) div 2 - 32;
  txtDirectory.Left := (frmVolume.Width - 24) div 2 + 16;

  txtAuthor.Width := (frmVolume.Width - 24) div 2;

  btnSelectDir.Left := (frmVolume.Width - 40);
  btnAddVolume.Top := frmVolume.Height - 44;
  btnDeleteVolume.Top := frmVolume.Height - 44;
  btnDeleteVolume.Left := lstVolumes.Width - 28;
end;

procedure TfrmVolume.btnSelectDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtDirectory.Text := SelectDirectoryDialog1.Filename;
  end;
end;

procedure TfrmVolume.FormCreate (Sender : TObject );
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

end.

