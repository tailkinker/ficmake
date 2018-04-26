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

unit ftextpro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, LCLType,
  gprofile;

type

  { TfrmTextProfile }

  TfrmTextProfile = class(TForm)
    btnClose: TButton;
    btnOutDir: TBitBtn;
    chkFFMLCompliant: TCheckBox;
    chkBulkText: TCheckBox;
    chkUseTBL: TCheckBox;
    chkUsePreconv: TCheckBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    txtName: TLabeledEdit;
    txtOutDir: TLabeledEdit;
    txtSeparator: TLabeledEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOutDirClick(Sender: TObject);
    procedure chkBulkTextChange(Sender: TObject);
    procedure chkFFMLCompliantChange(Sender: TObject);
    procedure chkUsePreconvChange(Sender: TObject);
    procedure chkUseTBLChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtOutDirChange(Sender: TObject);
    procedure txtSeparatorChange(Sender: TObject);
  private
    { private declarations }
    t_profile : tTextProfile;
    procedure LoadProfile (aProfile : tTextProfile);
  public
    { public declarations }
    property Profile : tTextProfile read t_profile write LoadProfile;
  end;

implementation

uses
  doption;

{$R *.lfm}

{ TfrmTextProfile }

procedure TfrmTextProfile.LoadProfile (aProfile : tTextProfile);
begin
  t_profile := aProfile;
  txtName.Text := aProfile.Name;
  txtOutDir.Text := aProfile.OutputDir;
  txtSeparator.Text := aProfile.Separator;
  chkBulkText.Checked := aProfile.BulkText;
  chkFFMLCompliant.Checked := aProfile.FFMLCompliant;
  chkUsePreconv.Checked := aProfile.UsePreconv;
  chkUseTBL.Checked := aProfile.UseTBL;
end;

procedure TfrmTextProfile.FormCreate(Sender: TObject);
begin
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
end;

procedure TfrmTextProfile.btnOutDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtOutDir.Text := SelectDirectoryDialog1.Filename;
  end;
end;

procedure TfrmTextProfile.btnCloseClick(Sender: TObject);
begin
  if (DirectoryExists (Profile.OutputDir)) then
    Hide
  else
    Application.MessageBox
      ('Cannot save this profile with an invalid output directory',
      'Invalid Directory', MB_ICONHAND + MB_OK);
end;

procedure TfrmTextProfile.chkBulkTextChange(Sender: TObject);
begin
  Profile.BulkText := chkBulkText.Checked;
end;

procedure TfrmTextProfile.chkFFMLCompliantChange(Sender: TObject);
begin
  Profile.FFMLCompliant := chkFFMLCompliant.Checked;
end;

procedure TfrmTextProfile.chkUsePreconvChange(Sender: TObject);
begin
  Profile.UsePreconv := chkUsePreconv.Checked;
end;

procedure TfrmTextProfile.chkUseTBLChange(Sender: TObject);
begin
  Profile.UseTBL := chkUseTBL.Checked;
end;

procedure TfrmTextProfile.txtNameChange(Sender: TObject);
begin
  if (length (txtName.Text) > 0) then begin
    Profile.Name := txtName.Text;
    txtName.Color := clDefault;
    btnClose.Enabled := TRUE
  end else begin
    txtName.Color := clRed;
    btnClose.Enabled := FALSE;
  end;
end;

procedure TfrmTextProfile.txtOutDirChange(Sender: TObject);
begin
  if (txtOutDir.Enabled) then
    Profile.OutputDir := txtOutDir.Text;
end;

procedure TfrmTextProfile.txtSeparatorChange(Sender: TObject);
begin
  Profile.Separator := txtSeparator.Text;
end;

end.

