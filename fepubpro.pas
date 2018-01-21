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

unit fepubpro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, LCLType,
  gprofile;

type

  { TfrmEPubProfile }

  TfrmEPubProfile = class(TForm)
    btnClose: TButton;
    btnOutDir: TBitBtn;
    chkBlurbInEPub: TCheckBox;
    chkUsePreconv: TCheckBox;
    labSeparator: TLabel;
    labOutDir: TLabel;
    labEPubSeries: TLabel;
    labName: TLabel;
    txtName: TEdit;
    txtOutDir: TEdit;
    txtEPubSeries: TEdit;
    txtSeparator: TEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOutDirClick(Sender: TObject);
    procedure chkBlurbInEPubChange(Sender: TObject);
    procedure chkUsePreconvChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure txtEPubSeriesChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtOutDirChange(Sender: TObject);
    procedure txtSeparatorChange(Sender: TObject);
  private
    { private declarations }
    t_profile : tEPubProfile;
    procedure LoadProfile (aProfile : tEPubProfile);
  public
    { public declarations }
    property Profile : tEPubProfile read t_profile write LoadProfile;
  end;

implementation

procedure TfrmEPubProfile.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmEPubProfile.txtEPubSeriesChange(Sender: TObject);
begin
  Profile.EPubSeries := txtEPubSeries.Text;
end;

procedure TfrmEPubProfile.btnOutDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtOutDir.Text := SelectDirectoryDialog1.Filename;
  end;
end;

procedure TfrmEPubProfile.btnCloseClick(Sender: TObject);
begin
  if (DirectoryExists (Profile.OutputDir)) then
    Hide
  else
    Application.MessageBox
      ('Cannot save this profile with an invalid output directory',
      'Invalid Directory', MB_ICONHAND + MB_OK);
end;

procedure TfrmEPubProfile.chkBlurbInEPubChange(Sender: TObject);
begin
  Profile.BlurbInEPub := chkBlurbInEPub.Checked;
end;

procedure TfrmEPubProfile.chkUsePreconvChange(Sender: TObject);
begin
  Profile.UsePreconv := chkUsePreconv.Checked;
end;

procedure TfrmEPubProfile.txtNameChange(Sender: TObject);
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

procedure TfrmEPubProfile.txtOutDirChange(Sender: TObject);
begin
  if (txtOutDir.Enabled) then
    Profile.OutputDir := txtOutDir.Text;
end;

procedure TfrmEPubProfile.txtSeparatorChange(Sender: TObject);
begin
  Profile.Separator := txtSeparator.Text;
end;

procedure TfrmEPubProfile.LoadProfile (aProfile : tEPubProfile);
begin
  t_profile := aProfile;
  txtName.Text := aProfile.Name;
  txtOutDir.Text := aProfile.OutputDir;
  txtSeparator.Text := aProfile.Separator;
  txtEPubSeries.Text := aProfile.EPubSeries;
  chkBlurbInEPub.Checked := aProfile.BlurbInEPub;
  chkUsePreconv.Checked := aProfile.UsePreconv;

end;

{$R *.lfm}

end.

