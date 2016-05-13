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
  flog,
  gvolume, gprofile;

type

  { TfrmVolume }

  TfrmVolume = class(TForm)
    btnAbout : TButton;
    btnAddVolume : TBitBtn;
    btnAddProfile : TBitBtn;
    btnDeleteVolume : TBitBtn;
    btnDeleteProfile : TBitBtn;
    btnOptions : TButton;
    btnSaveVolumes : TBitBtn;
    btnSaveProfiles : TBitBtn;
    btnSelectDir: TBitBtn;
    chkHTMLIndex: TCheckBox;
    Label1: TLabel;
    labProfile : TLabel;
    lstGlobalProfiles : TListBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    txtVolumeName: TLabeledEdit;
    lstVolumes: TListBox;
    txtDirectory: TLabeledEdit;
    txtAuthor: TLabeledEdit;
    procedure btnAboutClick(Sender: TObject);
    procedure btnAddProfileClick (Sender : TObject );
    procedure btnAddVolumeClick (Sender : TObject );
    procedure btnDeleteProfileClick (Sender : TObject );
    procedure btnDeleteVolumeClick (Sender : TObject );
    procedure btnOptionsClick (Sender : TObject );
    procedure btnSaveProfilesClick(Sender: TObject);
    procedure btnSaveVolumesClick (Sender : TObject );
    procedure btnSelectDirClick(Sender: TObject);
    procedure chkHTMLIndexChange (Sender : TObject );
    procedure FormClose (Sender : TObject; var CloseAction : TCloseAction );
    procedure FormCreate (Sender : TObject );
    procedure FormResize(Sender: TObject);
    procedure lstGlobalProfilesClick (Sender : TObject );
    procedure lstGlobalProfilesDblClick(Sender: TObject);
    procedure lstGlobalProfilesKeyPress(Sender: TObject; var Key: char);
    procedure lstVolumesClick (Sender : TObject );
    procedure lstVolumesDblClick (Sender : TObject );
    procedure lstVolumesKeyPress(Sender: TObject; var Key: char);
    procedure txtAuthorChange (Sender : TObject );
    procedure txtDirectoryChange (Sender : TObject );
    procedure txtVolumeNameChange (Sender : TObject );
  private
    { private declarations }
    Volumes : tVolumeList;
    Profiles : tProfileList;
    procedure PopulateVolumeList;
    procedure PopulateProfileList;
  public
    { public declarations }
  end;

var
  frmVolume: TfrmVolume;

implementation

uses
  LCLType,
  fabout, fnewvol, foptions, fstory, fnewprof, doption;

const
  Version = '1.1.4';

{$R *.lfm}

{ TfrmVolume }

procedure TfrmVolume.FormResize(Sender: TObject);
begin
  lstVolumes.Width := (Width - 24) div 2;
  lstVolumes.Height := Height - 228;

  txtVolumeName.Width := (Width - 24) div 2;

  txtDirectory.Width := (Width - 24) div 2 - 32;
  txtDirectory.Left := (Width - 24) div 2 + 16;

  txtAuthor.Width := (Width - 24) div 2;

  btnSelectDir.Left := (Width - 40);
  btnAddVolume.Top := Height - 44;
  btnDeleteVolume.Top := Height - 44;
  btnDeleteVolume.Left := (lstVolumes.Width - btnDeleteVolume.Width) + 8;

  labProfile.Left := (Width - 24) div 2 + 16;
  lstGlobalProfiles.Left := (Width - 24) div 2 + 16;
  lstGlobalProfiles.Width := (Width - 24) div 2;
  lstGlobalProfiles.Height := Height - 228;

  btnAddProfile.Left := lstGlobalProfiles.Left;
  btnDeleteProfile.Left := (Width - btnDeleteProfile.Width) - 8;
  btnAddProfile.Top := Height - 44;
  btnDeleteProfile.Top := Height - 44;

  btnSaveVolumes.Top := Height - 44;
  btnSaveVolumes.Left := (lstVolumes.Width - btnSaveVolumes.Width) div 2 + 8;
  btnSaveProfiles.Top := Height - 44;
  btnSaveProfiles.Left := (lstGlobalProfiles.Width - btnSaveProfiles.Width)
  	div 2 + lstGlobalProfiles.Left;

  btnAbout.Left := Width - 108;
  btnOptions.Left := Width - 108;
end;

procedure TfrmVolume.lstGlobalProfilesClick (Sender : TObject );
var
  index : integer;
  s : string;
begin
	index := lstGlobalProfiles.ItemIndex;
  btnDeleteProfile.Enabled := FALSE;

  if (index in [0..lstGlobalProfiles.Items.Count]) then begin
    s := lstGlobalProfiles.Items [index];
    Profiles.Select (s);
    btnDeleteProfile.Enabled := TRUE;
  end;
end;

procedure TfrmVolume.lstGlobalProfilesDblClick(Sender: TObject);
var
  index : integer;
  s : string;
begin
  index := lstGlobalProfiles.ItemIndex;
  if (index in [0..lstGlobalProfiles.Items.Count]) then begin
    s := lstGlobalProfiles.Items [index];
    Profiles.Select (s);
    Profiles.Current.Edit;
    Profiles.MarkDirty;
    btnSaveProfiles.Enabled := TRUE;
  end;
end;

procedure TfrmVolume.lstGlobalProfilesKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
     lstGlobalProfilesDblClick (Sender);
end;

procedure TfrmVolume.lstVolumesClick (Sender : TObject );
var
  index : integer;
  s : string;
begin
  index := lstVolumes.ItemIndex;
  btnDeleteVolume.Enabled := FALSE;
  btnSelectDir.Enabled := FALSE;

  if (index in [0..lstVolumes.Items.Count]) then begin
    s := lstVolumes.Items [index];
    Volumes.SelectVolume (s);

    // Disable Controls
    txtVolumeName.Enabled := FALSE;
    txtDirectory.Enabled := FALSE;
    txtAuthor.Enabled := FALSE;
    chkHTMLIndex.Enabled := FALSE;

    // Load Controls
    txtVolumeName.Text := Volumes.Current.VolumeName;
    txtDirectory.Text := Volumes.Current.BaseDir;
    txtAuthor.Text := Volumes.Current.Author;
    chkHTMLIndex.Checked := Volumes.Current.GenerateIndexFile;

    // Enable Controls
    btnDeleteVolume.Enabled := TRUE;
    txtVolumeName.Enabled := TRUE;
    txtDirectory.Enabled := TRUE;
    txtAuthor.Enabled := TRUE;
    chkHTMLIndex.Enabled := TRUE;
    btnSelectDir.Enabled := TRUE;
  end;
end;

procedure TfrmVolume.lstVolumesDblClick (Sender : TObject );
var
  NewStoryForm : TfrmStory;
  FormCaption : string;
  index : integer;
  Present : boolean;
begin
  if (Volumes.Current <> nil) then begin
	  FormCaption := 'Volume "' + Volumes.Current.VolumeName + '"';
    index := 0;
    Present := false;
    repeat
      if (Screen.Forms [index].Caption = FormCaption) then
        Present := true
      else
    	  index += 1;
    until (Present or (index >= Screen.FormCount));
    if (DirectoryExists(Volumes.Current.BaseDir)) then begin
      if (Present) then begin
        Screen.Forms [index].Show;
        Screen.Forms [index].BringToFront;
      end else begin
        NewStoryForm := TfrmStory.Create (Application);
        NewStoryForm.Caption := FormCaption;
        NewStoryForm.SetBaseDir (Volumes.Current.BaseDir);
        NewStoryForm.ForceLoadStoryList;
        NewStoryForm.GlobalProfiles := Profiles;
        NewStoryForm.Volume := Volumes.Current;
        NewStoryForm.Show;
      end
    end else
      Application.MessageBox
        ('Cannot open Volume with an invalid base directory',
      	'Invalid Directory', MB_ICONQUESTION + MB_OK);
  end;
end;

procedure TfrmVolume.lstVolumesKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
    lstVolumesDblClick (Sender);
end;

procedure TfrmVolume.txtAuthorChange (Sender : TObject );
begin
  if (txtAuthor.Enabled) then
    if (Volumes.Current <> nil) then begin
      Volumes.Current.Author := txtAuthor.Text;
      Volumes.MarkDirty;
      btnSaveVolumes.Enabled := true;
    end;
end;

procedure TfrmVolume.txtDirectoryChange (Sender : TObject );
begin
  if (txtDirectory.Enabled) then
    if (Volumes.Current <> nil) then begin
      Volumes.Current.BaseDir := txtDirectory.Text;
      Volumes.MarkDirty;
      btnSaveVolumes.Enabled := true;
    end;
end;

procedure TfrmVolume.txtVolumeNameChange (Sender : TObject );
begin
  if (txtVolumeName.Enabled) then
    if (Volumes.Current <> nil) then begin
    	Volumes.Current.VolumeName := txtVolumeName.Text;
      Volumes.MarkDirty;
      PopulateVolumeList;
      btnSaveVolumes.Enabled := true;
    end;
end;

procedure TfrmVolume.btnSelectDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtDirectory.Text := SelectDirectoryDialog1.Filename;
  end;
end;

procedure TfrmVolume.chkHTMLIndexChange (Sender : TObject );
begin
  if (chkHTMLIndex.Enabled) then
    if (Volumes.Current <> nil) then begin
      Volumes.Current.GenerateIndexFile := chkHTMLIndex.Checked;
      Volumes.MarkDirty;
      btnSaveVolumes.Enabled := true;
    end;
end;

procedure TfrmVolume.FormClose (Sender : TObject; var CloseAction : TCloseAction );
begin
  if (Volumes.Dirty) then
    if (Application.MessageBox ('Do you wish to save the Volume List?',
    	'Volume List Changed', MB_ICONQUESTION + MB_YESNO) = IDYES) then
      Volumes.SaveVolumeList;
end;

procedure TfrmVolume.btnAddVolumeClick (Sender : TObject );
var
  Dialog : TfrmNewVolume;
begin
	Dialog := TfrmNewVolume.Create (Application);
  if (Dialog.ShowModal = mrOK) then
  	Volumes.NewVolume (Dialog.txtName.Text);
  Dialog.Destroy;
  PopulateVolumeList;
end;

procedure TfrmVolume.btnDeleteProfileClick (Sender : TObject );
var
  s : string;
begin
  if (Profiles.Current <> nil) then begin
    s := 'Are you sure you wish to delete the Profile "'
    	+ Profiles.Current.Name + '"?';
    if (Application.MessageBox (pchar(s), 'Delete Profile',
    	MB_ICONQUESTION + MB_YESNO) = IDYES) then
    	Profiles.Delete;
      btnSaveProfiles.Enabled := TRUE;
      Profiles.MarkDirty;
  end;
  btnDeleteProfile.Enabled := FALSE;
  PopulateProfileList;
end;

procedure TfrmVolume.btnAddProfileClick (Sender : TObject );
var
  Dialog : TfrmNewProfile;
begin
	Dialog := TfrmNewProfile.Create (Application);
  if (Dialog.ShowModal = mrOK) then begin
    Profiles.Add (Dialog.NewProfile);
    Profiles.Edit;
    Profiles.MarkDirty;
    btnSaveProfiles.Enabled := TRUE;
  end;
  Dialog.Destroy;
  PopulateProfileList;
end;

procedure TfrmVolume.btnAboutClick(Sender: TObject);
begin
  with (TfrmAbout.Create (Application)) do begin
    labVersion.Caption := labVersion.Caption + Version;
    ShowModal;
    Free
  end;
end;

procedure TfrmVolume.btnDeleteVolumeClick (Sender : TObject );
var
  s : string;
begin
  if (Volumes.Current <> nil) then begin
    s := 'Are you sure you wish to delete the Volume "'
    	+ Volumes.Current.VolumeName + '"?';
    if (Application.MessageBox (pchar(s), 'Delete Volume',
    	MB_ICONQUESTION + MB_YESNO) = IDYES) then
    	Volumes.DeleteVolume;
  end;
  btnDeleteVolume.Enabled := FALSE;
  PopulateVolumeList;
end;

procedure TfrmVolume.btnOptionsClick (Sender : TObject );
var
  Dialog : TfrmOptions;
begin
	Dialog := TfrmOptions.Create (Application);
  Dialog.ShowModal;
  Dialog.Destroy;
  Width := optInitialX;
  Height := optInitialY;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmVolume.btnSaveProfilesClick(Sender: TObject);
begin
  Profiles.Save;
  btnSaveProfiles.Enabled := FALSE;
end;

procedure TfrmVolume.btnSaveVolumesClick (Sender : TObject );
begin
  Volumes.SaveVolumeList;
  btnSaveVolumes.Enabled := FALSE;
end;

procedure TfrmVolume.FormCreate (Sender : TObject );
begin
  Caption := Caption + Version;
  LoadOptions;
  Width := optInitialX;
  Height := optInitialY;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;

  Volumes := tVolumeList.Create;
  Volumes.LoadVolumeList;
  PopulateVolumeList;

  Profiles := tProfileList.Create;
  Profiles.BaseDir := GetEnvironmentVariable ('HOME') + '/.ficmake';
  Profiles.Load;
  PopulateProfileList;

  if (optOpenLogOnStart) then begin
    frmLog.Show;
    frmLog.BringToFront;
    frmLog.txtLog.Lines.Clear;
  end;
end;

procedure TfrmVolume.PopulateVolumeList;
var
  index : integer;
begin
  lstVolumes.Items.Clear;
  if (Volumes.Count > 0) then
    for index := 0 to (Volumes.Count - 1) do
  		lstVolumes.Items.Add (Volumes.VolumeName (index));
  btnSaveVolumes.Enabled := Volumes.Dirty;
end;

procedure TfrmVolume.PopulateProfileList;
var
  index : integer;
begin
  lstGlobalProfiles.Items.Clear;
  if (Profiles.Count > 0) then
    for index := 0 to (Profiles.Count - 1) do
      lstGlobalProfiles.Items.Add (Profiles.Name (index));
end;

end.

