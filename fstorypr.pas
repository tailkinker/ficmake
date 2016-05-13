unit fstorypr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,
  gprofile;

type

  { TfrmStoryProfiles }

  TfrmStoryProfiles = class(TForm)
    btnAddProfile: TBitBtn;
    btnDeleteProfile: TBitBtn;
    btnImportProfile: TBitBtn;
    btnSaveProfiles: TBitBtn;
    Label1: TLabel;
    lstProfiles: TListBox;
    procedure btnAddProfileClick(Sender: TObject);
    procedure btnDeleteProfileClick(Sender: TObject);
    procedure btnImportProfileClick(Sender: TObject);
    procedure btnSaveProfilesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lstProfilesClick(Sender: TObject);
    procedure lstProfilesDblClick(Sender: TObject);
    procedure lstProfilesKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    t_basedir : string;
    procedure SetBaseDir (aDir : string);
    procedure PopulateProfileList;
  public
    Profiles : tProfileList;
    GlobalProfiles : tProfileList;
    property BaseDir : string read t_basedir write SetBaseDir;
    { public declarations }
  end;

implementation

uses
  LCLType,
  fimprof, fnewprof,
  doption;

{$R *.lfm}

{ TfrmStoryProfiles }

procedure TfrmStoryProfiles.SetBaseDir (aDir : string);
begin
  t_basedir := aDir;
  Profiles.BaseDir := aDir;
  Profiles.Load;
  PopulateProfileList;
end;

procedure TfrmStoryProfiles.FormCreate(Sender: TObject);
begin
  Width := optInitialX div 2;
  Height := optInitialY;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  Profiles := tProfileList.Create;
end;

procedure TfrmStoryProfiles.btnAddProfileClick(Sender: TObject);
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

procedure TfrmStoryProfiles.btnDeleteProfileClick(Sender: TObject);
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

procedure TfrmStoryProfiles.btnImportProfileClick(Sender: TObject);
var
  NewDialog : TfrmImportProfile;
begin
  NewDialog := TfrmImportProfile.Create (Application);
  NewDialog.Profiles := GlobalProfiles;
  if (NewDialog.ShowModal = mrOK) then begin
    Profiles.Add (NewDialog.SelectedProfile);
    Profiles.MarkDirty;
    btnSaveProfiles.Enabled := TRUE;
    PopulateProfileList;
  end;
  NewDialog.Free;
end;

procedure TfrmStoryProfiles.btnSaveProfilesClick(Sender: TObject);
begin
  Profiles.Save;
  btnSaveProfiles.Enabled := FALSE;
end;

procedure TfrmStoryProfiles.FormDestroy(Sender: TObject);
begin
  Profiles.Destroy;
end;

procedure TfrmStoryProfiles.FormResize(Sender: TObject);
begin
  lstProfiles.Top := 32;
  lstProfiles.Left := 8;
  lstProfiles.Width := Width - 16;
  lstProfiles.Height := Height - (btnAddProfile.Height + 48);

  btnImportProfile.Left := (Width - btnImportProfile.Width) - 8;
  btnImportProfile.Top := lstProfiles.Top - (btnImportProfile.Height + 8);
  btnAddProfile.Left := lstProfiles.Left;
  btnDeleteProfile.Left := (Width - btnDeleteProfile.Width) - 8;
  btnAddProfile.Top := Height - 44;
  btnDeleteProfile.Top := Height - 44;
  btnSaveProfiles.Top := Height - 44;
  btnSaveProfiles.Left := ((lstProfiles.Width - btnSaveProfiles.Width)
  	div 3 * 2) + lstProfiles.Left;
  btnImportProfile.Left := ((lstProfiles.Width - btnSaveProfiles.Width)
  	div 3) + lstProfiles.Left;
  btnImportProfile.Top := Height - 44;
end;

procedure TfrmStoryProfiles.lstProfilesClick(Sender: TObject);
var
  index : integer;
  s : string;
begin
	index := lstProfiles.ItemIndex;
  btnDeleteProfile.Enabled := FALSE;

  if (index in [0..lstProfiles.Items.Count]) then begin
    s := lstProfiles.Items [index];
    Profiles.Select (s);
    btnDeleteProfile.Enabled := TRUE;
  end;
end;

procedure TfrmStoryProfiles.lstProfilesDblClick(Sender: TObject);
var
  index : integer;
  s : string;
begin
  index := lstProfiles.ItemIndex;
  if (index in [0..lstProfiles.Items.Count]) then begin
    s := lstProfiles.Items [index];
    Profiles.Select (s);
    Profiles.Current.Edit;
    Profiles.MarkDirty;
    btnSaveProfiles.Enabled := TRUE;
    PopulateProfileList;
  end;
end;

procedure TfrmStoryProfiles.lstProfilesKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
     lstProfilesDblClick (Sender);
end;

procedure TfrmStoryProfiles.PopulateProfileList;
var
  index : integer;
begin
  lstProfiles.Items.Clear;
  if (Profiles.Count > 0) then
    for index := 0 to (Profiles.Count - 1) do
      lstProfiles.Items.Add (Profiles.Name (index));
end;

end.

