unit fhtmlpro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls,
  gprofile;

type

  { TfrmHTMLProfile }

  TfrmHTMLProfile = class(TForm)
    btnClose: TButton;
    btnOutDir: TBitBtn;
    chkUsePreconv: TCheckBox;
    chkBulkHTML: TCheckBox;
    chkSingleLineBooks: TCheckBox;
    chkBookFilesInIndex: TCheckBox;
    chkDisclaimerInIndex: TCheckBox;
    chkSeparateBookFiles: TCheckBox;
    chkDisclaimerInChapter: TCheckBox;
    chkIsFFNet: TCheckBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    txtBackLink: TLabeledEdit;
    txtBackLinkText: TLabeledEdit;
    txtName: TLabeledEdit;
    txtOutDir: TLabeledEdit;
    txtSeparator: TLabeledEdit;
    procedure btnOutDirClick(Sender: TObject);
    procedure chkBookFilesInIndexChange(Sender: TObject);
    procedure chkBulkHTMLChange(Sender: TObject);
    procedure chkDisclaimerInChapterChange(Sender: TObject);
    procedure chkDisclaimerInIndexChange(Sender: TObject);
    procedure chkIsFFNetChange(Sender: TObject);
    procedure chkSeparateBookFilesChange(Sender: TObject);
    procedure chkSingleLineBooksChange(Sender: TObject);
    procedure chkUsePreconvChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure txtBackLinkChange(Sender: TObject);
    procedure txtBackLinkTextChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtOutDirChange(Sender: TObject);
    procedure txtSeparatorChange(Sender: TObject);
  private
    { private declarations }
    t_profile : tHTMLProfile;
    procedure LoadProfile (aProfile : tHTMLProfile);
    procedure AdjustVisibility;
  public
    property Profile : tHTMLProfile read t_profile write LoadProfile;
    { public declarations }
  end;

var
  frmHTMLProfile: TfrmHTMLProfile;

implementation

{$R *.lfm}

{ TfrmHTMLProfile }

procedure TfrmHTMLProfile.LoadProfile (aProfile : tHTMLProfile);
begin
  t_Profile := aProfile;
  with (t_Profile) do begin
    txtName.Text := Name;
    txtOutDir.Text := OutputDir;
    txtBackLink.Text := BackLink;
    txtBackLinkText.Text := BackLinkText;
    txtSeparator.Text := Separator;

    chkIsFFNet.Checked := IsFFNet;
    chkDisclaimerInChapter.Checked := DisclaimerInChapter;
    chkSeparateBookFiles.Checked := SeparateBookFiles;
    chkDisclaimerInIndex.Checked := DisclaimerInIndex;
    chkBookFilesInIndex.Checked := BookFilesInIndex;
    chkSingleLineBooks.Checked := SingleLineBooks;
    chkBulkHTML.Checked := BulkHTML;
    chkUsePreconv.Checked := UsePreconv;
  end;
  AdjustVisibility;
end;

procedure TfrmHTMLProfile.AdjustVisibility;
begin
  chkBulkHTML.Enabled := NOT (chkIsFFNet.Checked);
  if (NOT (chkBulkHTML.Enabled)) then
    chkBulkHTML.Checked := FALSE;
  chkDisclaimerInIndex.Enabled := NOT (chkIsFFNet.Checked OR chkBulkHTML.Checked);
  chkBookFilesInIndex.Enabled := NOT (chkIsFFNet.Checked OR chkBulkHTML.Checked);
  chkSingleLineBooks.Enabled := NOT (chkIsFFNet.Checked OR chkBulkHTML.Checked);
  chkDisclaimerInChapter.Enabled := NOT (chkBulkHTML.Checked);
  chkSeparateBookFiles.Enabled := NOT (chkBulkHTML.Checked);
  txtSeparator.Enabled := NOT (chkIsFFNet.Checked);
end;

procedure TfrmHTMLProfile.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmHTMLProfile.txtBackLinkChange(Sender: TObject);
begin
  Profile.BackLink := txtBackLink.Text;
end;

procedure TfrmHTMLProfile.txtBackLinkTextChange(Sender: TObject);
begin
  Profile.BackLinkText := txtBackLinkText.Text;
end;

procedure TfrmHTMLProfile.btnOutDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtOutDir.Text := SelectDirectoryDialog1.Filename;
  end;
end;

procedure TfrmHTMLProfile.chkBookFilesInIndexChange(Sender: TObject);
begin
  Profile.BookFilesInIndex := chkBookFilesInIndex.Checked;
  AdjustVisibility;
end;

procedure TfrmHTMLProfile.chkBulkHTMLChange(Sender: TObject);
begin
  Profile.BulkHTML := chkBulkHTML.Checked;
  AdjustVisibility;
end;

procedure TfrmHTMLProfile.chkDisclaimerInChapterChange(Sender: TObject);
begin
  Profile.DisclaimerInChapter := chkDisclaimerInChapter.Checked;
  AdjustVisibility;
end;

procedure TfrmHTMLProfile.chkDisclaimerInIndexChange(Sender: TObject);
begin
  Profile.DisclaimerInIndex := chkDisclaimerInIndex.Checked;
  AdjustVisibility;
end;

procedure TfrmHTMLProfile.chkIsFFNetChange(Sender: TObject);
begin
  Profile.IsFFNet := chkIsFFNet.Checked;
  AdjustVisibility;
end;

procedure TfrmHTMLProfile.chkSeparateBookFilesChange(Sender: TObject);
begin
  Profile.SeparateBookFiles := chkSeparateBookFiles.Checked;
  AdjustVisibility;
end;

procedure TfrmHTMLProfile.chkSingleLineBooksChange(Sender: TObject);
begin
  Profile.SingleLineBooks := chkSingleLineBooks.Checked;
end;

procedure TfrmHTMLProfile.chkUsePreconvChange(Sender: TObject);
begin
  Profile.UsePreconv := chkUsePreconv.Checked;
  AdjustVisibility;
end;

procedure TfrmHTMLProfile.txtNameChange(Sender: TObject);
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

procedure TfrmHTMLProfile.txtOutDirChange(Sender: TObject);
begin
  if (txtOutDir.Enabled) then
    Profile.OutputDir := txtOutDir.Text;
end;

procedure TfrmHTMLProfile.txtSeparatorChange(Sender: TObject);
begin
  Profile.Separator := txtSeparator.Text;
end;

end.

