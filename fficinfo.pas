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

unit fficinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ExtDlgs, gstory;

type

  { TfrmStoryInfo }

  TfrmStoryInfo = class (TForm )
    btnOk : TButton;
    btnOpenSource : TBitBtn;
    btnOpenTitle : TBitBtn;
    btnOpenCover : TBitBtn;
    chkAutoFileName: TCheckBox;
    chkSubtitleFirst : TCheckBox;
    chkSuppressAuthor : TCheckBox;
    chkSuppressTitles : TCheckBox;
    OpenPicture : TOpenPictureDialog;
    SelectDirectory : TSelectDirectoryDialog;
    txtAuthor : TLabeledEdit;
    txtCoverPicture : TLabeledEdit;
    txtLongName : TLabeledEdit;
    txtOmakeHeader : TLabeledEdit;
    txtShortName : TLabeledEdit;
    txtSourceDir : TLabeledEdit;
    txtSubtitle : TLabeledEdit;
    txtTitlePicture : TLabeledEdit;
    txtTrailerHeader : TLabeledEdit;
    procedure btnOkClick(Sender: TObject);
    procedure btnOpenCoverClick (Sender : TObject );
    procedure btnOpenSourceClick (Sender : TObject );
    procedure btnOpenTitleClick (Sender : TObject );
    procedure chkAutoFileNameChange(Sender: TObject);
    procedure chkSubtitleFirstChange (Sender : TObject );
    procedure chkSuppressAuthorChange (Sender : TObject );
    procedure chkSuppressTitlesChange (Sender : TObject );
    procedure FormResize (Sender : TObject );
    procedure txtAuthorChange (Sender : TObject );
    procedure txtCoverPictureChange (Sender : TObject );
    procedure txtLongNameChange (Sender : TObject );
    procedure txtOmakeHeaderChange (Sender : TObject );
    procedure txtShortNameChange (Sender : TObject );
    procedure txtSourceDirChange (Sender : TObject );
    procedure txtSubtitleChange (Sender : TObject );
    procedure txtTitlePictureChange (Sender : TObject );
    procedure txtTrailerHeaderChange (Sender : TObject );
  private
    t_story : tStory;
    procedure SetStory (a_story : tStory);
  public
    property Story : tStory read t_story write SetStory;
  end;

implementation

uses
  LCLType;

{$R *.lfm}

procedure TfrmStoryInfo.btnOpenSourceClick (Sender : TObject );
begin
  if (SelectDirectory.Execute) then begin
    txtSourceDir.Text := SelectDirectory.Filename;
  end;
end;

procedure TfrmStoryInfo.btnOpenCoverClick (Sender : TObject );
begin
  if (OpenPicture.Execute) then begin
    txtCoverPicture.Text := OpenPicture.Filename;
  end;
end;

procedure TfrmStoryInfo.btnOkClick(Sender: TObject);
begin
  if (Story.ShortName = '') then
    Application.MessageBox (
      'Without a source file name, FicMake cannot create documents.',
      'Error', MB_ICONHAND + MB_OK)
  else if (Story.Title = '') then
    Application.MessageBox (
      'Without a title, FicMake cannot create documents.',
      'Error', MB_ICONHAND + MB_OK)
  else if (DirectoryExists (Story.SourceDir)) then begin
    if (FileExists (Story.SourceDir + '/stories.fic')) then
      Application.MessageBox (
        'The selected Source Directory is a Volume Directory.  Please select a different Source Directory.',
        'Error', MB_ICONEXCLAMATION + MB_OK)
    else begin
      ModalResult := mrOk;
      Close
    end
  end else
    Application.MessageBox
      ('Cannot save a story with an invalid source directory.',
      'Invalid Directory', MB_ICONQUESTION + MB_OK);
end;

procedure TfrmStoryInfo.btnOpenTitleClick (Sender : TObject );
begin
  if (OpenPicture.Execute) then begin
    txtTitlePicture.Text := OpenPicture.Filename;
  end;
end;

procedure TfrmStoryInfo.chkAutoFileNameChange(Sender: TObject);
begin
  if (chkAutoFileName.Enabled) then
    Story.AutoFileName := chkAutoFileName.Checked;
end;

procedure TfrmStoryInfo.chkSubtitleFirstChange (Sender : TObject );
begin
  if (chkSubtitleFirst.Enabled) then
  	Story.SubtitleFirst := chkSubtitleFirst.Checked;
end;

procedure TfrmStoryInfo.chkSuppressAuthorChange (Sender : TObject );
begin
  if (chkSuppressAuthor.Enabled) then
  	Story.SuppressAuthor := chkSuppressAuthor.Checked;
end;

procedure TfrmStoryInfo.chkSuppressTitlesChange (Sender : TObject );
begin
  if (chkSuppressTitles.Enabled) then begin
  	Story.SuppressTitles := chkSuppressTitles.Checked;
    chkSubtitleFirst.Enabled := not chkSuppressTitles.Checked;
  end;
end;

procedure TfrmStoryInfo.FormResize (Sender : TObject );
begin
  btnOk.Left := (Width - btnOk.Width) - 8;
  btnOk.Top := (Height - btnOk.Height) - 8;
end;

procedure TfrmStoryInfo.txtAuthorChange (Sender : TObject );
begin
  if (txtAuthor.Enabled) then
  	Story.Author := txtAuthor.Text;
end;

procedure TfrmStoryInfo.txtCoverPictureChange (Sender : TObject );
begin
  if (txtCoverPicture.Enabled) then
  	Story.CoverPicture := txtCoverPicture.Text;
end;

procedure TfrmStoryInfo.txtLongNameChange (Sender : TObject );
begin
  if (txtLongName.Enabled) then
  	Story.LongName := txtLongName.Text;
end;

procedure TfrmStoryInfo.txtOmakeHeaderChange (Sender : TObject );
begin
  if (txtOmakeHeader.Enabled) then
  	Story.OmakeHeader := txtOmakeHeader.Text;
end;

procedure TfrmStoryInfo.txtShortNameChange (Sender : TObject );
begin
  if (txtShortName.Enabled) then
  	Story.ShortName := txtShortName.Text;
end;

procedure TfrmStoryInfo.txtSourceDirChange (Sender : TObject );
begin
  if (txtSourceDir.Enabled) then
  	Story.SourceDir := txtSourceDir.Text;
  if (FileExists (Story.SourceDir + '/stories.fic')) then begin
    txtSourceDir.Color := clRed;
    btnOK.Enabled := FALSE
  end else begin
    txtSourceDir.Color := clDefault;
    btnOK.Enabled := TRUE
  end;
end;

procedure TfrmStoryInfo.txtSubtitleChange (Sender : TObject );
begin
  if (txtSubtitle.Enabled) then
  	Story.Subtitle := txtSubtitle.Text;
end;

procedure TfrmStoryInfo.txtTitlePictureChange (Sender : TObject );
begin
  if (txtTitlePicture.Enabled) then
  	Story.TitlePicture := txtTitlePicture.Text;
end;

procedure TfrmStoryInfo.txtTrailerHeaderChange (Sender : TObject );
begin
  if (txtTrailerHeader.Enabled) then
  	Story.TrailerHeader := txtTrailerHeader.Text;
end;

procedure TfrmStoryInfo.SetStory (a_story : tStory);
begin
  if (a_story <> nil) then begin
    t_story := a_story;
    Caption := 'Information for "' + t_story.Title + '"';

    // Disable all controls
    txtSubtitle.Enabled := FALSE;
    txtAuthor.Enabled := FALSE;
    txtSourceDir.Enabled := FALSE;
    txtTitlePicture.Enabled := FALSE;
    txtCoverPicture.Enabled := FALSE;
    txtLongName.Enabled := FALSE;
    txtShortName.Enabled := FALSE;
    txtOmakeHeader.Enabled := FALSE;
    txtTrailerHeader.Enabled := FALSE;
    chkSubtitleFirst.Enabled := FALSE;
    chkSuppressTitles.Enabled := FALSE;
    chkSuppressAuthor.Enabled := FALSE;
    chkAutoFileName.Enabled := FALSE;

    // Load all controls
    txtSubtitle.Text := t_story.Subtitle;
    txtAuthor.Text := t_story.Author;
    txtSourceDir.Text := t_story.SourceDir;
    txtTitlePicture.Text := t_story.TitlePicture;
    txtCoverPicture.Text := t_story.CoverPicture;
    txtLongName.Text := t_story.LongName;
    txtShortName.Text := t_story.ShortName;
    txtOmakeHeader.Text := t_story.OmakeHeader;
    txtTrailerHeader.Text := t_story.TrailerHeader;
    chkSubtitleFirst.Checked := t_story.SubtitleFirst;
    chkSuppressTitles.Checked := t_story.SuppressTitles;
    chkSuppressAuthor.Checked := t_story.SuppressAuthor;
    chkAutoFileName.Checked := t_story.AutoFileName;

    // Enable all controls
    txtSubtitle.Enabled := TRUE;
    txtAuthor.Enabled := TRUE;
    txtSourceDir.Enabled := TRUE;
    txtTitlePicture.Enabled := TRUE;
    txtCoverPicture.Enabled := TRUE;
    txtLongName.Enabled := TRUE;
    txtShortName.Enabled := TRUE;
    txtOmakeHeader.Enabled := TRUE;
    txtTrailerHeader.Enabled := TRUE;
    chkSubtitleFirst.Enabled := TRUE;
    chkSuppressTitles.Enabled := TRUE;
    chkSuppressAuthor.Enabled := TRUE;
    chkAutoFileName.Enabled := TRUE;
  end;
end;

end.

