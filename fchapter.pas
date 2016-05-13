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

unit fchapter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, gchapter, gstory, gprofile;

type

  { TfrmChapter }

  TfrmChapter = class (TForm )
    btnChapterUp: TBitBtn;
    btnAddChapter : TBitBtn;
    btnChapterDown: TBitBtn;
    btnDeleteChapter : TBitBtn;
    btnSaveChapters : TBitBtn;
    btnBuild: TButton;
    btnMake: TButton;
    btnStoryProfiles: TButton;
    chkNoTitle: TCheckBox;
    chkIsABook : TCheckBox;
    chkSubtitleFirst : TCheckBox;
    Label1 : TLabel;
    lstChapters : TListBox;
    txtFilename : TLabeledEdit;
    txtSubtitle : TLabeledEdit;
    txtTitle : TLabeledEdit;
    procedure btnAddChapterClick (Sender : TObject );
    procedure btnBuildClick(Sender: TObject);
    procedure btnChapterDownClick(Sender: TObject);
    procedure btnChapterUpClick(Sender: TObject);
    procedure btnDeleteChapterClick (Sender : TObject );
    procedure btnMakeClick(Sender: TObject);
    procedure btnSaveChaptersClick (Sender : TObject );
    procedure btnStoryProfilesClick(Sender: TObject);
    procedure chkIsABookChange(Sender: TObject);
    procedure chkNoTitleChange(Sender: TObject);
    procedure chkSubtitleFirstChange(Sender: TObject);
    procedure FormClose (Sender : TObject; var CloseAction : TCloseAction );
    procedure FormCreate (Sender : TObject );
    procedure FormResize (Sender : TObject );
    procedure lstChaptersClick (Sender : TObject );
    procedure lstChaptersDblClick(Sender: TObject);
    procedure lstChaptersKeyPress(Sender: TObject; var Key: char);
    procedure txtFilenameChange(Sender: TObject);
    procedure txtSubtitleChange(Sender: TObject);
    procedure txtTitleChange(Sender: TObject);
  private
    Chapters : tChapterList;
    t_shortname : string;
    procedure PopulateChapterList;
  public
    Story : tStory;
    Profiles : tProfileList;
    property ShortName : string read t_shortname write t_shortname;
    procedure SetBaseDir (aDir : string);
    procedure ForceChapterListLoad;
  end;

implementation

uses
  LCLType,
  doption, gmake, fstorypr, fnewchap, feditor, flog;

{$R *.lfm}

{ TfrmChapter }

procedure TfrmChapter.FormClose (Sender : TObject; var CloseAction : TCloseAction );
begin
  if (Chapters.Dirty) then
    if (Application.MessageBox ('Do you wish to save the Chapters List?',
    	'Chapters List Changed', MB_ICONQUESTION + MB_YESNO) = IDYES) then
      Chapters.Save;
end;

procedure TfrmChapter.btnAddChapterClick (Sender : TObject );
var
  Dialog : TfrmNewChapter;
  t : string;
begin
	Dialog := TfrmNewChapter.Create (Application);
  if (Dialog.ShowModal = mrOK) then begin
  	Chapters.New (Dialog.txtName.Text);
    Chapters.SelectAt (Chapters.Count - 1);
    if (Story.AutoFileName) then begin
      str (Chapters.Count, t);
      while (length (t) < 4) do
        t := '0' + t;
      Chapters.Current.Filename := Story.ShortName + t;
    end;
    btnSaveChapters.Enabled := TRUE;
  end;
  Dialog.Destroy;
  PopulateChapterList;
end;

procedure TfrmChapter.btnBuildClick(Sender: TObject);
var
  StoryProfiles : tProfileList;
  limit,
  index : integer;
  s : string;
begin
  if (Chapters.Dirty) then begin
    s := 'Your Chapters List has not been saved.  Building the document without'
      + ' first saving the Chapters List will not update the document.  Do you '
      + 'wish to save the Chapters List before Building?';
    if (Application.MessageBox (pchar (s), 'Save Chapters List',
      MB_ICONQUESTION + MB_YESNO) = IDYES) then begin
        Chapters.Save;
        btnSaveChapters.Enabled := FALSE;
      end;
  end;
  s := 'For Story ''' + Story.Title + ''':';
  frmLog.Show;
  frmLog.BringToFront;
  frmLog.txtLog.Lines.Clear;
  frmLog.txtLog.Lines.Add (s);

  StoryProfiles := tProfileList.Create;
  StoryProfiles.BaseDir := Story.SourceDir;
  StoryProfiles.Load;
  limit := StoryProfiles.Count;
  if (limit > 0) then
    for index := 0 to (limit - 1) do begin
      StoryProfiles.SelectAt (index);
      StoryProfiles.Current.Build (Story);
    end;
  StoryProfiles.Free;
  WriteStoryMake (Story);
end;

procedure TfrmChapter.btnChapterDownClick(Sender: TObject);
begin
  if (lstChapters.ItemIndex < (Chapters.Count - 1)) then begin
  	Chapters.MoveChapterDown;
    PopulateChapterList;
    Chapters.MarkDirty;
    btnSaveChapters.Enabled := TRUE;
    lstChapters.ItemIndex := Chapters.CurrentIndex;
  end;
end;

procedure TfrmChapter.btnChapterUpClick(Sender: TObject);
begin
  if (lstChapters.ItemIndex > 0) then begin
  	Chapters.MoveChapterUp;
    PopulateChapterList;
    Chapters.MarkDirty;
    btnSaveChapters.Enabled := TRUE;
    lstChapters.ItemIndex := Chapters.CurrentIndex;
  end;
end;

procedure TfrmChapter.btnDeleteChapterClick (Sender : TObject );
var
  s : string;
begin
  if (Chapters.Current <> nil) then begin
      s := 'Are you sure you wish to delete the Chapter "'
      	+ Chapters.Current.Title + '"?  (This does not delete any files.)';
      if (Application.MessageBox (pchar(s), 'Delete Chapter',
      	MB_ICONQUESTION + MB_YESNO) = IDYES) then
      	Chapters.Delete;
    end;
  btnDeleteChapter.Enabled := FALSE;
  PopulateChapterList;
end;

procedure TfrmChapter.btnMakeClick(Sender: TObject);
begin
  frmLog.Show;
  frmLog.BringToFront;
  frmLog.txtLog.Lines.Clear;
  Make (Story.SourceDir);
end;

procedure TfrmChapter.btnSaveChaptersClick (Sender : TObject );
begin
  Chapters.Save;
  btnSaveChapters.Enabled := FALSE;
end;

procedure TfrmChapter.btnStoryProfilesClick(Sender: TObject);
begin
  with (TfrmStoryProfiles.Create (Application)) do begin
    GlobalProfiles := Self.Profiles;
    BaseDir := Story.SourceDir;
    ShowModal;
  end;
end;

procedure TfrmChapter.chkIsABookChange(Sender: TObject);
begin
  if (chkIsABook.Enabled) then
    Chapters.Current.IsABook := chkIsABook.Checked;
end;

procedure TfrmChapter.chkNoTitleChange(Sender: TObject);
begin
  if (chkNoTitle.Enabled) then
    Chapters.Current.SuppressTitle := chkNoTitle.Checked;
end;

procedure TfrmChapter.chkSubtitleFirstChange(Sender: TObject);
begin
  if (chkSubtitleFirst.Enabled) then
    Chapters.Current.SubtitleFirst:=chkSubtitleFirst.Checked;
end;

procedure TfrmChapter.FormCreate (Sender : TObject );
begin
  Width := optInitialX;
  Height := optInitialY;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  Chapters := tChapterList.Create;
end;

procedure TfrmChapter.FormResize (Sender : TObject );
var
  col : integer;
begin
	col := (Width - 32) div 3;

  lstChapters.Width := col;
  lstChapters.Height := Height - 80;

  txtTitle.Left := col + 16;
  txtTitle.Width := col * 2 + 8;
  txtSubtitle.Left := col + 16;
  txtSubtitle.Width := col * 2 + 8;
  txtFilename.Left := col + 16;
  txtFilename.Width := col * 2 + 8;
  chkIsABook.Left := col + 80;
  chkSubtitleFirst.Left := col + 80;
  btnChapterUp.Left := col + 16;
  btnChapterDown.Left := col + 16;

  btnAddChapter.Left := 8;
  btnAddChapter.Top := Height - 44;
  btnDeleteChapter.Left := (col - btnDeleteChapter.Width) + 8;
  btnDeleteChapter.Top := Height - 44;
  btnSaveChapters.Left := (lstChapters.Width - btnSaveChapters.Width) div 2 + 8;
  btnSaveChapters.Top := Height - 44;

  btnStoryProfiles.Left := col + 16;
  btnStoryProfiles.Top := Height - 120;
  btnStoryProfiles.Width := col * 2 + 8;
  btnBuild.Left := col + 16;
  btnBuild.Top := Height - 80;
  btnBuild.Width := col * 2 + 8;
  btnBuild.Caption := 'Build "' + Story.Title + '"';
  btnMake.Left := col + 16;
  btnMake.Width := col * 2 + 8;
  btnMake.Top := Height - 40;
  btnMake.Caption := 'Run Make on "' + Story.Title + '"';
end;

procedure TfrmChapter.lstChaptersClick (Sender : TObject );
var
  index : integer;
  s : string;
begin
  index := lstChapters.ItemIndex;
  btnDeleteChapter.Enabled := FALSE;
  if (index > -1) then begin
    s := lstChapters.Items [index];
    if (s <> '') then begin
      Chapters.Select (s);

      // Disable Controls
      txtTitle.Enabled := FALSE;
      txtSubtitle.Enabled := FALSE;
      txtFilename.Enabled := FALSE;
      chkIsABook.Enabled := FALSE;
      chkSubtitleFirst.Enabled := FALSE;
      chkNoTitle.Enabled := FALSE;

      if (Chapters.Current <> nil) then begin
        // Load Controls
        txtTitle.Text := Chapters.Current.Title;
        txtSubtitle.Text := Chapters.Current.Subtitle;
        txtFilename.Text := Chapters.Current.Filename;
        chkIsABook.Checked := Chapters.Current.IsABook;
        chkSubtitleFirst.Checked := Chapters.Current.SubtitleFirst;
        chkNoTitle.Checked := Chapters.Current.SuppressTitle;

        // Enable Controls
        txtTitle.Enabled := TRUE;
        txtSubtitle.Enabled := TRUE;
        txtFilename.Enabled := TRUE;
        chkIsABook.Enabled := TRUE;
        chkSubtitleFirst.Enabled := TRUE;
        chkNoTitle.Enabled := TRUE;
      end;
    end;
  end;
end;

procedure TfrmChapter.lstChaptersDblClick(Sender: TObject);
var
  NewEditor : TfrmEditor;
  CurrentCaption,
  FormCaption : string;
  index : integer;
  Present : boolean;
begin
  if (Chapters.Current <> nil) then begin
	  FormCaption := 'Chapter "' + Chapters.Current.Title + '"';
    index := 0;
    Present := false;
    repeat
      CurrentCaption := Screen.Forms [index].Caption;
      if (copy (CurrentCaption, 1, 1) = '*') then
        delete (CurrentCaption, 1, 1);
      if (CurrentCaption = FormCaption) then
        Present := true
      else
    	  index += 1;
    until (Present or (index >= Screen.FormCount));
    if (Present) then begin
      Screen.Forms [index].Show;
      Screen.Forms [index].BringToFront
    end else begin
      NewEditor := TfrmEditor.Create (Application);
      NewEditor.Caption := FormCaption;
      NewEditor.BaseDir := Chapters.BaseDir;
      NewEditor.Filename := Chapters.Current.Filename;
      NewEditor.Story := Story;
      NewEditor.Show;
    end;
  end;
end;

procedure TfrmChapter.lstChaptersKeyPress(Sender: TObject; var Key: char);
begin
  if (KEy = #13) then
    lstChaptersDblClick (Sender);
end;

procedure TfrmChapter.txtFilenameChange(Sender: TObject);
begin
  if (txtFilename.Enabled) then
    if (Chapters.Current <> nil) then begin
      Chapters.Current.Filename := txtFilename.Text;
      btnSaveChapters.Enabled := TRUE;
      Chapters.MarkDirty
    end;
end;

procedure TfrmChapter.txtSubtitleChange(Sender: TObject);
begin
  if (txtSubtitle.Enabled) then
    if (Chapters.Current <> nil) then begin
      Chapters.Current.Subtitle := txtSubtitle.Text;
      btnSaveChapters.Enabled := TRUE;
      Chapters.MarkDirty
    end;
end;

procedure TfrmChapter.txtTitleChange(Sender: TObject);
begin
  if (txtTitle.Enabled) then
    if (Chapters.Current <> nil) then begin
      Chapters.Current.Title := txtTitle.Text;
      btnSaveChapters.Enabled := TRUE;
      Chapters.MarkDirty;
      PopulateChapterList;
    end;
end;

procedure TfrmChapter.ForceChapterListLoad;
begin
  if (Chapters.BaseDir <> '') then
    Chapters.Load;
  PopulateChapterList;
end;

procedure TfrmChapter.SetBaseDir (aDir : string);
begin
  if (Chapters <> nil) then
  	Chapters.BaseDir := aDir;
end;

procedure TfrmChapter.PopulateChapterList;
var
  index : integer;
begin
  lstChapters.Items.Clear;
  if (Chapters.Count > 0) then
    for index := 0 to (Chapters.Count - 1) do
  		lstChapters.Items.Add (Chapters.Name (index));

end;

end.

