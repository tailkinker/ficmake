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

unit feditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Menus,
  gstory;

type

  { TfrmEditor }

  TfrmEditor = class(TForm)
    MainMenu1: TMainMenu;
    mnuFileSaveNoBuild: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuMakeOnSave: TMenuItem;
    mnuFileClose: TMenuItem;
    mnuBuildBuild: TMenuItem;
    mnuBuildMake: TMenuItem;
    mnuFormatBold: TMenuItem;
    mnuFormatItalics: TMenuItem;
    mnuFormat: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditWords: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuFileDelete: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuBuild: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFile: TMenuItem;
    txtEditor: TMemo;
    tabEditors: TTabControl;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuBuildMakeClick(Sender: TObject);
    procedure mnuBuildBuildClick(Sender: TObject);
    procedure mnuEditCopyClick(Sender: TObject);
    procedure mnuEditCutClick(Sender: TObject);
    procedure mnuEditPasteClick(Sender: TObject);
    procedure mnuEditWordsClick(Sender: TObject);
    procedure mnuFileCloseClick(Sender: TObject);
    procedure mnuFileDeleteClick(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure mnuFileSaveNoBuildClick(Sender: TObject);
    procedure mnuFormatBoldClick(Sender: TObject);
    procedure mnuFormatItalicsClick(Sender: TObject);
    procedure mnuMakeOnSaveClick(Sender: TObject);
    procedure tabEditorsChange(Sender: TObject);
    procedure txtEditorChange(Sender: TObject);
    procedure txtEditorKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    t_basedir : string;
    t_filename : string;
    t_tab : byte;
    t_makeonsave : boolean;
    t_dirty : boolean;
    procedure SetBaseDir (aDir : string);
    procedure SetFilename (aFilename : string);
    procedure LoadFromFile;
    procedure SaveCurrentEdit;
    procedure SetDirty (aDirty : boolean);
  public
    { public declarations }
    Story : tStory;
    property BaseDir : string read t_basedir write SetBaseDir;
    property Filename : string read t_filename write SetFileName;
    property MakeOnSave : boolean read t_makeonsave write t_makeonsave;
    property Dirty : boolean read t_dirty write SetDirty;
  end;

implementation

uses
  LCLType,
  doption, flog, fpickchap,
  gprofile, gmake, gtools;

{$R *.lfm}

{ TfrmEditor }

procedure TfrmEditor.SetDirty (aDirty : boolean);
var
  s : string;
begin
  t_dirty := aDirty;
  s := Caption;
  if (copy (s, 1, 1) = '*') then
    delete (s, 1, 1);
  if (aDirty = TRUE) then
    s := '*' + s;
  Caption := s
end;

procedure TfrmEditor.SetBaseDir (aDir : string);
begin
  t_basedir := aDir;
end;

procedure TfrmEditor.SetFilename (aFilename : string);
begin
  t_filename := aFilename;
  LoadFromFile;
end;

procedure TfrmEditor.FormCreate(Sender: TObject);
begin
  Width := optInitialX;
  Height := optInitialY;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  t_tab := 1;
  tabEditors.TabIndex := 1;
  mnuMakeOnSave.Checked := optAssumeMakeOnSave;
end;

procedure TfrmEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (Dirty) then begin
    if (Application.MessageBox ('Save changes to this story portion?',
      'Save Changes', MB_ICONQUESTION + MB_YESNO) = IDYES) then
      SaveCurrentEdit;
  end;
end;

procedure TfrmEditor.FormResize(Sender: TObject);
begin
  txtEditor.Font.Size := optFontSize;
  {
  tabEditors.Height := Height - (tabEditors.TabHeight + 16);
  tabEditors.Width := Width - 16;
  txtEditor.Width := tabEditors.Width;
  txtEditor.Height := tabEditors.Height - (tabEditors.TabHeight + 8);
  txtEditor.Left := 0;
  txtEditor.Top := tabEditors.TabHeight;
  }
end;

procedure TfrmEditor.mnuBuildMakeClick(Sender: TObject);
begin
  frmLog.Show;
  frmLog.BringToFront;
  frmLog.txtLog.Lines.Clear;
  Make (Story.SourceDir);
end;

procedure TfrmEditor.mnuBuildBuildClick(Sender: TObject);
var
  s : string;
  limit,
  index : integer;
  StoryProfiles : tProfileList;
begin
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

procedure TfrmEditor.mnuEditCopyClick(Sender: TObject);
begin
  txtEditor.CopyToClipboard;
end;

procedure TfrmEditor.mnuEditCutClick(Sender: TObject);
begin
  txtEditor.CutToClipboard;
end;

procedure TfrmEditor.mnuEditPasteClick(Sender: TObject);
begin
  txtEditor.PasteFromClipboard;
end;

procedure TfrmEditor.mnuEditWordsClick(Sender: TObject);
var
  i,
  j,
  words : longint;
  s : string;
  m : pchar;
  OnWord,
  LastOnWord : boolean;
begin
  words := 0;
	if (txtEditor.Lines.Count > 0) then begin
    for i := 0 to txtEditor.Lines.Count do begin
      s := txtEditor.Lines.Strings [i];
      if (length (s) > 0) then
          if (not (s [1] = '.')) then begin
          j := 1;
          OnWord := false;
          while (j < length (s)) do begin
            LastOnWord := OnWord;
            if (s [j] <> ' ') then
              OnWord := true
            else
              OnWord := false;
            if ((OnWord) and not (LastOnWord)) then
              words += 1;
            j += 1;
          end;
        end;
    end;
  end;

  s := 'Word Count:  ' + InsertCommas (words);
  m := pchar (s);
  Application.MessageBox (m, 'Word Count', MB_ICONINFORMATION + MB_OK);
  txtEditor.SetFocus;
end;

procedure TfrmEditor.mnuFileCloseClick(Sender: TObject);
begin
  Destroy;
end;

procedure TfrmEditor.mnuFileDeleteClick(Sender: TObject);
var
  CurrentFileName : string;
begin
  CurrentFileName := BaseDir + '/' + Filename;
  case (t_tab) of
  	0 : CurrentFileName += '.co';
    1 : CurrentFileName += '.so';
    2 : CurrentFileName += '.tr';
    3 : CurrentFileName += '.om';
    4 : CurrentFileName += '.an';
  end;
  if (Application.MessageBox (
    'Delete this story portion?  This cannot be undone!',
    'Delete Portion', MB_ICONQUESTION + MB_YESNO) = IDYES) then begin
      DeleteFile (CurrentFileName);
      txtEditor.Lines.Clear;
    end;
end;

procedure TfrmEditor.mnuFileOpenClick(Sender: TObject);
begin
  if Dirty then
    if (Application.MessageBox ('Save this Chapter before continuing?',
      'Save Chapter', MB_ICONQUESTION + MB_YESNO) = IDYES) then
        SaveCurrentEdit;
  with (TfrmPickChapter.Create (Application)) do begin
    BaseDir := self.BaseDir;
    if (ShowModal = mrOK) then begin
      self.Filename := Filename;
      LoadFromFile;
    end;
    Destroy;
  end;
end;

procedure TfrmEditor.mnuFileSaveClick(Sender: TObject);
begin
  SaveCurrentEdit;
end;

procedure TfrmEditor.mnuFileSaveNoBuildClick(Sender: TObject);
var
  Store : BOOLEAN;
begin
  Store := mnuMakeOnSave.Checked;
  mnuMakeOnSave.Checked := FALSE;
  SaveCurrentEdit;
  mnuMakeOnSave.Checked := Store;
end;

procedure TfrmEditor.mnuFormatBoldClick(Sender: TObject);
var
  s : string;
  SavePos : TPoint;
begin
  with (txtEditor) do
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
end;

procedure TfrmEditor.mnuFormatItalicsClick(Sender: TObject);
var
  s : string;
  SavePos : TPoint;
begin
  with (txtEditor) do
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
end;

procedure TfrmEditor.mnuMakeOnSaveClick(Sender: TObject);
begin
  MakeOnSave := not MakeOnSave;
  mnuMakeOnSave.Checked := MakeOnSave;
end;

procedure TfrmEditor.tabEditorsChange(Sender: TObject);
begin
  if (Dirty) then begin
    if (Application.MessageBox ('Save changes to this story portion?',
      'Save Changes', MB_ICONQUESTION + MB_YESNO) = IDYES) then
      SaveCurrentEdit;
  end;
  t_tab := tabEditors.TabIndex;
  LoadFromFile;
end;

procedure TfrmEditor.txtEditorChange(Sender: TObject);
begin
  Dirty := TRUE;
end;

procedure TfrmEditor.txtEditorKeyPress(Sender: TObject; var Key: char);
begin
  with (txtEditor) do
    if (Key = #13) then
      if (CaretPos.X = 0) then begin
        Lines.Insert (CaretPos.Y, '.PP');
        Key := #0
      end;
end;

procedure TfrmEditor.LoadFromFile;
var
  CurrentFileName : string;
  Start : tPoint;
begin
  txtEditor.Lines.Clear;
  mnuFileDelete.Enabled := FALSE;
  CurrentFileName := BaseDir + '/' + Filename;
  case (t_tab) of
	  0 : CurrentFileName += '.co';
    1 : CurrentFileName += '.so';
    2 : CurrentFileName += '.tr';
    3 : CurrentFileName += '.om';
    4 : CurrentFileName += '.an';
  end;
  Dirty := FALSE;
  if (FileExists (CurrentFileName)) then begin
    txtEditor.Lines.LoadFromFile (CurrentFileName);
    if (t_tab <> 1) then
      mnuFileDelete.Enabled := TRUE;
    Start.X := 0;
    Start.Y := 0;
    txtEditor.CaretPos := Start;
  end;
end;

procedure TfrmEditor.SaveCurrentEdit;
var
  CurrentFileName : string;
begin
  CurrentFileName := BaseDir + '/' + Filename;
  case (t_tab) of
  	0 : CurrentFileName += '.co';
    1 : CurrentFileName += '.so';
    2 : CurrentFileName += '.tr';
    3 : CurrentFileName += '.om';
    4 : CurrentFileName += '.an';
  end;
  txtEditor.Lines.SaveToFile (CurrentFileName);
  Dirty := FALSE;
  if (mnuMakeOnSave.Checked) then
    Make (Story.SourceDir);
end;

end.

