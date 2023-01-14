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

  { tMacro }

  tMacro = class (tObject)
    MacroName : string;
    MacroLines : tStringList;
    constructor Create;
    destructor Destroy;
    procedure LoadFrom (var t : text);
    procedure SaveTo (var t : text);
    procedure Edit;
  end;

  { TfrmEditor }
  TfrmEditor = class(TForm)
    grpMacros: TGroupBox;
    MainMenu1: TMainMenu;
    MenuEditNotes: TMenuItem;
    mnuInsertFile: TMenuItem;
    mnuFileRevert: TMenuItem;
    mnuEditMacros: TMenuItem;
    MenuSpacer1: TMenuItem;
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
    MenuSpacer2: TMenuItem;
    diaOpen: TOpenDialog;
    txtEditor: TMemo;
    tabEditors: TTabControl;
    procedure btnMacroClick(Sender: TObject);
    procedure btnMacroMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuEditNotesClick(Sender: TObject);
    procedure mnuBuildMakeClick(Sender: TObject);
    procedure mnuBuildBuildClick(Sender: TObject);
    procedure mnuEditCopyClick(Sender: TObject);
    procedure mnuEditCutClick(Sender: TObject);
    procedure mnuEditMacrosClick(Sender: TObject);
    procedure mnuEditPasteClick(Sender: TObject);
    procedure mnuEditWordsClick(Sender: TObject);
    procedure mnuFileCloseClick(Sender: TObject);
    procedure mnuFileDeleteClick(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuFileRevertClick(Sender: TObject);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure mnuFileSaveNoBuildClick(Sender: TObject);
    procedure mnuFormatBoldClick(Sender: TObject);
    procedure mnuFormatItalicsClick(Sender: TObject);
    procedure mnuInsertFileClick(Sender: TObject);
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
    procedure LoadMacros;
    procedure SaveMacros;
  public
    { public declarations }
    t_macros : array [0..9] of tMacro;
    btnMacros : array [0..9] of tButton;
    Story : tStory;
    property BaseDir : string read t_basedir write SetBaseDir;
    property Filename : string read t_filename write SetFileName;
    property MakeOnSave : boolean read t_makeonsave write t_makeonsave;
    property Dirty : boolean read t_dirty write SetDirty;
  end;

implementation

uses
  LCLType,
  doption, flog, fpickchap, fbaredit, fmacedit,
  gprofile, gmake, gtools, gothers;

{$R *.lfm}

{ tMacro }
constructor tMacro.Create;
begin
  inherited Create;
  MacroLines := tStringList.Create;
end;

destructor tMacro.Destroy;
begin
  MacroLines.Destroy;
  inherited Destroy;
end;

procedure tMacro.LoadFrom (var t : text);
var
  s : string;
begin
  readln (t, MacroName);
  MacroLines.Clear;
  repeat
    readln (t, s);
    if (s <> '<END>') then
      MacroLines.Add (s);
  until (s = '<END>');
end;

procedure tMacro.SaveTo (var t : text);
var
  index : integer;
begin
  writeln (t, MacroName);
  for index := 0 to (MacroLines.Count - 1) do
    writeln (t, MacroLines [index]);
  writeln (t, '<END>');
end;

procedure tMacro.Edit;
var
  Editor : TfrmMacroEdit;
  index : integer;
begin
  Editor := TfrmMacroEdit.Create(Application);
  Editor.txtName.Text := MacroName;
  if (MacroLines.Count > 0) then
    for index := 0 to (MacroLines.Count - 1) do
      Editor.txtLines.Lines.Add(MacroLines [index]);
  Editor.ShowModal;
  MacroName := Editor.txtName.Text;
  MacroLines.Clear;
  if (Editor.txtLines.Lines.Count > 0) then
    for index := 0 to (Editor.txtLines.Lines.Count - 1) do
      MacroLines.Add (Editor.txtLines.Lines [index]);
  Editor.Destroy;
end;

{ TfrmEditor }

procedure TfrmEditor.LoadMacros;
var
  aFilename : string;
  MacroCount : integer;
  t : text;
  s : string;
begin
  aFilename := Story.SourceDir + '/macros.txt';
  if (FileExists (aFilename)) then begin
    system.assign (t, aFilename);
    system.reset (t);
    MacroCount := 0;
    repeat
      readln (t, s);
      if (s = '<MACRO>') then begin
        t_macros [MacroCount] := tMacro.Create;
        t_macros [MacroCount].LoadFrom (t);
        btnMacros [MacroCount].Caption := t_macros [MacroCount].MacroName;
        inc (MacroCount);
        mnuEditMacros.Checked := TRUE;
        grpMacros.Visible := TRUE;
      end;
    until (s = '<END LIST>');
    system.close (t);
  end;
end;

procedure TfrmEditor.SaveMacros;
var
  aFilename : string;
  t : text;
  index : integer;
begin
  aFilename := Story.SourceDir + '/macros.txt';
  system.assign (t, aFilename);
  system.rewrite (t);

  for index := 0 to 9 do
    if (t_macros [index] <> nil) then begin
      writeln (t, '<MACRO>');
      t_macros [index].SaveTo(t);
    end;
  writeln (t, '<END LIST>');
  system.close(t);
end;

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
var
  aFileName : string;
  index : integer;
begin
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

  for index := 0 to 9 do begin
    str (index + 1, aFilename);
    aFilename := 'Macro ' + aFilename;
    btnMacros [index] := tButton.Create (grpMacros);
    with btnMacros [index] do begin
      Caption := aFilename;
      Parent := grpMacros;
      Width := 120;
      Height := 30;
      Left := 4;
      Top := 4 + (index - 1) * 34;
      Hint := 'Right-Click to Edit';
      Tag := index;
      ShowHint := TRUE;
      OnClick := @btnMacroClick;
      OnMouseUp := @btnMacroMouseUp;
    end;
  end;
  t_tab := 1;
  tabEditors.TabIndex := 1;
  mnuMakeOnSave.Checked := optAssumeMakeOnSave;

  mnuEditMacros.Checked := FALSE;
end;

procedure TfrmEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (Dirty) then begin
    if (Application.MessageBox ('Save changes to this story portion?',
      'Save Changes', MB_ICONQUESTION + MB_YESNO) = IDYES) then
      SaveCurrentEdit;
  end;
end;

procedure TfrmEditor.btnMacroMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Macro : tMacro;
  index : integer;
begin
  index := TButton(Sender).Tag;
  if (Button = mbRight) then begin
    if (t_macros [index] = nil) then
      t_macros [index] := tMacro.Create;
    Macro := t_macros [index];
    Macro.Edit;
    SaveMacros;
  end;
end;

procedure TfrmEditor.btnMacroClick(Sender: TObject);
var
  Macro : tMacro;
  index : integer;
begin
  index := TButton(Sender).Tag;
  Macro := t_macros [index];
  if (Macro = nil) then begin
    t_macros [index] := tMacro.Create;
    Macro := t_macros [index];
    Macro.Edit;
    btnMacros [(Sender as tButton).Tag].Caption := Macro.MacroName;
    SaveMacros;
  end else begin
    for index := 0 to (Macro.MacroLines.Count - 1) do begin
      with (txtEditor) do
        Lines.Insert (CaretPos.Y, Macro.MacroLines [index]);
    end;
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
  LoadMacros;
end;

procedure TfrmEditor.MenuEditNotesClick(Sender: TObject);
begin
  with (TfrmBareEditor.Create (Application)) do begin
    Caption := 'Edit Notes';
    Filename := Story.SourceDir + '/notes.txt';
    ShowModal;
  end;
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

procedure TfrmEditor.mnuEditMacrosClick(Sender: TObject);
var
  aFileName : string;
begin
  mnuEditMacros.Checked := not mnuEditMacros.Checked;
  if (mnuEditMacros.Checked) then begin
    aFilename := Story.SourceDir + '/macros.txt';
    if (FileExists (aFileName)) then
      LoadMacros;
  end;
  grpMacros.Visible := mnuEditMacros.Checked;
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

procedure TfrmEditor.mnuFileRevertClick(Sender: TObject);
begin
  if Dirty then
    if (Application.MessageBox (
      'This will reload the file from disk and erase any changes that were made.',
      'Are You Sure?', MB_ICONQUESTION + MB_YESNO) = IDYES) then
      LoadFromFile;
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
begin
  with (txtEditor) do
    if (SelLength > 0) then begin
      s := '\fB' + SelText + '\fR';
      SelText := s;
    end else
      SelText := '\fB'
end;

procedure TfrmEditor.mnuFormatItalicsClick(Sender: TObject);
var
  s : string;
begin
  with (txtEditor) do
    if (SelLength > 0) then begin
      s := '\fI' + SelText + '\fR';
      SelText := s;
    end else
      SelText := '\fI';
end;

procedure TfrmEditor.mnuInsertFileClick(Sender: TObject);
var
  soFilename : string;
  OtherList : tOtherFileList;
begin
  if (diaOpen.Execute) then begin
    soFilename := diaOpen.FileName;
    with (txtEditor) do
      Lines.Insert (CaretPos.Y, '.so ' + soFilename);
    OtherList := tOtherFileList.Create;
    OtherList.BaseDir := Story.SourceDir;
    OtherList.Load;
    OtherList.New(soFilename);
    OtherList.Save;
    OtherList.Destroy
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
    if ((Key = #13) or (Key = #10)) then
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

