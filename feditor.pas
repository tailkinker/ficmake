unit feditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Menus;

type

  { TfrmEditor }

  TfrmEditor = class(TForm)
    MainMenu1: TMainMenu;
    mnuMakeOnSave: TMenuItem;
    mnuFileClose: TMenuItem;
    mnuBuildBuild: TMenuItem;
    mntBuildMake: TMenuItem;
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
    procedure mnuEditCopyClick(Sender: TObject);
    procedure mnuEditCutClick(Sender: TObject);
    procedure mnuEditPasteClick(Sender: TObject);
    procedure mnuFileCloseClick(Sender: TObject);
    procedure mnuFileDeleteClick(Sender: TObject);
    procedure mnuFileSaveClick(Sender: TObject);
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
    property BaseDir : string read t_basedir write SetBaseDir;
    property Filename : string read t_filename write SetFileName;
    property MakeOnSave : boolean read t_makeonsave write t_makeonsave;
    property Dirty : boolean read t_dirty write SetDirty;
  end;

var
  frmEditor: TfrmEditor;

implementation

uses
  LCLType,
  doption;

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
  tabEditors.Height := Height - (tabEditors.TabHeight + 16);
  tabEditors.Width := Width - 16;
  txtEditor.Font.Size := optFontSize;
  txtEditor.Width := tabEditors.Width;
  txtEditor.Height := tabEditors.Height - (tabEditors.TabHeight + 8);
  txtEditor.Left := 0;
  txtEditor.Top := tabEditors.TabHeight;
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
  if (Application.MessageBox ('Delete this story portion?  This cannot be undone!',
    'Delete Portion', MB_ICONQUESTION + MB_YESNO) = IDYES) then begin
      DeleteFile (CurrentFileName);
      txtEditor.Lines.Clear;
    end;
end;

procedure TfrmEditor.mnuFileSaveClick(Sender: TObject);
begin
  SaveCurrentEdit;
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
end;

end.

