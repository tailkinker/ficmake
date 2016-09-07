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

unit fothers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, gothers;

type

  { TfrmOtherFiles }

  TfrmOtherFiles = class(TForm)
    btnAddFile: TBitBtn;
    btnDeleteFile: TBitBtn;
    btnSaveList: TBitBtn;
    btnPickFile: TButton;
    txtFileName: TLabeledEdit;
    lstOtherFiles: TListBox;
    OpenDialog1: TOpenDialog;
    procedure btnAddFileClick(Sender: TObject);
    procedure btnDeleteFileClick(Sender: TObject);
    procedure btnPickFileClick(Sender: TObject);
    procedure btnSaveListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure txtFileNameChange(Sender: TObject);
    procedure lstOtherFilesClick(Sender: TObject);
  private
    { private declarations }
    List : tOtherFileList;
  public
    { public declarations }
    procedure SetBaseDir (aDir : string);
    procedure ForceFileListLoad;
    procedure PopulateFileList;
  end;

implementation

uses
  LCLType;

{$R *.lfm}

{ TfrmOtherFiles }

procedure TfrmOtherFiles.FormResize(Sender: TObject);
var
  x : integer;
begin
  x := (Width - 24) div 2;
  lstOtherFiles.Left := 8;
  lstOtherFiles.Top := 8;
  lstOtherFiles.Height := Height - 60;
  lstOtherFiles.Width := x;

  btnAddFile.Top := Height - 44;
  btnAddFile.Left := 8;
  btnDeleteFile.Top := Height - 44;
  btnDeleteFile.Left := (lstOtherFiles.Left + lstOtherFiles.Width)
    - btnDeleteFile.Width;
  btnSaveList.Top := Height - 44;
  btnSaveList.Left := (btnDeleteFile.Left - 8) div 2 + 8;

  x += 16;
  txtFileName.Top := 32;
  txtFileName.Width := Width - (x + 8);
  txtFileName.Left := x;
  btnPickFile.Left := (Width - (8 + btnPickFile.Width));
  btnPickFile.Top := txtFileName.Top + txtFileName.Height + 8;
end;

procedure TfrmOtherFiles.txtFileNameChange(Sender: TObject);
begin
  if (txtFileName.Enabled) then
    if (List.Current <> nil) then begin
      List.Current.Filename := txtFileName.Text;
      List.MarkDirty;
      btnSaveList.Enabled := TRUE;
      PopulateFileList;
    end;
end;

procedure TfrmOtherFiles.lstOtherFilesClick(Sender: TObject);
var
  index : integer;
  s : string;
begin
  index := lstOtherFiles.ItemIndex;
  btnDeleteFile.Enabled := FALSE;

  if (index in [0..lstOtherFiles.Items.Count]) then begin
    s := lstOtherFiles.Items [index];
    List.Select (s);

    // Disable Controls
    txtFileName.Enabled := FALSE;
    // Load Controls
    txtFileName.Text := List.Current.Filename;

    // Enable Controls
    if (List.Current <> nil) then begin
      txtFileName.Enabled := TRUE;
      btnDeleteFile.Enabled := TRUE;
    end;
  end;
end;

procedure TfrmOtherFiles.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  List := tOtherFileList.Create;
end;

procedure TfrmOtherFiles.btnPickFileClick(Sender: TObject);
begin
  if (OpenDialog1.Execute) then begin
    txtFileName.Text := OpenDialog1.Filename;
    List.MarkDirty;
    btnSaveList.Enabled := TRUE;
  end;
end;

procedure TfrmOtherFiles.btnDeleteFileClick(Sender: TObject);
begin
	List.Delete;
  btnDeleteFile.Enabled := FALSE;
  PopulateFileList;
  btnSaveList.Enabled := TRUE;
end;

procedure TfrmOtherFiles.btnAddFileClick(Sender: TObject);
begin
  if (OpenDialog1.Execute) then begin
    List.New ('<NEW ENTRY>');
    List.Current.Filename := OpenDialog1.Filename;
    List.MarkDirty;
    btnSaveList.Enabled := TRUE;
    PopulateFileList;
    btnSaveList.Enabled := TRUE;
  end;
end;

procedure TfrmOtherFiles.btnSaveListClick(Sender: TObject);
begin
  List.Save;
  btnSaveList.Enabled := FALSE;
end;

procedure TfrmOtherFiles.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if (List.Dirty) then
    if (Application.MessageBox ('Do you wish to save the Dependant File List?',
    	'File List Changed', MB_ICONQUESTION + MB_YESNO) = IDYES) then
      List.Save;
end;

procedure TfrmOtherFiles.SetBaseDir (aDir : string);
begin
  List.BaseDir := aDir;
end;

procedure TfrmOtherFiles.ForceFileListLoad;
begin
  if (List.BaseDir <> '') then
    List.Load;
  PopulateFileList;
end;

procedure TfrmOtherFiles.PopulateFileList;
var
  index : integer;
begin
  lstOtherFiles.Items.Clear;
  if (List.Count > 0) then
    for index := 0 to (List.Count - 1) do
      lstOtherFiles.Items.Add (List.Name (index))
end;

end.

