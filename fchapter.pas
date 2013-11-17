unit fchapter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, gchapter;

type

  { TfrmChapter }

  TfrmChapter = class (TForm )
    chkIsABook : TCheckBox;
    chkSubtitleFirst : TCheckBox;
    Label1 : TLabel;
    lstChapters : TListBox;
    txtFilename : TLabeledEdit;
    txtSubtitle : TLabeledEdit;
    txtTitle : TLabeledEdit;
    procedure FormClose (Sender : TObject; var CloseAction : TCloseAction );
    procedure FormCreate (Sender : TObject );
    procedure FormResize (Sender : TObject );
  private
    Chapters : tChapterList;
    procedure PopulateChapterList;
  public
    procedure SetBaseDir (aDir : string);
    procedure ForceChapterListLoad;
  end;

implementation

uses
  LCLType,
  foptions;

{$R *.lfm}

{ TfrmChapter }

procedure TfrmChapter.FormClose (Sender : TObject; var CloseAction : TCloseAction );
begin
  if (Chapters.Dirty) then
    if (Application.MessageBox ('Do you wish to save the Chapters List?',
    	'Chapters List Changed', MB_ICONQUESTION + MB_YESNO) = IDYES) then
      Chapters.Save;
end;

procedure TfrmChapter.FormCreate (Sender : TObject );
begin
  Width := InitialX;
  Height := InitialY;
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
  chkIsABook.Left := col + 16;
  chkSubtitleFirst.Left := col + 16;
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

