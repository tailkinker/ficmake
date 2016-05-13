unit fpickchap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  gchapter;

type

  { TfrmPickChapter }

  TfrmPickChapter = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lstChapters: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstChaptersClick(Sender: TObject);
    procedure lstChaptersDblClick(Sender: TObject);
    procedure lstChaptersKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    t_basedir : string;
    t_chapterpick : string;
    Chapters : tChapterList;
    procedure SetBaseDir (aDir : string);
  public
    { public declarations }
    property BaseDir : string read t_basedir write SetBaseDir;
    property Filename : string read t_chapterpick;
  end;

implementation

{$R *.lfm}

procedure TfrmPickChapter.FormCreate(Sender: TObject);
begin
  Chapters := tChapterList.Create;
end;

procedure TfrmPickChapter.FormDestroy(Sender: TObject);
begin
  Chapters.Destroy;
end;

procedure TfrmPickChapter.lstChaptersClick(Sender: TObject);
begin
  if (lstChapters.ItemIndex > 0) then
    if (lstChapters.ItemIndex < (Chapters.Count)) then begin
      Chapters.SelectAt (lstChapters.ItemIndex);
      t_chapterpick := Chapters.Current.Filename;
    end;
end;

procedure TfrmPickChapter.lstChaptersDblClick(Sender: TObject);
begin
  if (lstChapters.ItemIndex > 0) then
    if (lstChapters.ItemIndex < (Chapters.Count)) then begin
      Chapters.SelectAt (lstChapters.ItemIndex);
      t_chapterpick := Chapters.Current.Filename;
    end;
  ModalResult := mrOk;
end;

procedure TfrmPickChapter.lstChaptersKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
    lstChaptersDblClick (Sender);
end;

procedure TfrmPickChapter.SetBaseDir (aDir : string);
var
  index : integer;
begin
  t_basedir := aDir;
  Chapters.BaseDir := aDir;
  Chapters.Load;

  lstChapters.Items.Clear;
  for index := 0 to (Chapters.Count - 1) do
    lstChapters.Items.Add (Chapters.Name (index));
end;

end.

