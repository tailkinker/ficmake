unit fbaredit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TfrmBareEditor }

  TfrmBareEditor = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    txtEditor: TMemo;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure txtEditorChange(Sender: TObject);
    procedure txtEditorKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    t_filename : string;
    procedure SaveCurrentEdit;
    procedure LoadFileName (aFileName : string);
  public
    { public declarations }
    Dirty : boolean;
    property FileName : string read t_filename write LoadFileName;
  end;

var
  frmBareEditor: TfrmBareEditor;

implementation

{$R *.lfm}

uses
  LCLType,
  doption;

{ TfrmBareEditor }

procedure TfrmBareEditor.btnOKClick(Sender: TObject);
begin
  SaveCurrentEdit;
  Hide;
end;

procedure TfrmBareEditor.btnCancelClick(Sender: TObject);
begin
  if ((NOT Dirty) OR (Application.MessageBox ('Close this editor without saving?', 'Cancel',
    MB_ICONQUESTION + MB_YESNO) = IDYES)) then
    Hide;
end;

procedure TfrmBareEditor.FormCreate(Sender: TObject);
begin
  Width := InitialX;
  Height := InitialY;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmBareEditor.FormResize(Sender: TObject);
begin
  txtEditor.Width := Width - 16;
  txtEditor.Height := Height - (btnOK.Height + 24);
  btnOK.Top := Height - (btnOK.Height + 8);
  btnCancel.Top := Height - (btnOK.Height + 8);
  btnCancel.Left := Width - (btnCancel.Width + 8);
  btnOK.Left := btnCancel.Left - (btnOK.Width + 8);
end;

procedure TfrmBareEditor.txtEditorChange(Sender: TObject);
begin
  Dirty := TRUE;
end;

procedure TfrmBareEditor.txtEditorKeyPress(Sender: TObject; var Key: char);
var
  s : string;
  SavePos : TPoint;
begin
  with (txtEditor) do begin
	  if (Key = #2) then
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
    if (Key = #13) then
      if (CaretPos.X = 0) then begin
        Lines.Insert (CaretPos.Y, '.PP');
        Key := #0
      end;
	  if (Key = #14) then
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
	  if (Key = #18) then begin
    	SavePos := CaretPos;
    	s := Lines [CaretPos.Y];
      s := copy (s, 1, CaretPos.X)
      	+ '\fR'
        + copy (s, CaretPos.X + 1, length (s) - CaretPos.X);
      Lines [CaretPos.Y] := s;
      SavePos.X += 3;
      CaretPos := SavePos
    end;
//    if (Key = #23) then
//      frmMain.btnWordCountClick (frmMain.txtEditor);
    if (Key = #26) then
      if (CanUndo) then
        Undo;
  end;
  if (Key = #19) then
  	SaveCurrentEdit;
end;

procedure TfrmBareEditor.SaveCurrentEdit;
begin
  txtEditor.Lines.SaveToFile (FileName);
  Dirty := FALSE;
end;

procedure TfrmBareEditor.LoadFileName (aFileName : string);
begin
  t_filename := aFileName;
  if (FileExists (aFileName)) then
    txtEditor.Lines.LoadFromFile (aFileName);
  Dirty := FALSE;
end;

end.

