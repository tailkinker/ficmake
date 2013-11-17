unit fnewchap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmNewChapter }

  TfrmNewChapter = class (TForm )
    btnCancel : TButton;
    btnOk : TButton;
    txtName : TLabeledEdit;
    procedure FormCreate (Sender : TObject );
    procedure txtNameChange (Sender : TObject );
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmNewChapter : TfrmNewChapter;

implementation

{$R *.lfm}

{ TfrmNewChapter }

procedure TfrmNewChapter.FormCreate (Sender : TObject );
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmNewChapter.txtNameChange (Sender : TObject );
begin
  if (length (txtName.Text) > 0) then
  	btnOk.Enabled := TRUE
  else
    btnOk.Enabled := FALSE;
end;

end.

