unit fnewvol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmNewVolume }

  TfrmNewVolume = class (TForm )
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

implementation

uses
  doption;

{$R *.lfm}

{ TfrmNewVolume }

procedure TfrmNewVolume.FormCreate (Sender : TObject );
begin
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
end;

procedure TfrmNewVolume.txtNameChange (Sender : TObject );
begin
  if (length (txtName.Text) > 0) then
  	btnOk.Enabled := TRUE
  else
    btnOk.Enabled := FALSE;
end;

end.

