unit fhdhelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmHeaderHelp }

  TfrmHeaderHelp = class(TForm)
    btnDismiss: TButton;
    Label1: TLabel;
    procedure btnDismissClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmHeaderHelp: TfrmHeaderHelp;

implementation

{$R *.lfm}

{ TfrmHeaderHelp }

procedure TfrmHeaderHelp.btnDismissClick(Sender: TObject);
begin
  frmHeaderHelp.Close;
end;

procedure TfrmHeaderHelp.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

end.

