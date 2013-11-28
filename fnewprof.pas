unit fnewprof;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls,
  gprofile;

type

  { TfrmNewProfile }

  TfrmNewProfile = class (TForm )
    btnPDF : TButton;
    btnHTML : TButton;
    btnText : TButton;
    btnEPub : TButton;
    btnCancel : TButton;
    txtName : TLabeledEdit;
    procedure btnEPubClick (Sender : TObject );
    procedure btnHTMLClick (Sender : TObject );
    procedure btnPDFClick (Sender : TObject );
    procedure btnTextClick (Sender : TObject );
    procedure FormCreate (Sender : TObject );
    procedure txtNameChange (Sender : TObject );
  private
    { private declarations }
  public
    NewProfile : tBaseProfile;
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TfrmNewProfile }

procedure TfrmNewProfile.btnPDFClick (Sender : TObject );
begin
  NewProfile := tPDFProfile.Create;
  NewProfile.Name := txtName.Text;
end;

procedure TfrmNewProfile.btnTextClick (Sender : TObject );
begin
  NewProfile := tTextProfile.Create;
  NewProfile.Name := txtName.Text;
end;

procedure TfrmNewProfile.FormCreate (Sender : TObject );
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmNewProfile.txtNameChange (Sender : TObject );
begin
  if (Length (txtName.Text) > 0) then begin
    btnPDF.Enabled := TRUE;
    btnHTML.Enabled := TRUE;
    btnText.Enabled := TRUE;
    btnEPub.Enabled := TRUE;
  end else begin
  	btnPDF.Enabled := FALSE;
    btnHTML.Enabled := FALSE;
    btnText.Enabled := FALSE;
    btnEPub.Enabled := FALSE;
  end;
end;

procedure TfrmNewProfile.btnHTMLClick (Sender : TObject );
begin
	NewProfile := tHTMLProfile.Create;
  NewProfile.Name := txtName.Text;
end;

procedure TfrmNewProfile.btnEPubClick (Sender : TObject );
begin
  NewProfile := tEPubProfile.Create;
  NewProfile.Name := txtName.Text;
end;


end.

