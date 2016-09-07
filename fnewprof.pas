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

