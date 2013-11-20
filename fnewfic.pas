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

unit fnewfic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmNewStory }

  TfrmNewStory = class (TForm )
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

{$R *.lfm}

{ TfrmNewStory }

procedure TfrmNewStory.FormCreate (Sender : TObject );
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmNewStory.txtNameChange (Sender : TObject );
begin
  if (length (txtName.Text) > 0) then
  	btnOk.Enabled := TRUE
  else
    btnOk.Enabled := FALSE;
end;

end.

