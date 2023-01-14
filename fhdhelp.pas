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

uses
  doption;
{$R *.lfm}

{ TfrmHeaderHelp }

procedure TfrmHeaderHelp.btnDismissClick(Sender: TObject);
begin
  frmHeaderHelp.Close;
end;

procedure TfrmHeaderHelp.FormCreate(Sender: TObject);
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

end.

