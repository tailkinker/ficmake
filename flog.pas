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

unit flog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfrmLog }

  TfrmLog = class(TForm)
    btnClose: TButton;
    txtLog: TMemo;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmLog: TfrmLog;

implementation

uses
  doption;

{$R *.lfm}

{ TfrmLog }

procedure TfrmLog.FormCreate(Sender: TObject);
begin
  Width := optInitialX;
  Height := optInitialY;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  txtLog.Clear;
  if (optOpenLogOnStart) then
    Show;
end;

procedure TfrmLog.btnCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TfrmLog.FormResize(Sender: TObject);
begin
  txtLog.Left := 8;
  txtLog.Top := 8;
  txtLog.Width := Width - 16;
  txtLog.Height := Height - 56;
  btnClose.Left := Width - 88;
  btnClose.Top := Height - 40;
end;

end.

