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

unit fimprof;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  gprofile;

type

  { TfrmImportProfile }

  TfrmImportProfile = class(TForm)
    btnCancel: TButton;
    btnImport: TButton;
    Label1: TLabel;
    lstProfiles: TListBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstProfilesClick(Sender: TObject);
    procedure lstProfilesDblClick(Sender: TObject);
  private
    { private declarations }
    t_proflist : tProfileList;
    t_prof : tBaseProfile;
    procedure SetProfileList (aProf : tProfileList);
  public
    { public declarations }
    property Profiles : tProfileList read t_proflist write SetProfileList;
    property SelectedProfile : tBaseProfile read t_prof;
  end;

implementation

{$R *.lfm}

{ TfrmImportProfile }

procedure TfrmImportProfile.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmImportProfile.btnImportClick(Sender: TObject);
begin
  t_prof := Profiles.Current.Duplicate;
  Hide;
end;

procedure TfrmImportProfile.btnCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TfrmImportProfile.lstProfilesClick(Sender: TObject);
var
  index : integer;
  s : string;
begin
	index := lstProfiles.ItemIndex;
  btnImport.Enabled := FALSE;

  if (index in [0..lstProfiles.Items.Count]) then begin
    s := lstProfiles.Items [index];
    Profiles.Select (s);
    btnImport.Enabled := TRUE;
  end;
end;

procedure TfrmImportProfile.lstProfilesDblClick(Sender: TObject);
var
  index : integer;
  s : string;
begin
	index := lstProfiles.ItemIndex;
  btnImport.Enabled := FALSE;

  if (index in [0..lstProfiles.Items.Count]) then begin
    s := lstProfiles.Items [index];
    Profiles.Select (s);
    btnImport.Enabled := TRUE;
  end;
end;

procedure TfrmImportProfile.SetProfileList (aProf : tProfileList);
var
  index : integer;
begin
  t_proflist := aProf;
  lstProfiles.Items.Clear;
  if (t_proflist.Count > 0) then
    for index := 0 to (t_proflist.Count - 1) do
      lstProfiles.Items.Add (t_proflist.Name (index));
end;

end.

