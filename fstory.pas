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

unit fstory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, gstory;

type

  { TfrmStory }

  TfrmStory = class (TForm )
    btnAddStory : TBitBtn;
    btnBlurb : TButton;
    btnBuild : TButton;
    btnCredits : TButton;
    btnProfiles : TButton;
    btnDeleteStory : TBitBtn;
    btnDisclaimer : TButton;
    btnEditChapters : TButton;
    btnEditStory : TButton;
    btnMake : TButton;
    btnSaveStories : TBitBtn;
    Label1 : TLabel;
    labProfiles : TLabel;
    lstProfiles : TListBox;
    lstStories : TListBox;
    txtTitle : TLabeledEdit;
    procedure btnAddStoryClick (Sender : TObject );
    procedure btnDeleteStoryClick (Sender : TObject );
    procedure btnEditChaptersClick (Sender : TObject );
    procedure btnEditStoryClick (Sender : TObject );
    procedure btnSaveStoriesClick (Sender : TObject );
    procedure FormClose (Sender : TObject; var CloseAction : TCloseAction );
    procedure FormCreate (Sender : TObject );
    procedure FormResize (Sender : TObject );
    procedure lstStoriesClick (Sender : TObject );
  private
    Stories : tStoryList;
    procedure PopulateStoryList;
  public
    procedure SetBaseDir (aDir : string);
    procedure ForceLoadStoryList;
  end;

implementation

uses
  LCLType,
  fchapter, fnewfic, fficinfo, doption;

{$R *.lfm}

{ TfrmStory }

procedure TfrmStory.FormCreate (Sender : TObject );
begin
  Width := InitialX;
  Height := InitialY;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  Stories := tStoryList.Create;
end;

procedure TfrmStory.btnAddStoryClick (Sender : TObject );
var
  Dialog : TfrmNewStory;
begin
	Dialog := TfrmNewStory.Create (Application);
  if (Dialog.ShowModal = mrOK) then
  	Stories.NewStory (Dialog.txtName.Text);
  Dialog.Destroy;
  PopulateStoryList;
end;

procedure TfrmStory.btnDeleteStoryClick (Sender : TObject );
var
  s : string;
begin
  if (Stories.Current <> nil) then begin
    s := 'Are you sure you wish to delete the Story "'
    	+ Stories.Current.Title + '"?  (This does not delete any files.)';
    if (Application.MessageBox (pchar(s), 'Delete Story',
    	MB_ICONQUESTION + MB_YESNO) = IDYES) then
    	Stories.DeleteStory;
  end;
  btnDeleteStory.Enabled := FALSE;
  PopulateStoryList;
end;

procedure TfrmStory.btnEditChaptersClick (Sender : TObject );
var
  NewForm : TfrmChapter;
  FormCaption : string;
  index : integer;
  Present : boolean;
begin
	FormCaption := 'Story "' + Stories.Current.Title + '"';
  index := 0;
  Present := false;
  repeat
    if (Screen.Forms [index].Caption = FormCaption) then
      Present := true
    else
    	index += 1;
  until (Present or (index >= Screen.FormCount));
  if (Present) then
    Screen.Forms [index].BringToFront
  else begin
    NewForm := TfrmChapter.Create (Application);
    NewForm.Caption := FormCaption;
    NewForm.SetBaseDir (Stories.Current.SourceDir);
    NewForm.ForceChapterListLoad;
    NewForm.ShortName := Stories.Current.ShortName;
    NewForm.Show;
  end;
end;

procedure TfrmStory.btnEditStoryClick (Sender : TObject );
var
  Dialog : TfrmStoryInfo;
begin
	Dialog := TfrmStoryInfo.Create (Application);
  Dialog.Story := Stories.Current;
  Dialog.ShowModal;
  Dialog.Destroy;
  Stories.SetDirty;
  btnSaveStories.Enabled := true;
end;

procedure TfrmStory.btnSaveStoriesClick (Sender : TObject );
begin
  Stories.SaveStoryList;
  btnSaveStories.Enabled := FALSE;
end;

procedure TfrmStory.FormClose (Sender : TObject; var CloseAction : TCloseAction );
begin
	if (Stories.Dirty) then
    if (Application.MessageBox ('Do you wish to save the Story List?',
    	'Story List Changed', MB_ICONQUESTION + MB_YESNO) = IDYES) then
      Stories.SaveStoryList;
end;

procedure TfrmStory.FormResize (Sender : TObject );
var
  x,
  y : integer;
begin
  lstStories.Width := (Width - 32) div 3;
  lstStories.Height := Height - 80;

  txtTitle.Width := (Width - 32) div 3 * 2 + 8;
  txtTitle.Left := (Width - 32) div 3 + 16;

  labProfiles.Left := (Width - 32) div 3 * 2 + 24;
  lstProfiles.Left := (Width - 32) div 3 * 2 + 24;
  lstProfiles.Width := (Width - 32) div 3;
  lstProfiles.Height := Height - 144;

  x := (Width - 32) div 3;
  y := (Width - 32) div 3 + 16;
  btnEditStory.Width := x;
  btnEditStory.Left := y;
  btnEditChapters.Width := x;
  btnEditChapters.Left := y;
  btnBlurb.Width := x;
  btnBlurb.Left := y;
  btnDisclaimer.Width := x;
  btnDisclaimer.Left := y;
  btnCredits.Width := x;
  btnCredits.Left := y;
  btnBuild.Width := x;
  btnBuild.Left := y;
  btnMake.Width := x;
  btnMake.Left := y;
  btnProfiles.Width := x;
  btnProfiles.Left := y;

  btnAddStory.Left := 8;
  btnAddStory.Top := Height - 44;
  btnDeleteStory.Left := (x - btnDeleteStory.Width) + 8;
  btnDeleteStory.Top := Height - 44;
  btnSaveStories.Left := (lstStories.Width - btnSaveStories.Width) div 2 + 8;
  btnSaveStories.Top := Height - 44;
end;

procedure TfrmStory.lstStoriesClick (Sender : TObject );
var
  index : integer;
  s : string;
begin
  index := lstStories.ItemIndex;
  btnDeleteStory.Enabled := FALSE;

  if (index in [0..lstStories.Items.Count]) then begin
    s := lstStories.Items [index];
    Stories.SelectStory (s);

    // Disable Controls
    txtTitle.Enabled := FALSE;
    btnEditStory.Enabled := FALSE;
    btnEditChapters.Enabled := FALSE;
    btnBlurb.Enabled := FALSE;
    btnDisclaimer.Enabled := FALSE;
    btnCredits.Enabled := FALSE;
    btnProfiles.Enabled := FALSE;

    // Load Controls
    txtTitle.Text := Stories.Current.Title;

    // Enable Controls
    if (Stories.Current <> nil) then begin
      txtTitle.Enabled := TRUE;
      btnDeleteStory.Enabled := TRUE;
      btnEditStory.Enabled := TRUE;
      btnEditChapters.Enabled := TRUE;
      btnBlurb.Enabled := TRUE;
      btnDisclaimer.Enabled := TRUE;
      btnCredits.Enabled := TRUE;
      btnProfiles.Enabled := TRUE;
    end;
  end;
end;

procedure TfrmStory.SetBaseDir (aDir : string);
begin
  if (Stories <> nil) then
  	Stories.BaseDir := aDir;
end;

procedure TfrmStory.PopulateStoryList;
var
  index : integer;
begin
  lstStories.Items.Clear;
  if (Stories.Count > 0) then
    for index := 0 to (Stories.Count - 1) do
  		lstStories.Items.Add (Stories.StoryTitle (index));
  btnSaveStories.Enabled := Stories.Dirty;
end;

procedure TfrmStory.ForceLoadStoryList;
begin
  if (Stories.BaseDir <> '') then
    Stories.LoadStoryList;
  PopulateStoryList;
end;

end.

