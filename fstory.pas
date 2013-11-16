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
  fnewfic, foptions;

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
  s := lstStories.Items [index];
  if (s <> '') then begin
    Stories.SelectStory (s);

    // Disable Controls
    txtTitle.Enabled := FALSE;
    btnEditStory.Enabled := FALSE;
    btnEditChapters.Enabled := FALSE;
    btnBlurb.Enabled := FALSE;
    btnDisclaimer.Enabled := FALSE;
    btnCredits.Enabled := FALSE;

    // Load Controls
    txtTitle.Text := Stories.Current.Title;

    // Enable Controls
    txtTitle.Enabled := TRUE;
    btnDeleteStory.Enabled := TRUE;
    btnEditStory.Enabled := TRUE;
    btnEditChapters.Enabled := TRUE;
    btnBlurb.Enabled := TRUE;
    btnDisclaimer.Enabled := TRUE;
    btnCredits.Enabled := TRUE;
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

