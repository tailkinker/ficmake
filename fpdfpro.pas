unit fpdfpro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons,
  gprofile;

type

  { TfrmPDFProfile }

  TfrmPDFProfile = class (TForm )
    btnOutDir : TBitBtn;
    chkLandscape : TCheckBox;
    cmbColumns : TComboBox;
    cmbPageSize : TComboBox;
    cmbPageSizeUnits : TComboBox;
    Label1 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    Label7 : TLabel;
    Label8 : TLabel;
    labPageSize : TLabel;
    pgPDF : TPageControl;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    sBackground : TShape;
    sLeftMargin : TShape;
    sLeftPage : TShape;
    sRightMargin : TShape;
    sRightPage : TShape;
    tabContent : TTabSheet;
    tabPage : TTabSheet;
    tabStyles : TTabSheet;
    txtBottomMargin : TEdit;
    txtInsideMargin : TEdit;
    txtName : TLabeledEdit;
    txtOutDir : TLabeledEdit;
    txtOutsideMargin : TEdit;
    txtPDFHeight : TEdit;
    txtPDFWidth : TEdit;
    txtTopMargin : TEdit;
    procedure btnOutDirClick(Sender: TObject);
    procedure chkLandscapeChange(Sender: TObject);
    procedure cmbColumnsChange(Sender: TObject);
    procedure cmbPageSizeChange(Sender: TObject);
    procedure cmbPageSizeUnitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure txtBottomMarginChange(Sender: TObject);
    procedure txtInsideMarginChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtOutDirChange(Sender: TObject);
    procedure txtOutsideMarginChange(Sender: TObject);
    procedure txtPDFHeightChange(Sender: TObject);
    procedure txtPDFWidthChange(Sender: TObject);
    procedure txtTopMarginChange(Sender: TObject);
  private
    { private declarations }
    t_profile : tPDFProfile;
    procedure ResizePageView;
    procedure ChangePaperSize;
    procedure LoadProfile (aProfile : tPDFProfile);
  public
    property Profile : tPDFProfile read t_profile write LoadProfile;
    { public declarations }
  end;

var
  frmPDFProfile : TfrmPDFProfile;

implementation

uses
  dgroff, dtools, doption,
  gtools;

{$R *.lfm}

procedure TfrmPDFProfile.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmPDFProfile.txtBottomMarginChange(Sender: TObject);
var
  n : real;
begin
  val (txtBottomMargin.Text, n);
  Profile.BottomMargin := round (n * ScaleMult);
  if (Profile.BottomMargin = 0) then
    txtBottomMargin.Color := clRed
  else
    txtBottomMargin.Color := clDefault;
  ResizePageView;
end;

procedure TfrmPDFProfile.txtInsideMarginChange(Sender: TObject);
var
  n : real;
begin
  val (txtInsideMargin.Text, n);
  Profile.InnerMargin := round (n * ScaleMult);
  if (Profile.InnerMargin = 0) then
    txtInsideMargin.Color := clRed
  else
    txtInsideMargin.Color := clDefault;
  ResizePageView;
end;

procedure TfrmPDFProfile.LoadProfile (aProfile : tPDFProfile);
begin
  t_profile := aProfile;
  txtName.Text := aProfile.Name;
  cmbPageSize.ItemIndex := aProfile.PageSize;
  chkLandscape.Checked := aProfile.Landscape;
  txtPDFHeight.Text := BasicReal (aProfile.PageV / ScaleMult);
  txtPDFWidth.Text := BasicReal (aProfile.PageH / ScaleMult);
  cmbPageSizeUnits.ItemIndex := ScaleType;
  cmbColumns.ItemIndex := aProfile.Columns - 1;

  // Margins
  txtOutsideMargin.Text := BasicReal (aProfile.OuterMargin / ScaleMult);
  txtInsideMargin.Text := BasicReal (aProfile.InnerMargin / ScaleMult);
  txtTopMargin.Text := BasicReal (aProfile.TopMargin / ScaleMult);
  txtBottomMargin.Text := BasicReal (aProfile.BottomMargin / ScaleMult);

  ChangePaperSize;
end;

procedure TfrmPDFProfile.btnOutDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtOutDir.Text := SelectDirectoryDialog1.Filename;
  end;
end;

procedure TfrmPDFProfile.chkLandscapeChange(Sender: TObject);
begin
  Profile.Landscape := chkLandscape.Checked;
  ChangePaperSize;
end;

procedure TfrmPDFProfile.cmbColumnsChange(Sender: TObject);
begin
  Profile.Columns := cmbColumns.ItemIndex + 1;
end;

procedure TfrmPDFProfile.cmbPageSizeChange(Sender: TObject);
begin
  Profile.PageSize := cmbPageSize.ItemIndex;
  if (Profile.PageSize = PaperCount) then begin
    txtPDFWidth.Enabled := TRUE;
    txtPDFHeight.Enabled := TRUE;
    chkLandscape.Enabled := FALSE;
  end else begin
    txtPDFWidth.Enabled := FALSE;
    txtPDFHeight.Enabled := FALSE;
    chkLandscape.Enabled := TRUE;
  end;
  ChangePaperSize;
end;

procedure TfrmPDFProfile.cmbPageSizeUnitsChange(Sender: TObject);
begin
  if (cmbPageSize.ItemIndex in [0..2]) then
    SetScale (cmbPageSizeUnits.ItemIndex);
  txtPDFHeight.Text := BasicReal (Profile.PageV / ScaleMult);
  txtPDFWidth.Text := BasicReal (Profile.PageH / ScaleMult);
end;

procedure TfrmPDFProfile.txtNameChange(Sender: TObject);
begin
  if (txtName.Enabled) then
    if (length (txtName.Text) > 0) then
      Profile.Name := txtName.Text;
end;

procedure TfrmPDFProfile.txtOutDirChange(Sender: TObject);
begin
  if (txtOutDir.Enabled) then
    Profile.OutputDir := txtOutDir.Text;
end;

procedure TfrmPDFProfile.txtOutsideMarginChange(Sender: TObject);
var
  n : real;
begin
  val (txtOutsideMargin.Text, n);
  Profile.OuterMargin := round (n * ScaleMult);
  if (Profile.OuterMargin = 0) then
    txtOutsideMargin.Color := clRed
  else
    txtOutsideMargin.Color := clDefault;
  ResizePageView;
end;

procedure TfrmPDFProfile.txtPDFHeightChange(Sender: TObject);
var
  n : real;
begin
  val (txtPDFHeight.Text, n);
  Profile.PageV := round (n * ScaleMult);
  if (Profile.PageV = 0) then
    txtPDFHeight.Color := clRed
  else
    txtPDFHeight.Color := clDefault;
  ResizePageView;
end;

procedure TfrmPDFProfile.txtPDFWidthChange(Sender: TObject);
var
  n : real;
begin
  val (txtPDFWidth.Text, n);
  Profile.PageH := round (n * ScaleMult);
  if (Profile.PageH = 0) then
    txtPDFWidth.Color := clRed
  else
    txtPDFWidth.Color := clDefault;
  ResizePageView;
end;

procedure TfrmPDFProfile.txtTopMarginChange(Sender: TObject);
var
  n : real;
begin
  val (txtTopMargin.Text, n);
  Profile.TopMargin := round (n * ScaleMult);
  if (Profile.TopMargin = 0) then
    txtTopMargin.Color := clRed
  else
    txtTopMargin.Color := clDefault;
  ResizePageView;
end;

procedure TfrmPDFProfile.ResizePageView;
var
  max,
  margin_w,
  margin_x_left,
  margin_x_right,
  margin_h,
  margin_y,
  page_w,
  page_x,
  profile_x,
  page_h,
  page_y,
  profile_y : integer;
	scale : real;
  background_h,
  background_w,
  background_x,
  background_y : integer;
begin
  // Determine Limits
  background_h := (sBackground.Height -  8);
  background_w := (sBackground.Width - 12) div 2;
  background_x := sBackground.Left;
  background_y := sBackground.Top ;

  // Determine Scale
	profile_x := Profile.PageH;
  profile_y := Profile.PageV;
  max := profile_x;
  if (profile_x < profile_y) then
  	max := profile_y;
	if ((background_w / profile_x) > (background_h / profile_y)) then
  	scale := background_h / max
  else
    scale := background_w / max;

  // Determine x and y page sizes
  page_w := round (profile_x * scale);
  page_h := round (profile_y * scale);

  // Determine left-hand corners
  page_x := (background_x + background_w + 6) - (2 + page_w);
  page_y := (background_h - page_h) div 2 + background_y + 4;

  // Move and resize page graphics
  sLeftPage.Height := page_h;
  sLeftPage.Width := page_w;
  sRightPage.Height := page_h;
  sRightPage.Width := page_w;

  sLeftPage.Left := page_x;
  sLeftPage.Top := page_y;
  sRightPage.Left := (page_x + page_w + 4);
  sRightPage.Top := page_y;

  // Now for the margins sizes
  margin_w := page_w - round ((Profile.InnerMargin + Profile.OuterMargin) * scale);
  margin_h := page_h - round ((Profile.TopMargin + Profile.BottomMargin) * scale);

  // Determine margin corners
  margin_x_left := page_x + round (Profile.OuterMargin * scale);
  margin_x_right := (page_x + page_w + 4) + round (Profile.InnerMargin * scale);
  margin_y := page_y + round (Profile.TopMargin * scale);

  // Move and resize margin graphics
  sLeftMargin.Width := margin_w;
  sLeftMargin.Height := margin_h;
  sRightMargin.Width := margin_w;
  sRightMargin.Height := margin_h;

  sLeftMargin.Left := margin_x_left;
  sLeftMargin.Top := margin_y;
  sRightMargin.Left := margin_x_right;
  sRightMargin.Top := margin_y;
end;

procedure TfrmPDFProfile.ChangePaperSize;
begin
  if (chkLandscape.Checked) then begin
    if (Profile.PageSize < PaperCount) then begin
      Profile.PageH := round (PaperMeasurements [Profile.PageSize, 1]);
      Profile.PageV := round (PaperMeasurements [Profile.PageSize, 0]);
    end;
    txtPDFWidth.Text := BasicReal (Profile.PageH / ScaleMult);
    txtPDFHeight.Text := BasicReal (Profile.PageV / ScaleMult);
  end else begin
    if (Profile.PageSize < PaperCount) then begin
      Profile.PageH := round (PaperMeasurements [Profile.PageSize, 0]);
      Profile.PageV := round (PaperMeasurements [Profile.PageSize, 1]);
    end;
    txtPDFWidth.Text := BasicReal (Profile.PageH / ScaleMult);
    txtPDFHeight.Text := BasicReal (Profile.PageV / ScaleMult);
  end;
  ResizePageView;
end;


end.

