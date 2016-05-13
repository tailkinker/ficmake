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

unit fpdfpro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, LCLType,
  gprofile;

type

  { TfrmPDFProfile }

  TfrmPDFProfile = class (TForm )
    btnOutDir : TBitBtn;
    btnVariableHelp: TButton;
    btnClose: TButton;
    chkCallEQN: TCheckBox;
    chkCallTBL: TCheckBox;
    chkCallPreconv: TCheckBox;
    chkTitlePageOneColumn: TCheckBox;
    chkFontCentered: TCheckBox;
    chkFontItalics: TCheckBox;
    chkFontBold: TCheckBox;
    chkEvenFooterEnable: TCheckBox;
    chkOddFooterEnable: TCheckBox;
    chkEvenHeaderEnable: TCheckBox;
    chkOddHeaderEnable: TCheckBox;
    chkLandscape : TCheckBox;
    cmbColumns : TComboBox;
    cmbPageSize : TComboBox;
    cmbPageSizeUnits : TComboBox;
    cmbFontFamily: TComboBox;
    cmbH1CenterMode: TComboBox;
    Label10: TLabel;
    Label9: TLabel;
    radForceFirstPage: TRadioGroup;
    txtSeparator: TLabeledEdit;
    Panel1: TPanel;
    btnBordersTop: TToggleBox;
    btnBordersBottom: TToggleBox;
    btnBordersLeft: TToggleBox;
    btnBordersRight: TToggleBox;
    txtFontSize: TLabeledEdit;
    txtSpaceAbove: TLabeledEdit;
    Label2: TLabel;
    lstStyles: TListBox;
    txtOddHeaderLeft: TEdit;
    txtEvenFooterLeft: TEdit;
    txtEvenFooterMiddle: TEdit;
    txtEvenFooterRight: TEdit;
    txtOddHeaderMiddle: TEdit;
    txtOddHeaderRight: TEdit;
    txtEvenHeaderLeft: TEdit;
    txtEvenHeaderMiddle: TEdit;
    txtEvenHeaderRight: TEdit;
    txtOddFooterLeft: TEdit;
    txtOddFooterMiddle: TEdit;
    txtOddFooterRight: TEdit;
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
    txtSpaceBelow: TLabeledEdit;
    txtSpaceReserved: TLabeledEdit;
    txtTopMargin : TEdit;
    procedure btnBordersTopChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOutDirClick(Sender: TObject);
    procedure btnVariableHelpClick(Sender: TObject);
    procedure chkCallEQNChange(Sender: TObject);
    procedure chkCallPreconvChange(Sender: TObject);
    procedure chkCallTBLChange(Sender: TObject);
    procedure chkEvenFooterEnableChange(Sender: TObject);
    procedure chkEvenHeaderEnableChange(Sender: TObject);
    procedure chkFontBoldChange(Sender: TObject);
    procedure chkFontCenteredChange(Sender: TObject);
    procedure chkFontItalicsChange(Sender: TObject);
    procedure chkLandscapeChange(Sender: TObject);
    procedure chkOddFooterEnableChange(Sender: TObject);
    procedure chkOddHeaderEnableChange(Sender: TObject);
    procedure chkTitlePageOneColumnChange(Sender: TObject);
    procedure cmbColumnsChange(Sender: TObject);
    procedure cmbFontFamilyChange(Sender: TObject);
    procedure cmbH1CenterModeChange(Sender: TObject);
    procedure cmbPageSizeChange(Sender: TObject);
    procedure cmbPageSizeUnitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstStylesClick(Sender: TObject);
    procedure radForceFirstPageClick(Sender: TObject);
    procedure txtBottomMarginChange(Sender: TObject);
    procedure txtEvenFooterLeftChange(Sender: TObject);
    procedure txtEvenFooterMiddleChange(Sender: TObject);
    procedure txtEvenFooterRightChange(Sender: TObject);
    procedure txtEvenHeaderLeftChange(Sender: TObject);
    procedure txtEvenHeaderMiddleChange(Sender: TObject);
    procedure txtEvenHeaderRightChange(Sender: TObject);
    procedure txtFontSizeChange(Sender: TObject);
    procedure txtInsideMarginChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtOddFooterLeftChange(Sender: TObject);
    procedure txtOddFooterMiddleChange(Sender: TObject);
    procedure txtOddFooterRightChange(Sender: TObject);
    procedure txtOddHeaderLeftChange(Sender: TObject);
    procedure txtOddHeaderMiddleChange(Sender: TObject);
    procedure txtOddHeaderRightChange(Sender: TObject);
    procedure txtOutDirChange(Sender: TObject);
    procedure txtOutsideMarginChange(Sender: TObject);
    procedure txtPDFHeightChange(Sender: TObject);
    procedure txtPDFWidthChange(Sender: TObject);
    procedure txtSeparatorChange(Sender: TObject);
    procedure txtSpaceAboveChange(Sender: TObject);
    procedure txtSpaceBelowChange(Sender: TObject);
    procedure txtSpaceReservedChange(Sender: TObject);
    procedure txtTopMarginChange(Sender: TObject);
  private
    { private declarations }
    t_profile : tPDFProfile;
    CurrentFont : integer;
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
  fhdhelp,
  dgroff, doption,
  gtools;

{$R *.lfm}

procedure TfrmPDFProfile.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  pgPDF.ActivePage := tabPage;
end;

procedure TfrmPDFProfile.lstStylesClick(Sender: TObject);
begin
  chkFontCentered.Enabled := FALSE;
  cmbH1CenterMode.Enabled := FALSE;
  btnBordersTop.Enabled := FALSE;
  btnBordersBottom.Enabled := FALSE;
  btnBordersLeft.Enabled := FALSE;
  btnBordersRight.Enabled := FALSE;
  txtSpaceAbove.Enabled := FALSE;
  txtSpaceBelow.Enabled := FALSE;
  txtSpaceReserved.Enabled := FALSE;
  Panel1.Enabled := FALSE;

  if (lstStyles.ItemIndex in [0..11]) then begin
    CurrentFont := lstStyles.ItemIndex;
    with (Profile.GroffFonts [CurrentFont]) do begin
      cmbFontFamily.ItemIndex := Family;
      if ((Font AND 1) = 1) then
        chkFontBold.Checked := TRUE
      else
        chkFontBold.Checked := FALSE;
      if ((Font AND 2) = 2) then
        chkFontItalics.Checked := TRUE
      else
        chkFontItalics.Checked := FALSE;
      chkFontCentered.Checked := Centered;
      txtFontSize.Text := BasicReal (Size);
      txtSpaceReserved.Text := BasicReal (ReserveSpace);
      txtSpaceAbove.Text := BasicReal (SpaceAbove);
      txtSpaceBelow.Text := BasicReal (SpaceBelow);

      btnBordersTop.Checked := Borders [0];
      btnBordersBottom.Checked := Borders [1];
      btnBordersLeft.Checked := Borders [2];
      btnBordersRight.Checked := Borders [3];
    end;

    Label2.Enabled := TRUE;
    cmbFontFamily.Enabled := TRUE;
    chkFontBold.Enabled := TRUE;
    chkFontItalics.Enabled := TRUE;
    if (CurrentFont <> 4) then
      chkFontCentered.Enabled := TRUE
    else begin
      Label9.Enabled := TRUE;
      cmbH1CenterMode.Enabled := TRUE;
    end;
    txtFontSize.Enabled := TRUE;
    if (CurrentFont in [3..8]) then begin
      if (CurrentFont > 4) then
        txtSpaceReserved.Enabled := TRUE;
      Panel1.Enabled := TRUE;
      btnBordersTop.Enabled := TRUE;
      btnBordersBottom.Enabled := TRUE;
      btnBordersLeft.Enabled := TRUE;
      btnBordersRight.Enabled := TRUE;
    end;
    if (CurrentFont in [1..8]) then begin
      txtSpaceAbove.Enabled := TRUE;
      txtSpaceBelow.Enabled := TRUE;
    end;
  end;
end;

procedure TfrmPDFProfile.radForceFirstPageClick(Sender: TObject);
begin
  if (radForceFirstPage.Enabled) then
    Profile.ForceFirstPage := radForceFirstPage.ItemIndex;
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

procedure TfrmPDFProfile.txtEvenFooterLeftChange(Sender: TObject);
begin
  Profile.EvenFooter.Left := txtEvenFooterLeft.Text;
end;

procedure TfrmPDFProfile.txtEvenFooterMiddleChange(Sender: TObject);
begin
  Profile.EvenFooter.Middle := txtEvenFooterMiddle.Text;
end;

procedure TfrmPDFProfile.txtEvenFooterRightChange(Sender: TObject);
begin
  Profile.EvenFooter.Right := txtEvenFooterRight.Text;
end;

procedure TfrmPDFProfile.txtEvenHeaderLeftChange(Sender: TObject);
begin
  Profile.EvenHeader.Left := txtEvenHeaderLeft.Text;
end;

procedure TfrmPDFProfile.txtEvenHeaderMiddleChange(Sender: TObject);
begin
  Profile.EvenHeader.Middle := txtEvenHeaderMiddle.Text;
end;

procedure TfrmPDFProfile.txtEvenHeaderRightChange(Sender: TObject);
begin
  Profile.EvenHeader.Right := txtEvenHeaderRight.Text;
end;

procedure TfrmPDFProfile.txtFontSizeChange(Sender: TObject);
var
  n : integer;
begin
  val (txtFontSize.Text, n);
  Profile.GroffFonts [CurrentFont].Size := n;
  if (n = 0) then
    txtFontSize.Color := clRed
  else
    txtFontSize.Color := clDefault;
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
  txtOutDir.Text := aProfile.OutputDir;
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

  txtSeparator.Text := aProfile.Separator;

  // Headers
  chkOddHeaderEnable.Checked := aProfile.OddHeader.Enabled;
  txtOddHeaderLeft.Text := aProfile.OddHeader.Left;
  txtOddHeaderMiddle.Text := aProfile.OddHeader.Middle;
  txtOddHeaderRight.Text := aProfile.OddHeader.Right;

  chkEvenHeaderEnable.Checked := aProfile.EvenHeader.Enabled;
  txtEvenHeaderLeft.Text := aProfile.EvenHeader.Left;
  txtEvenHeaderMiddle.Text := aProfile.EvenHeader.Middle;
  txtEvenHeaderRight.Text := aProfile.EvenHeader.Right;

  chkOddFooterEnable.Checked := aProfile.OddFooter.Enabled;
  txtOddFooterLeft.Text := aProfile.OddFooter.Left;
  txtOddFooterMiddle.Text := aProfile.OddFooter.Middle;
  txtOddFooterRight.Text := aProfile.OddFooter.Right;

  chkEvenFooterEnable.Checked := aProfile.EvenFooter.Enabled;
  txtEvenFooterLeft.Text := aProfile.EvenFooter.Left;
  txtEvenFooterMiddle.Text := aProfile.EvenFooter.Middle;
  txtEvenFooterRight.Text := aProfile.EvenFooter.Right;

  // Styles
  chkTitlePageOneColumn.Checked := aProfile.OneColumnTitlePage;
  if (aProfile.Columns > 1) then
    chkTitlePageOneColumn.Enabled := TRUE;

  // Other Crap
  radForceFirstPage.ItemIndex := aProfile.ForceFirstPage;
  chkCallPreconv.Checked := aProfile.UsePreconv;
  chkCallTBL.Checked := aProfile.UseTBL;
  chkCallEQN.Checked := aProfile.UseEQN;

  ChangePaperSize;
end;

procedure TfrmPDFProfile.btnOutDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtOutDir.Text := SelectDirectoryDialog1.Filename;
  end;
end;

procedure TfrmPDFProfile.btnBordersTopChange(Sender: TObject);
begin
  Profile.GroffFonts [CurrentFont].Borders [TToggleBox (Sender).Tag] :=
    TToggleBox (Sender).Checked;
  chkCallTBL.Checked := chkCallTBL.Checked OR (TToggleBox (Sender).Checked);
end;

procedure TfrmPDFProfile.btnCloseClick(Sender: TObject);
begin
  if (DirectoryExists (Profile.OutputDir)) then
    Hide
  else
    Application.MessageBox
      ('Cannot save this profile with an invalid output directory',
      'Invalid Directory', MB_ICONHAND + MB_OK);
end;

procedure TfrmPDFProfile.btnVariableHelpClick(Sender: TObject);
begin
  frmHeaderHelp.Visible := true;
end;

procedure TfrmPDFProfile.chkCallEQNChange(Sender: TObject);
begin
  Profile.UseEQN := chkCallEQN.Checked;
end;

procedure TfrmPDFProfile.chkCallPreconvChange(Sender: TObject);
begin
  Profile.UsePreconv := chkCallPreconv.Checked;
end;

procedure TfrmPDFProfile.chkCallTBLChange(Sender: TObject);
begin
  Profile.UseTBL := chkCallTBL.Checked;
end;

procedure TfrmPDFProfile.chkEvenFooterEnableChange(Sender: TObject);
begin
  Profile.EvenFooter.Enabled := chkEvenFooterEnable.Checked;
  txtEvenFooterLeft.Enabled := Profile.EvenFooter.Enabled;
  txtEvenFooterMiddle.Enabled := Profile.EvenFooter.Enabled;
  txtEvenFooterRight.Enabled := Profile.EvenFooter.Enabled;
end;

procedure TfrmPDFProfile.chkEvenHeaderEnableChange(Sender: TObject);
begin
  Profile.EvenHeader.Enabled := chkEvenHeaderEnable.Checked;
  txtEvenHeaderLeft.Enabled := Profile.EvenHeader.Enabled;
  txtEvenHeaderMiddle.Enabled := Profile.EvenHeader.Enabled;
  txtEvenHeaderRight.Enabled := Profile.EvenHeader.Enabled;
end;

procedure TfrmPDFProfile.chkFontBoldChange(Sender: TObject);
begin
  with (Profile.GroffFonts [CurrentFont]) do
    if (chkFontBold.Checked) then
      Font := Font OR 1
    else
      Font := Font AND 254;
end;

procedure TfrmPDFProfile.chkFontCenteredChange(Sender: TObject);
begin
  Profile.GroffFonts [CurrentFont].Centered := chkFontCentered.Checked;
end;

procedure TfrmPDFProfile.chkFontItalicsChange(Sender: TObject);
begin
  with (Profile.GroffFonts [CurrentFont]) do
    if (chkFontItalics.Checked) then
      Font := Font OR 2
    else
      Font := Font AND 253;
end;

procedure TfrmPDFProfile.chkLandscapeChange(Sender: TObject);
begin
  Profile.Landscape := chkLandscape.Checked;
  ChangePaperSize;
end;

procedure TfrmPDFProfile.chkOddFooterEnableChange(Sender: TObject);
begin
  Profile.OddFooter.Enabled := chkOddFooterEnable.Checked;
  txtOddFooterLeft.Enabled := Profile.OddFooter.Enabled;
  txtOddFooterMiddle.Enabled := Profile.OddFooter.Enabled;
  txtOddFooterRight.Enabled := Profile.OddFooter.Enabled;
end;

procedure TfrmPDFProfile.chkOddHeaderEnableChange(Sender: TObject);
begin
  Profile.OddHeader.Enabled := chkOddHeaderEnable.Checked;
  txtOddHeaderLeft.Enabled := Profile.OddHeader.Enabled;
  txtOddHeaderMiddle.Enabled := Profile.OddHeader.Enabled;
  txtOddHeaderRight.Enabled := Profile.OddHeader.Enabled;
end;

procedure TfrmPDFProfile.chkTitlePageOneColumnChange(Sender: TObject);
begin
  Profile.OneColumnTitlePage := chkTitlePageOneColumn.Checked;
end;

procedure TfrmPDFProfile.cmbColumnsChange(Sender: TObject);
begin
  Profile.Columns := cmbColumns.ItemIndex + 1;
  if (Profile.Columns > 1) then
    chkTitlePageOneColumn.Enabled := TRUE
  else
    chkTitlePageOneColumn.Enabled := FALSE;
end;

procedure TfrmPDFProfile.cmbFontFamilyChange(Sender: TObject);
begin
  Profile.GroffFonts [CurrentFont].Family := cmbFontFamily.ItemIndex;
end;

procedure TfrmPDFProfile.cmbH1CenterModeChange(Sender: TObject);
begin
  Profile.H1Mode := cmbH1CenterMode.ItemIndex;
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
    if (length (txtName.Text) > 0) then begin
      Profile.Name := txtName.Text;
      txtName.Color := clDefault;
      btnClose.Enabled := TRUE
    end else begin
      txtName.Color := clRed;
      btnClose.Enabled := FALSE;
    end;
end;

procedure TfrmPDFProfile.txtOddFooterLeftChange(Sender: TObject);
begin
  Profile.OddFooter.Left := txtOddFooterLeft.Text;
end;

procedure TfrmPDFProfile.txtOddFooterMiddleChange(Sender: TObject);
begin
  Profile.OddFooter.Middle := txtOddFooterMiddle.Text;
end;

procedure TfrmPDFProfile.txtOddFooterRightChange(Sender: TObject);
begin
  Profile.OddFooter.Right := txtOddFooterRight.Text;
end;

procedure TfrmPDFProfile.txtOddHeaderLeftChange(Sender: TObject);
begin
  Profile.OddHeader.Left := txtOddHeaderLeft.Text;
end;

procedure TfrmPDFProfile.txtOddHeaderMiddleChange(Sender: TObject);
begin
  Profile.OddHeader.Middle := txtOddHeaderMiddle.Text;
end;

procedure TfrmPDFProfile.txtOddHeaderRightChange(Sender: TObject);
begin
  Profile.OddHeader.Right := txtOddHeaderRight.Text;
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

procedure TfrmPDFProfile.txtSeparatorChange(Sender: TObject);
begin
  Profile.Separator := txtSeparator.Text;
end;

procedure TfrmPDFProfile.txtSpaceAboveChange(Sender: TObject);
var
  n : integer;
begin
  val (txtSpaceAbove.Text, n);
  Profile.GroffFonts [CurrentFont].SpaceAbove := n;
end;

procedure TfrmPDFProfile.txtSpaceBelowChange(Sender: TObject);
var
  n : integer;
begin
  val (txtSpaceBelow.Text, n);
  Profile.GroffFonts [CurrentFont].SpaceBelow := n;
end;

procedure TfrmPDFProfile.txtSpaceReservedChange(Sender: TObject);
var
  n : integer;
begin
  val (txtSpaceReserved.Text, n);
  Profile.GroffFonts [CurrentFont].ReserveSpace := n;
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

