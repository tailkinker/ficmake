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
    Shape1 : TShape;
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
    procedure FormCreate(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtOutDirChange(Sender: TObject);
  private
    { private declarations }
    procedure ResizePageView;
    procedure ChangePaperSize;
  public
    Profile : tPDFProfile;
    { public declarations }
  end;

var
  frmPDFProfile : TfrmPDFProfile;

implementation

uses
  dgroff, dtools, doption;

{$R *.lfm}

procedure TfrmPDFProfile.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmPDFProfile.btnOutDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtOutDir.Text := SelectDirectoryDialog1.Filename;
  end;
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

procedure TfrmPDFProfile.ResizePageView;
var
  max,
  mx1,
  mx2,
  mx3,
  my1,
  my2,
  x1,
  x2,
  xa,
  y1,
  y2,
  ya : integer;
	scale : real;
  h0,
  w0,
  x0,
  y0 : integer;
begin
  // Determine Limits
  h0 := Shape1.Height - 8;
  w0 := (Shape1.Width - 12) div 2;
  x0 := Shape1.Left;
  y0 := Shape1.Top;

  // Determine Scale
	xa := Profile.PageH;
  ya := Profile.PageV;
  max := xa;
  if (xa < ya) then
  	max := ya;
	if ((w0 / xa) > (h0 / ya)) then
  	scale := h0 / max
  else
    scale := w0 / max;

  // Determine x and y page sizes
  x1 := round (xa * scale);
  y1 := round (ya * scale);

  // Determine left-hand corners
  x2 := (x0 + w0 + 4) - x1;
  y2 := (h0 - y1) div 2 + y1;

  // Move and resize page graphics
  sLeftPage.Height := y1;
  sLeftPage.Width := x1;
  sRightPage.Height := y1;
  sRightPage.Width := x1;

  sLeftPage.Left := x2;
  sLeftPage.Top := y2;
  sRightPage.Left := (x0 + w0 + 8);
  sRightPage.Top := y2;

  // Now for the margins sizes
  mx1 := x1 - round ((Profile.InnerMargin + Profile.OuterMargin) * scale);
  my1 := y1 - round ((Profile.TopMargin + Profile.BottomMargin) * scale);

  // Determine margin corners
  mx2 := x2 + round (Profile.OuterMargin * scale);
  mx3 := 572 + round (Profile.InnerMargin * scale);
  my2 := y2 + round (Profile.TopMargin * scale);

  // Move and resize margin graphics
  sLeftMargin.Width := mx1;
  sLeftMargin.Height := my1;
  sRightMargin.Width := mx1;
  sRightMargin.Height := my1;

  sLeftMargin.Left := mx2;
  sLeftMargin.Top := my2;
  sRightMargin.Left := mx3;
  sRightMargin.Top := my2;
end;

procedure TfrmPDFProfile.ChangePaperSize;
begin
  if (chkLandscape.Checked) then begin
    Profile.PageH := round (PaperMeasurements [Profile.PageSize, 1]);
    Profile.PageV := round (PaperMeasurements [Profile.PageSize, 0]);
    txtPDFWidth.Text := BasicReal (Profile.PageH / ScaleMult);
    txtPDFHeight.Text := BasicReal (Profile.PageV / ScaleMult);
  end else begin
    Profile.PageH := round (PaperMeasurements [Profile.PageSize, 0]);
    Profile.PageV := round (PaperMeasurements [Profile.PageSize, 1]);
    txtPDFWidth.Text := BasicReal (Profile.PageH / ScaleMult);
    txtPDFHeight.Text := BasicReal (Profile.PageV / ScaleMult);
  end;
end;


end.

