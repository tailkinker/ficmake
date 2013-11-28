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
  private
    { private declarations }
    procedure ResizePageView;
  public
    Profile : tPDFProfile;
    { public declarations }
  end;

var
  frmPDFProfile : TfrmPDFProfile;

implementation

{$R *.lfm}

procedure TfrmPDFProfile.ResizePageView;
begin

end;

end.

