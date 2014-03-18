unit fepubpro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls,
  gprofile;

type

  { TfrmEPubProfile }

  TfrmEPubProfile = class(TForm)
    btnClose: TButton;
    btnOutDir: TBitBtn;
    chkBlurbInEPub: TCheckBox;
    chkUsePreconv: TCheckBox;
    txtEPubSeries: TLabeledEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    txtName: TLabeledEdit;
    txtOutDir: TLabeledEdit;
    txtSeparator: TLabeledEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOutDirClick(Sender: TObject);
    procedure chkBlurbInEPubChange(Sender: TObject);
    procedure chkUsePreconvChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure txtEPubSeriesChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtOutDirChange(Sender: TObject);
    procedure txtSeparatorChange(Sender: TObject);
  private
    { private declarations }
    t_profile : tEPubProfile;
    procedure LoadProfile (aProfile : tEPubProfile);
  public
    { public declarations }
    property Profile : tEPubProfile read t_profile write LoadProfile;
  end;

implementation

procedure TfrmEPubProfile.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmEPubProfile.txtEPubSeriesChange(Sender: TObject);
begin
  Profile.EPubSeries := txtEPubSeries.Text;
end;

procedure TfrmEPubProfile.btnOutDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtOutDir.Text := SelectDirectoryDialog1.Filename;
  end;
end;

procedure TfrmEPubProfile.btnCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TfrmEPubProfile.chkBlurbInEPubChange(Sender: TObject);
begin
  Profile.BlurbInEPub := chkBlurbInEPub.Checked;
end;

procedure TfrmEPubProfile.chkUsePreconvChange(Sender: TObject);
begin
  Profile.UsePreconv := chkUsePreconv.Checked;
end;

procedure TfrmEPubProfile.txtNameChange(Sender: TObject);
begin
  if (length (txtName.Text) > 0) then begin
    Profile.Name := txtName.Text;
    txtName.Color := clDefault;
    btnClose.Enabled := TRUE
  end else begin
    txtName.Color := clRed;
    btnClose.Enabled := FALSE;
  end;
end;

procedure TfrmEPubProfile.txtOutDirChange(Sender: TObject);
begin
  if (txtOutDir.Enabled) then
    Profile.OutputDir := txtOutDir.Text;
end;

procedure TfrmEPubProfile.txtSeparatorChange(Sender: TObject);
begin
  Profile.Separator := txtSeparator.Text;
end;

procedure TfrmEPubProfile.LoadProfile (aProfile : tEPubProfile);
begin
  t_profile := aProfile;
  txtName.Text := aProfile.Name;
  txtOutDir.Text := aProfile.OutputDir;
  txtSeparator.Text := aProfile.Separator;
  txtEPubSeries.Text := aProfile.EPubSeries;
  chkBlurbInEPub.Checked := aProfile.BlurbInEPub;
  chkUsePreconv.Checked := aProfile.UsePreconv;

end;

{$R *.lfm}

end.

