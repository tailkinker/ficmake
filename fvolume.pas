unit fvolume;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TfrmVolume }

  TfrmVolume = class(TForm)
    btnSelectDir: TBitBtn;
    chkHTMLIndex: TCheckBox;
    Label1: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    txtVolumeName: TLabeledEdit;
    lstVolumes: TListBox;
    txtDirectory: TLabeledEdit;
    txtAuthor: TLabeledEdit;
    procedure btnSelectDirClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmVolume: TfrmVolume;

implementation

{$R *.lfm}

{ TfrmVolume }

procedure TfrmVolume.FormResize(Sender: TObject);
begin
  lstVolumes.Width := (frmVolume.Width - 32) div 3;
  lstVolumes.Height := frmVolume.Height - 64;

  txtVolumeName.Width := (frmVolume.Width - 32) div 3 * 2 + 8;
  txtVolumeName.Top := 32;
  txtVolumeName.Left := (frmVolume.Width - 32) div 3 + 16;

  txtDirectory.Width := (frmVolume.Width - 32) div 3 * 2 - 24;
  txtDirectory.Top := 96;
  txtDirectory.Left := (frmVolume.Width - 32) div 3 + 16;

  txtAuthor.Width := (frmVolume.Width - 32) div 3 * 2 + 8;
  txtAuthor.Top := 160;
  txtAuthor.Left := (frmVolume.Width - 32) div 3 + 16;

  chkHTMLIndex.Left := (frmVolume.Width - 32) div 3 + 16;
  chkHTMLIndex.Top := 192;

  btnSelectDir.Left := (frmVolume.Width - 40);
  btnSelectDir.Top := 96;
end;

procedure TfrmVolume.btnSelectDirClick(Sender: TObject);
begin
  if (SelectDirectoryDialog1.Execute) then begin
    txtDirectory.Text := SelectDirectoryDialog1.Filename;
  end;
end;

end.

