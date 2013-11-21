unit foptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmOptions }

  TfrmOptions = class (TForm )
    btnGuess : TButton;
    btnOk : TButton;
    radScreen : TRadioGroup;
    procedure btnGuessClick (Sender : TObject );
    procedure FormCreate (Sender : TObject );
    procedure FormResize (Sender : TObject );
    procedure radScreenClick (Sender : TObject );
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  InitialX,
  InitialY : integer;
  optScreenSize : integer = 0;

implementation

const
  MaxResolution = 9;
	Resolutions : array [0..MaxResolution] of string = (
  	'800x480 (W)',
    '800x600',
    '1024x600 (W)',
    '1024x768',
    '1280x720 (W)',
    '1280x1024',
    '1440x900 (W)',
    '1600x900 (W)',
    '1920x1080 (W)',
    '1024x1280 (P)'
  );

  Initials : array [0..MaxResolution, 0..1] of integer = (
	  (  800,  480 ), // 800x480
	  (  800,  600 ), // 800x600
	  ( 1024,  600 ), // 1024x600
	  ( 1024,  768 ), // 1024x768
	  ( 1280,  720 ), // 1280x720
	  ( 1280, 1024 ), // 1280x1024
	  ( 1440,  900 ), // 1440x900
	  ( 1600,  900 ), // 1600x900
	  ( 1920, 1080 ), // 1920x1080
	  ( 1024, 1280 )  // 1024x1280
  );

{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.FormResize (Sender : TObject );
begin
  btnOk.Left := (Width - btnOk.Width) - 8;
  btnOk.Top := (Height - btnOk.Height) - 8;
  btnGuess.Top := (radScreen.Height + 16);
end;

procedure TfrmOptions.FormCreate (Sender : TObject );
var
  index : integer;
  s,
  t : string;
  r : real;
begin
  radScreen.Items.Clear;
  for index := 0 to MaxResolution do begin
		str (Initials [index, 0], s);
    str (Initials [index, 1], t);
    s += 'x' + t;
    r := Initials [index, 0] / Initials [index, 1];
    if (r > 1.4) then
    	s += ' (W)'
    else if (r < 1) then
      s += ' (P)';
    radScreen.Items.Add (s);
  end;
  Width := radScreen.Width + 16;
  Height := radScreen.Height + 48;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmOptions.btnGuessClick (Sender : TObject );
var
  ScreenMP,
  ThisMP,
  DiffMP : Int64;
  ScreenRatio,
  ThisRatio,
  DiffRatio : real;
  index,
  closest : integer;
begin
	ScreenMP := Screen.Width * Screen.Height;
  ScreenRatio := Screen.Width / Screen.Height;
  DiffMP := 300000000;

  closest := 0;
  for index := 0 to 9 do begin
    ThisMP := Initials [index, 0] * Initials [index, 1];
    ThisRatio := Initials [index, 0] / Initials [index, 1];
    DiffRatio := abs (ThisRatio - ScreenRatio);
    if ((abs (ThisMP - ScreenMP) < DiffMP) and (DiffRatio < 0.2)) then begin
      DiffMP := abs (ThisMP - ScreenMP);
      closest := index;
    end;
  end;
  radScreen.ItemIndex := closest;
end;

procedure TfrmOptions.radScreenClick (Sender : TObject );
begin
  optScreenSize := radScreen.ItemIndex;
  InitialX := ((Initials [optScreenSize, 0] div 10) - 2) * 10;
  InitialY := ((Initials [optScreenSize, 1] div 10) - 8) * 10;
end;

end.

