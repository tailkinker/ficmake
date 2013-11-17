program ficmake;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fvolume, gprofile, gvolume, gtools, fnewvol, fstory, gstory, fnewfic,
  foptions, fficinfo, gchapter, fchapter, fnewchap
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm (TfrmVolume, frmVolume );
  Application.Run;
end.

