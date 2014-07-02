program ficmake;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fvolume, gprofile, gvolume, gtools, fnewvol, fstory, gstory, fnewfic,
  foptions, fficinfo, gchapter, fchapter, fnewchap, dgroff, fnewprof, fpdfpro,
  doption, dtools, fhdhelp, fhtmlpro, ftextpro, fepubpro, fbaredit, fimprof,
  fstorypr, feditor, gmake, flog, fothers, gothers, fpickchap;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmVolume, frmVolume);
  Application.CreateForm(TfrmHeaderHelp, frmHeaderHelp);
  Application.CreateForm(TfrmLog, frmLog);
  Application.Run;
end.

