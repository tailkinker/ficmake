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
  fstorypr, feditor, gmake, flog, fothers, gothers, fpickchap, fabout,
  fmacedit, dversion;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmVolume, frmVolume);
  Application.CreateForm(TfrmHeaderHelp, frmHeaderHelp);
  Application.CreateForm(TfrmLog, frmLog);
  Application.Run;
end.

