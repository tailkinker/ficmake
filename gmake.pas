unit gmake;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  gstory, gvolume;

procedure WriteStoryMake (aStory : tStory);
procedure WriteStoryMakeForVolume (aStory : tStory; aVolume : tVolume);
procedure WriteVolumeMake (aVolume : tVolume);
procedure Make (TargetDir : string);

implementation

uses
  process, forms,
  flog,
  gprofile, gchapter, gtools, gothers;

procedure WriteStoryMake (aStory : tStory);
var
  Chapters : tChapterList;
  Profiles : tProfileList;
  DependList : tOtherFileList;
  hsize,
  vsize : real;
  x : text;
  index0,
  index1,
  ChapNum : integer;
  s,
  GroffLine,
  StoryName,
  ProfileName,
  StoryProfileName,
  SourcePath,
  SourceFileName,
  OutputPath : string;
begin
  // Load Chapters and Profiles
  Chapters := tChapterList.Create;
  Chapters.BaseDir:=aStory.SourceDir;
  Chapters.Load;

  Profiles := tProfileList.Create;
  Profiles.BaseDir:=aStory.SourceDir;
  Profiles.Load;

  DependList := tOtherFileList.Create;
  DependList.BaseDir := aStory.SourceDir;
  DependList.Load;

  // Open Makefile
  SourcePath := aStory.SourceDir + '/';
  frmLog.txtLog.Lines.Add (' -> Creating Makefile at ' + SourcePath);
  assign (x, SourcePath + 'Makefile');
  rewrite (x);

  // Write the 'all' rule
  writeln (x, 'all: \');
  for index0 := 0 to (Profiles.Count - 1) do begin
    Profiles.SelectAt (index0);
    OutputPath := Profiles.Current.OutputDir + '/';
    case (Profiles.Current.ProfileType) of
      ptPDF : begin
        write (x, #9, OutputPath + aStory.LongName + '.pdf');
      end;
      ptHTML : begin
        if (tHTMLProfile (Profiles.Current).BulkHTML) then
          write (x, #9, OutputPath + aStory.LongName + '.htm')
        else
          for index1 := 0 to (Chapters.Count - 1) do begin
            Chapters.SelectAt (index1);
            write (x, #9, OutputPath + Chapters.Current.Filename + '.htm');
            if (index1 < (Chapters.Count - 1)) then
              writeln (x, ' \');
          end;
      end;
      ptText : begin
        if (tTextProfile(Profiles.Current).BulkText) then
          write (x, #9, OutputPath + aStory.LongName + '.txt')
        else
          for index1 := 0 to (Chapters.Count - 1) do begin
            Chapters.SelectAt (index1);
            write (x, #9, OutputPath + Chapters.Current.Filename + '.txt');
            if (index1 < (Chapters.Count - 1)) then
              writeln (x, ' \');
          end;
      end;
      ptEPub : begin
        write (x, #9, OutputPath + aStory.LongName + '.epub');
      end;
    end;
    if (index0 < (Profiles.Count - 1)) then
      writeln (x, ' \');
  end;
  writeln (x);
  writeln (x);

  // Next, write dependencies
  if (DependList.Count > 0) then begin
    write (x, 'OTHERFILES = ');
    for index0 := 0 to (DependList.Count - 1) do begin
      DependList.SelectAt (index0);
      write (x, DependList.Current.Filename);
      if (index0 < (DependList.Count - 1)) then begin
        writeln (x, ' \');
        write (x, #9);
      end else begin
        writeln (x);
        writeln (x);
      end;
    end;
  end;

  // Next, write chapter source files
  write (x, 'CHAPTERS = ');
  for index0 := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index0);
  	SourceFileName := Chapters.Current.Filename;
    if (FileExists (SourcePath + '/' + SourceFileName + '.co')) then
      write (x, SourceFileName + '.co ');
    write (x, SourceFileName + '.so ');
    if (FileExists (SourcePath + '/' + SourceFileName + '.tr')) then
      write (x, SourceFileName + '.tr ');
    if (FileExists (SourcePath + '/' + SourceFileName + '.an')) then
      write (x, SourceFileName + '.an ');
    if (FileExists (SourcePath + '/' + SourceFileName + '.om')) then
      write (x, SourceFileName + '.om ');
    if (index0 < (Chapters.Count - 1)) then begin
      writeln (x, ' \');
      write (x, #9);
    end else begin
      writeln (x);
      writeln (x);
    end;
  end;

  // Now, write the groff build lines for each profile.
  for index1 := 0 to (Profiles.Count - 1) do begin
    Profiles.SelectAt (index1);
    StoryName := StripSpaces (aStory.LongName);
    StoryProfileName := StripSpaces (StoryName + '-' + Profiles.Current.Name);
    OutputPath := Profiles.Current.OutputDir;
    ProfileName := StripSpaces (Profiles.Current.Name);

    case (Profiles.Current.ProfileType) of
      ptPDF: begin // PDF
      	// Build the basic Groff call
        groffline := 'groff -Tps -U -ms -s';
        if (TPDFProfile (Profiles.Current).Useeqn) then
          groffline += 'e';
        if (Profiles.Current.Usepreconv) then
          groffline += 'k';
        if (TPDFProfile (Profiles.Current).Usetbl) then
          groffline += 't';
        groffline += ' -P';
        if (TPDFProfile (Profiles.Current).PageSize < PaperCount) then
          groffline += PaperSizes [TPDFProfile (Profiles.Current).PageSize, 2]
        else begin
          hsize := TPDFProfile (Profiles.Current).PageH / 72000;
          vsize := TPDFProfile (Profiles.Current).PageV / 72000;
          str (vsize:0:3, s);
          groffline += '-p' + s + 'i,';
          str (hsize:0:3, s);
          groffline += s + 'i'
        end;
        if (TPDFProfile (Profiles.Current).Landscape) then
          groffline += ' -P-l';
        groffline += ' ';

        writeln (x, OutputPath + '/' + StoryName + '.pdf: '
        	+ StoryProfileName + '.ps');
        writeln (x, #9, 'ps2pdf "' + StoryProfileName + '.ps" \');
        writeln (x, #9, '"' + OutputPath + '/' + StoryName + '.pdf"');
        writeln (x);
        write (x, StoryProfileName + '.ps: $(CHAPTERS) ');
        if (DependList.Count > 0) then
          write (x, '$(OTHERFILES) ');
        write (x, StoryProfileName + '.ms');
        if (FileExists (SourcePath + '/disclaimer.so')) then
          write (x, ' disclaimer.so');
        if (FileExists (SourcePath + '/blurb.so')) then
          write (x, ' blurb.so');
        if (FileExists (SourcePath + '/credits.so')) then
          write (x, ' credits.so');

        writeln (x);
        writeln (x, #9, 'rm -f ',StoryProfileName + '.ref');
        writeln (x, #9, groffline, '-z ', StoryProfileName + '.ms > ',
        	StoryProfileName + '.ps');
        writeln (x, #9, groffline, '-z ', StoryProfileName + '.ms > ',
        	StoryProfileName + '.ps');
        writeln (x, #9, groffline, StoryProfileName + '.ms > ',
        	StoryProfileName + '.ps');
        writeln (x);
      end;

      ptHTML : begin // HTML
      	// Build the basic Groff call
        groffline := 'groff -Thtml -U -ms -s';
        if (Profiles.Current.Usepreconv) then
          groffline += 'k';
        groffline += ' ';

        s := OutputPath + '/' + StoryName;
        if (not THTMLProfile (Profiles.Current).BulkHTML) then
          if (not (DirectoryExists (s))) then begin
            mkdir (s);
            if (IOResult = 0) then
              frmLog.txtLog.Lines.Add ('  -> Creating target directory ' + s + '.');
          end;
        write (x, OutputPath + '/' + StoryName + '.htm: '
        	+ StoryProfileName + '.ms');
        if (THTMLProfile (Profiles.Current).BulkHTML) then
          write (x, ' $(CHAPTERS)');
        if (DependList.Count > 0) then
          write (x, '$(OTHERFILES) ');
        if (FileExists (SourcePath + '/disclaimer.so')) then
          write (x, ' disclaimer.so');
        if (FileExists (SourcePath + '/blurb.so')) then
          write (x, ' blurb.so');
        if (FileExists (SourcePath + '/credits.so')) then
          write (x, ' credits.so');
        writeln (x);
        writeln (x, #9, groffline, StoryProfileName + '.ms > "',
        	OutputPath + '/' + StoryName + '.htm"');
        writeln (x);

        if (not (THTMLProfile (Profiles.Current).BulkHTML)) then begin
          for ChapNum := 0 to (Chapters.Count - 1) do begin
            Chapters.SelectAt (ChapNum);
            write (x, OutputPath + '/' + StoryName + '/'
              + Chapters.Current.Filename + '.htm: ');
            write (x, Chapters.Current.Filename + '-' + ProfileName + '.ms ');
            s := SourcePath + '/' + Chapters.Current.Filename + '.co';
            if (FileExists (s)) then
              write (x, Chapters.Current.Filename + '.co ');
            write (x, Chapters.Current.Filename + '.so');
            s := SourcePath + '/' + Chapters.Current.Filename + '.tr';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.tr');
            s := SourcePath + '/' + Chapters.Current.Filename + '.an';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.an');
            s := SourcePath + '/' + Chapters.Current.Filename + '.om';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.om');
            s := SourcePath + '/disclaimer.so';
            if ((FileExists (s)) and
              (THTMLProfile (Profiles.Current).DisclaimerInChapter)) then
              write (x, ' disclaimer.so');
            writeln (x);
            writeln (x, #9, groffline, Chapters.Current.Filename + '-'
              + ProfileName + '.ms > "', OutputPath + '/' + StoryName + '/'
              + Chapters.Current.Filename + '.htm"');
            writeln (x);
          end;
        end;
      end;

      ptText : begin // Text
      	// Build the basic Groff call
        groffline := 'groff -Tascii -U -ms -s';
        if (Profiles.Current.Usepreconv) then
          groffline += 'k';
        if (tTextProfile (Profiles.Current).Usetbl) then
          groffline += 't';
        groffline += ' ';
        if (tTextProfile (Profiles.Current).BulkText) then begin
          write (x, OutputPath + '/' + StoryName + '.txt: $(CHAPTERS) ');
          if (DependList.Count > 0) then
            write (x, '$(OTHERFILES) ');
        	write (x, StoryProfileName + '.ms');
          if (FileExists (SourcePath + '/disclaimer.so')) then
            write (x, ' disclaimer.so');
          if (FileExists (SourcePath + '/blurb.so')) then
            write (x, ' blurb.so');
          if (FileExists (SourcePath + '/credits.so')) then
            write (x, ' credits.so');
          writeln (x);
          writeln (x, #9, 'rm -f ',StoryProfileName + '.ref');
          writeln (x, #9, groffline, StoryProfileName + '.ms > ',
        	  OutputPath + '/' + StoryName + '.txt');
          writeln (x, #9, groffline, StoryProfileName + '.ms > ',
        	  OutputPath + '/' + StoryName + '.txt');
          writeln (x, #9, groffline, StoryProfileName + '.ms > ',
        	  OutputPath + '/' + StoryName + '.txt');
          writeln (x);
        end else begin
          s := OutputPath + '/' + StoryName;
          if (not TTextProfile (Profiles.Current).BulkText) then
            if (not (DirectoryExists (s))) then begin
              {$I-}
              mkdir (s);
              if (IOResult = 0) then
                frmLog.txtLog.Lines.Add
                	('  -> Creating target directory ' + s + '.');
              {$I+}
            end;
          for chapnum := 0 to (Chapters.Count - 1) do begin
            Chapters.SelectAt (ChapNum);
            write (x, OutputPath + '/' + StoryName + '/'
              + Chapters.Current.Filename + '.txt: ');
            write (x, Chapters.Current.Filename + '-' + ProfileName + '.ms ');
            if (DependList.Count > 0) then
              write (x, '$(OTHERFILES) ');
            s := SourcePath + '/' + Chapters.Current.Filename + '.co';
            if (FileExists (s)) then
              write (x, Chapters.Current.Filename + '.co ');
            write (x, Chapters.Current.Filename + '.so');
            s := SourcePath + '/' + Chapters.Current.Filename + '.tr';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.tr');
            s := SourcePath + '/' + Chapters.Current.Filename + '.an';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.an');
            s := SourcePath + '/' + Chapters.Current.Filename + '.om';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.om');
            s := SourcePath + '/disclaimer.so';
//            if ((FileExists (s)) and
//              (tTextProfile (Profiles.Current).DisclaimerInTextChapter)) then
//              write (x, ' disclaimer.so');
            writeln (x);
            writeln (x, #9, groffline, Chapters.Current.Filename + '-' + ProfileName
              + '.ms > "', OutputPath + '/' + StoryName + '/' + Chapters.Current.Filename + '.txt"');
            writeln (x);
          end;
        end;

        end;

      ptEPub : begin // EPub
        // List all the EPub Chapters
        write (x, 'EPUBCHAP = ');
        for chapnum := 0 to (Chapters.Count - 1) do begin
          Chapters.SelectAt (chapnum);
          s := OutputPath + '/' + StoryName + '/OEBPS/Text/'
            + Chapters.Current.Filename + '.xhtml';
          write (x, #9, s);
          if (chapnum < (Chapters.Count - 1)) then
            writeln (x, ' \');
        end;
        writeln (x);
        writeln (x);

      // Build the basic Groff call
        groffline := 'groff -Txhtml -P-r -U -ms -s';
        if (Profiles.Current.Usepreconv) then
          groffline += 'k';
        groffline += ' ';

        writeln (x, OutputPath + '/' + aStory.LongName + '.epub: $(EPUBCHAP)');
        writeln (x, #9, 'cd ', OutputPath + '/' + aStory.LongName
          + ' && zip -Xr ../' + aStory.LongName
          + '.epub mimetype META-INF OEBPS');
        writeln (x);

        for chapnum := 0 to (Chapters.Count - 1) do begin
          Chapters.SelectAt (chapnum);
          write (x, OutputPath + '/' + StoryName + '/OEBPS/Text/' +
            Chapters.Current.Filename + '.xhtml: ');
          write (x, Chapters.Current.Filename + '-' + ProfileName + '.ms ');
          if (DependList.Count > 0) then
            write (x, '$(OTHERFILES) ');
        	write (x, StoryProfileName + '.ms');
          s := SourcePath + '/' + Chapters.Current.Filename + '.co';
          if (FileExists (s)) then
            write (x, Chapters.Current.Filename + '.co ');
          write (x, Chapters.Current.Filename + '.so');
          writeln (x);
          writeln (x, #9, groffline, Chapters.Current.Filename + '-'
            + ProfileName + '.ms > "', OutputPath + '/' + StoryName
            + '/OEBPS/Text/' + Chapters.Current.Filename + '.xhtml"');
          writeln (x);
        end;
      end;
    end;
  end;

  // Close out the Makefile
  close (x);

  // Erase temporary Variables
  Chapters.Free;
  Profiles.Free;
  DependList.Free;
end;

procedure WriteStoryMakeForVolume (aStory : tStory; aVolume : tVolume);
var
  Chapters : tChapterList;
  Profiles : tProfileList;
  hsize,
  vsize : real;
  x : text;
  index0,
  index1,
  ChapNum : integer;
  s,
  GroffLine,
  StoryName,
  ProfileName,
  StoryProfileName,
  SourcePath,
  SourceFileName,
  OutputPath : string;
begin
  // Load Chapters and Profiles
  Chapters := tChapterList.Create;
  Chapters.BaseDir:=aStory.SourceDir;
  Chapters.Load;

  Profiles := tProfileList.Create;
  Profiles.BaseDir:=aVolume.BaseDir;
  Profiles.Load;

  // open Makefile
  SourcePath := aStory.SourceDir + '/';
  frmLog.txtLog.Lines.Add (' -> Creating Makefile at ' + SourcePath);
  assign (x, SourcePath + 'Makefile');
  rewrite (x);

  // Write the 'all' rule
  writeln (x, 'all: \');
  for index0 := 0 to (Profiles.Count - 1) do begin
    Profiles.SelectAt (index0);
    OutputPath := Profiles.Current.OutputDir + '/';
    case (Profiles.Current.ProfileType) of
      ptPDF : begin
        write (x, #9, OutputPath + aStory.LongName + '.pdf');
      end;
      ptHTML : begin
        if (tHTMLProfile (Profiles.Current).BulkHTML) then
          write (x, #9, OutputPath + aStory.LongName + '.htm')
        else
          write (x, #9, OutputPath + aStory.LongName + '.htm');
          for index1 := 0 to (Chapters.Count - 1) do begin
            Chapters.SelectAt (index1);
            write (x, #9, OutputPath + aStory.LongName + '/'
              + Chapters.Current.Filename + '.htm');
            if (index1 < (Chapters.Count - 1)) then
              writeln (x, ' \');
          end;
      end;
      ptText : begin
        if (tTextProfile(Profiles.Current).BulkText) then
          write (x, #9, OutputPath + aStory.LongName + '.txt')
        else
          for index1 := 0 to (Chapters.Count - 1) do begin
            Chapters.SelectAt (index1);
            write (x, #9, OutputPath + Chapters.Current.Filename + '.txt');
            if (index1 < (Chapters.Count - 1)) then
              writeln (x, ' \');
          end;
      end;
      ptEPub : begin
        write (x, #9, OutputPath + aStory.LongName + '.epub');
      end;
    end;
    if (index0 < (Profiles.Count - 1)) then
      writeln (x, ' \');
  end;
  writeln (x);
  writeln (x);

  // Next, write chapter source files
  write (x, 'CHAPTERS = ');
  for index0 := 0 to (Chapters.Count - 1) do begin
    Chapters.SelectAt (index0);
  	SourceFileName := Chapters.Current.Filename;
    if (FileExists (SourcePath + '/' + SourceFileName + '.co')) then
      write (x, SourceFileName + '.co ');
    write (x, SourceFileName + '.so ');
    if (FileExists (SourcePath + '/' + SourceFileName + '.tr')) then
      write (x, SourceFileName + '.tr ');
    if (FileExists (SourcePath + '/' + SourceFileName + '.an')) then
      write (x, SourceFileName + '.an ');
    if (FileExists (SourcePath + '/' + SourceFileName + '.om')) then
      write (x, SourceFileName + '.om ');
{    if (DependCount > 0) then
      for index1 := 0 to (DependCount - 1) do
        if (DependList [index1].Chapter = index0) then begin
          writeln (x, ' \');
          write (x, #9, DependList [index1].Filename);
        end;
}    if (index0 < (Chapters.Count - 1)) then begin
      writeln (x, ' \');
      write (x, #9);
    end else begin
      writeln (x);
      writeln (x);
    end;
  end;

  // Now, write the groff build lines for each profile.
  for index1 := 0 to (Profiles.Count - 1) do begin
    Profiles.SelectAt (index1);
    StoryName := StripSpaces (aStory.LongName);
    StoryProfileName := StripSpaces (StoryName + '-' + Profiles.Current.Name);
    OutputPath := Profiles.Current.OutputDir;
    ProfileName := StripSpaces (Profiles.Current.Name);

    case (Profiles.Current.ProfileType) of
      ptPDF: begin // PDF
      	// Build the basic Groff call
        groffline := 'groff -Tps -U -ms -s';
        if (TPDFProfile (Profiles.Current).Useeqn) then
          groffline += 'e';
        if (Profiles.Current.Usepreconv) then
          groffline += 'k';
        if (TPDFProfile (Profiles.Current).Usetbl) then
          groffline += 't';
        groffline += ' -P';
        if (TPDFProfile (Profiles.Current).PageSize < PaperCount) then
          groffline += PaperSizes [TPDFProfile (Profiles.Current).PageSize, 2]
        else begin
          hsize := TPDFProfile (Profiles.Current).PageH / 72000;
          vsize := TPDFProfile (Profiles.Current).PageV / 72000;
          str (vsize:0:3, s);
          groffline += '-p' + s + 'i,';
          str (hsize:0:3, s);
          groffline += s + 'i'
        end;
        if (TPDFProfile (Profiles.Current).Landscape) then
          groffline += ' -P-l';
        groffline += ' ';

        writeln (x, OutputPath + '/' + StoryName + '.pdf: '
        	+ StoryProfileName + '.ps');
        writeln (x, #9, 'ps2pdf "' + StoryProfileName + '.ps" \');
        writeln (x, #9, '"' + OutputPath + '/' + StoryName + '.pdf"');
        writeln (x);
        write (x, StoryProfileName + '.ps: $(CHAPTERS) '
        	+ StoryProfileName + '.ms');
        if (FileExists (SourcePath + '/disclaimer.so')) then
          write (x, ' disclaimer.so');
        if (FileExists (SourcePath + '/blurb.so')) then
          write (x, ' blurb.so');
        if (FileExists (SourcePath + '/credits.so')) then
          write (x, ' credits.so');
{        if (DependCount > 0) then
          for dindex := 0 to (DependCount - 1) do
            if (DependList [dindex].Chapter = -1) then begin
              writeln (x, ' \');
              write (x, #9, DependList [dindex].Filename);
            end;
}
        writeln (x);
        writeln (x, #9, 'rm -f ',StoryProfileName + '.ref');
        writeln (x, #9, groffline, '-z ', StoryProfileName + '.ms > ',
        	StoryProfileName + '.ps');
        writeln (x, #9, groffline, '-z ', StoryProfileName + '.ms > ',
        	StoryProfileName + '.ps');
        writeln (x, #9, groffline, StoryProfileName + '.ms > ',
        	StoryProfileName + '.ps');
        writeln (x);
      end;

      ptHTML : begin // HTML
      	// Build the basic Groff call
        groffline := 'groff -Thtml -U -ms -s';
        if (Profiles.Current.Usepreconv) then
          groffline += 'k';
        groffline += ' ';

        s := OutputPath + '/' + StoryName;
        if (not THTMLProfile (Profiles.Current).BulkHTML) then
          if (not (DirectoryExists (s))) then begin
            {$I-}
            mkdir (s);
            if (IOResult = 0) then
              frmLog.txtLog.Lines.Add
                ('  -> Creating target directory ' + s + '.');
            {$I+}
          end;
        write (x, OutputPath + '/' + StoryName + '.htm: '
        	+ StoryProfileName + '.ms');
        if (THTMLProfile (Profiles.Current).BulkHTML) then
          write (x, ' $(CHAPTERS)');
        if (FileExists (SourcePath + '/disclaimer.so')) then
          write (x, ' disclaimer.so');
        if (FileExists (SourcePath + '/blurb.so')) then
          write (x, ' blurb.so');
        if (FileExists (SourcePath + '/credits.so')) then
          write (x, ' credits.so');
{        if (DependCount > 0) then
          for dindex := 0 to (DependCount - 1) do
            if (DependList [dindex].Chapter = -1) then begin
              writeln (x, ' \');
              write (x, #9, DependList [dindex].Filename);
            end;
}        writeln (x);
        writeln (x, #9, groffline, StoryProfileName + '.ms > "',
        	OutputPath + '/' + StoryName + '.htm"');
        writeln (x);

        if (not (THTMLProfile (Profiles.Current).BulkHTML)) then begin
          for ChapNum := 0 to (Chapters.Count - 1) do begin
            Chapters.SelectAt (ChapNum);
            write (x, OutputPath + '/' + StoryName + '/'
              + Chapters.Current.Filename + '.htm: ');
            write (x, Chapters.Current.Filename + '-' + ProfileName + '.ms ');
            s := SourcePath + '/' + Chapters.Current.Filename + '.co';
            if (FileExists (s)) then
              write (x, Chapters.Current.Filename + '.co ');
            write (x, Chapters.Current.Filename + '.so');
            s := SourcePath + '/' + Chapters.Current.Filename + '.tr';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.tr');
            s := SourcePath + '/' + Chapters.Current.Filename + '.an';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.an');
            s := SourcePath + '/' + Chapters.Current.Filename + '.om';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.om');
            s := SourcePath + '/disclaimer.so';
            if ((FileExists (s)) and
              (THTMLProfile (Profiles.Current).DisclaimerInChapter)) then
              write (x, ' disclaimer.so');
            writeln (x);
            writeln (x, #9, groffline, Chapters.Current.Filename + '-'
              + ProfileName + '.ms > "', OutputPath + '/' + StoryName + '/'
              + Chapters.Current.Filename + '.htm"');
            writeln (x);
          end;
        end;
      end;

      ptText : begin // Text
      	// Build the basic Groff call
        groffline := 'groff -Tascii -U -ms -s';
        if (Profiles.Current.Usepreconv) then
          groffline += 'k';
        if (tTextProfile (Profiles.Current).Usetbl) then
          groffline += 't';
        groffline += ' ';
        if (tTextProfile (Profiles.Current).BulkText) then begin
          write (x, OutputPath + '/' + StoryName + '.txt: $(CHAPTERS) '
        	  + StoryProfileName + '.ms');
          if (FileExists (SourcePath + '/disclaimer.so')) then
            write (x, ' disclaimer.so');
          if (FileExists (SourcePath + '/blurb.so')) then
            write (x, ' blurb.so');
          if (FileExists (SourcePath + '/credits.so')) then
            write (x, ' credits.so');
          writeln (x);
          writeln (x, #9, 'rm -f ',StoryProfileName + '.ref');
          writeln (x, #9, groffline, StoryProfileName + '.ms > ',
        	  OutputPath + '/' + StoryName + '.txt');
          writeln (x, #9, groffline, StoryProfileName + '.ms > ',
        	  OutputPath + '/' + StoryName + '.txt');
          writeln (x, #9, groffline, StoryProfileName + '.ms > ',
        	  OutputPath + '/' + StoryName + '.txt');
{          if (DependCount > 0) then
            for dindex := 0 to (DependCount - 1) do
              if (DependList [dindex].Chapter = -1) then begin
                writeln (x, ' \');
                write (x, #9, DependList [dindex].Filename);
              end;
}
          writeln (x);
        end else begin
          s := OutputPath + '/' + StoryName;
          if (not TTextProfile (Profiles.Current).BulkText) then
            if (not (DirectoryExists (s))) then begin
              {$I-}
              mkdir (s);
              if (IOResult = 0) then
                frmLog.txtLog.Lines.Add
                	('  -> Creating target directory ' + s + '.');
              {$I+}
            end;
          for chapnum := 0 to (Chapters.Count - 1) do begin
            Chapters.SelectAt (ChapNum);
            write (x, OutputPath + '/' + StoryName + '/'
              + Chapters.Current.Filename + '.txt: ');
            write (x, Chapters.Current.Filename + '-' + ProfileName + '.ms ');
            s := SourcePath + '/' + Chapters.Current.Filename + '.co';
            if (FileExists (s)) then
              write (x, Chapters.Current.Filename + '.co ');
            write (x, Chapters.Current.Filename + '.so');
            s := SourcePath + '/' + Chapters.Current.Filename + '.tr';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.tr');
            s := SourcePath + '/' + Chapters.Current.Filename + '.an';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.an');
            s := SourcePath + '/' + Chapters.Current.Filename + '.om';
            if (FileExists (s)) then
              write (x, ' ', Chapters.Current.Filename + '.om');
            s := SourcePath + '/disclaimer.so';
//            if ((FileExists (s)) and
//              (tTextProfile (Profiles.Current).DisclaimerInTextChapter)) then
//              write (x, ' disclaimer.so');
            writeln (x);
            writeln (x, #9, groffline, Chapters.Current.Filename + '-' + ProfileName
              + '.ms > "', OutputPath + '/' + StoryName + '/' + Chapters.Current.Filename + '.txt"');
            writeln (x);
          end;
        end;

        end;

      ptEPub : begin // EPub
        // List all the EPub Chapters
        write (x, 'EPUBCHAP = ');
        for chapnum := 0 to (Chapters.Count - 1) do begin
          Chapters.SelectAt (chapnum);
          s := OutputPath + '/' + StoryName + '/OEBPS/Text/'
            + Chapters.Current.Filename + '.xhtml';
          write (x, #9, s);
          if (chapnum < (Chapters.Count - 1)) then
            writeln (x, ' \');
        end;
        writeln (x);
        writeln (x);

      // Build the basic Groff call
        groffline := 'groff -Txhtml -P-r -U -ms -s';
        if (Profiles.Current.Usepreconv) then
          groffline += 'k';
        groffline += ' ';

        writeln (x, OutputPath + '/' + aStory.LongName + '.epub: $(EPUBCHAP)');
{        if (DependCount > 0) then
          for dindex := 0 to (DependCount - 1) do
            if (DependList [dindex].Chapter = -1) then begin
              writeln (x, ' \');
              write (x, #9, DependList [dindex].Filename);
            end;
}
        writeln (x, #9, 'cd ', OutputPath + '/' + aStory.LongName
          + ' && zip -Xr ../' + aStory.LongName
          + '.epub mimetype META-INF OEBPS');
        writeln (x);

        for chapnum := 0 to (Chapters.Count - 1) do begin
          Chapters.SelectAt (chapnum);
          write (x, OutputPath + '/' + StoryName + '/OEBPS/Text/' +
            Chapters.Current.Filename + '.xhtml: ');
          write (x, Chapters.Current.Filename + '-' + ProfileName + '.ms ');
          s := SourcePath + '/' + Chapters.Current.Filename + '.co';
          if (FileExists (s)) then
            write (x, Chapters.Current.Filename + '.co ');
          write (x, Chapters.Current.Filename + '.so');
          writeln (x);
          writeln (x, #9, groffline, Chapters.Current.Filename + '-'
            + ProfileName + '.ms > "', OutputPath + '/' + StoryName
            + '/OEBPS/Text/' + Chapters.Current.Filename + '.xhtml"');
          writeln (x);
        end;
      end;
    end;
  end;

  // Close out the Makefile
  close (x);

  // Erase temporary Variables
  Chapters.Free;
  Profiles.Free;
end;

procedure WriteVolumeMake (aVolume : tVolume);
var
  t : text;
  index : integer;
  s :  string;
  StoryList : tStoryList;
begin
	assign (t, aVolume.BaseDir + '/Makefile');
  rewrite (t);
  s := 'Creating Volume-level Makefile.';
  frmLog.txtLog.Lines.Add (s);

  StoryList := tStoryList.Create;
  StoryList.BaseDir := aVolume.BaseDir;
  StoryList.LoadStoryList;

  // Write the 'all' rule
  write (t, 'all:');
  for index := 0 to (StoryList.Count - 1) do begin
    StoryList.SelectAt (index);
  	write (t, #9, StoryList.Current.LongName);
    if (index < (StoryList.Count - 1)) then
    	write (t, ' \');
    writeln (t);
  end;
  writeln (t);

  // Next, write the make lines for each story;
  for index := 0 to (StoryList.Count - 1) do begin
    StoryList.SelectAt (index);
    writeln (t, StoryList.Current.LongName, ': FORCE');
    writeln (t, #9, 'cd ', StoryList.Current.SourceDir, ' && make');
    writeln (t);
  end;

  writeln (t, 'FORCE:');
  close (t);
end;

procedure Make (TargetDir : string);
var
  MemStream : TStringStream;
  NumBytes: LongInt;
  Buffer : pointer;
  StoryMake : tProcess;
const
  READ_BYTES = 2048;
begin
  // Gonna be a metric fuckton of comments in here, because this trick is new
  // to me.
  MemStream := TStringStream.Create('');
  frmLog.txtLog.Lines.Add ('');
  frmLog.txtLog.Lines.Add (' -> Running make in ' + TargetDir + '...');
  frmLog.txtLog.Lines.Add ('');

  StoryMake := tProcess.Create (nil); // Instantiate the process
  StoryMake.Executable := '/usr/bin/make'; // Hard-coded for now,
  // I will change it later to use a parameter
  StoryMake.CurrentDirectory := TargetDir; // Target Directory
  StoryMake.Options := [poUsePipes];
  StoryMake.Execute; // This should not hang the program until complete

  Getmem(Buffer, READ_BYTES);
  while StoryMake.Running do begin
    // try reading it
    NumBytes := StoryMake.Output.Read(Buffer^, READ_BYTES);
    if (NumBytes > 0) then begin
      MemStream.size := 0;
      MemStream.Write(Buffer^, NumBytes);
      // And add the raw stringdata to the memo
      frmLog.txtLog.Lines.Text := frmLog.txtLog.Lines.Text
        + (MemStream.DataString);
    end else
      Sleep(100); // no data, wait 100 ms
    Application.ProcessMessages;
  end;

  // Gather the remainder of the output
  repeat
    NumBytes := StoryMake.Output.Read(Buffer^, READ_BYTES);
    if NumBytes > 0 then begin
      MemStream.size := 0;
      MemStream.Write(Buffer^, NumBytes);
      // And add the raw stringdata to the memo
      frmLog.txtLog.Lines.Text := frmLog.txtLog.Lines.Text
        + (MemStream.DataString);
    end;
    Application.ProcessMessages;
  until NumBytes <= 0;

  MemStream.Free;
  StoryMake.Free;
end;

end.

