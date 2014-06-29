unit gothers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  tOtherFile = class (tObject)
    private
      t_filename : string;
    public
      property Filename : string read t_filename write t_filename;
      procedure Load  (var s : string);
      procedure Save (var t : text);
  end;

  tOtherFileList = class (tObject)
    private
      t_dirty : boolean;
      t_files : array of tOtherFile;
      t_current_file : tOtherFile;
      t_basedir : string;
      function GetFileCount : integer;
    public
      property Current : tOtherFile read t_current_file;
      property Count : integer read GetFileCount;
      property Dirty : boolean read t_dirty;
      property BaseDir : string read t_basedir write t_basedir;
      procedure New (aFileName : string);
      procedure Delete;
      procedure Select (aFileName : string);
      procedure SelectAt (aFileIndex : integer);
      procedure Load;
      procedure Save;
      function Name (index : integer) : string;
      procedure MarkDirty;
  end;

implementation

uses
  gtools;

{$region tOtherFile}

procedure tOtherFile.Load (var s : string);
var
  k,
  v : string;
  i : integer;
begin
  // Find the = sign
  i := 0;
  repeat
    inc (i);
  until ((copy (s, i, 1) = '=') or (i > length (s)));

  // Split Keyline
  k := copy (s, 1, i - 1);
  v := copy (s, i + 1, length (s) - i);
  if (length (k) > 0) then
  	Trim (k);
  if (length (v) > 0) then
	  Trim (v);

  // Select Variable
  if (k = 'Filename') then
    Filename := v;
end;

procedure tOtherFile.Save (var t : text);
var
  index : integer;
begin
  writeln (t, 'Filename = ', Filename);
end;

{$endregion}

{$region tOtherFileList}

function tOtherFileList.GetFileCount : integer;
begin
	GetFileCount := length (t_files);
end;

procedure tOtherFileList.New (aFileName : string);
var
  index : integer;
begin
	index := length (t_files);
  SetLength (t_files, index + 1);
  t_files [index] := tOtherFile.Create;
  t_current_file := t_files [index];
  t_current_file.Filename := aFileName;
  MarkDirty;
end;

procedure tOtherFileList.Delete;
var
  index,
  last : integer;
begin
  last := length (t_files) - 1;
  index := 0;
  while ((t_files [index] <> t_current_file) and (index < last)) do
    inc (index);
  t_files [index].Destroy;
  if (index < last) then
    t_files [index] := t_files [last];
  SetLength (t_files, last);
  MarkDirty;
end;

procedure tOtherFileList.Select (aFileName : string);
var
  found : boolean;
  index : integer;
begin
	// Simple linear search
  index := 0;
  found := false;
  while ((index < length (t_files)) and not found) do begin
    if (aFileName = t_files [index].Filename) then
      found := true
    else
    	index += 1;
  end;
  if found then
    t_current_file := t_files [index];
end;

procedure tOtherFileList.Load;
var
  filename,
	s : string;
  t : text;
  index : integer;
begin
  // Verify that the directory exists, and create it if it doesn't
	filename := t_basedir;
  if not (DirectoryExists(filename)) then
    CreateDir (filename);
  filename += '/depends.fic';

  // Verify that depends.fic exists
  if (FileExists (filename)) then begin
    // Open it, and start scanning
    assign (t, filename);
    reset (t);

    repeat
      readln (t, s);
      if (length (s) > 0) then
        Trim (s);

      index := length (t_files);
      SetLength (t_files, index + 1);
      t_files [index] := tOtherFile.Create;
      t_files [index].Load (s); // Dependency lists are far less complex than
          // other file types, and as such don't need as complex a load method
    until (eof (t));

    close (t);
  end;
end;

procedure tOtherFileList.Save;
var
  t : text;
  filename : string;
  index : integer;
begin
  if Dirty then begin
    // Verify that the target directory exists
  	filename := t_basedir;
    if (not (DirectoryExists (filename))) then
      CreateDir (filename);

    // Open the stories file
    filename += '/depends.fic';
    assign (t, filename);
    rewrite (t);

    // Go through all the stories, and save them
    for index := 0 to (length (t_files) - 1) do
      t_files [index].Save (t);

    // Close out the list
    close (t);
    t_dirty := FALSE;
  end;
end;

procedure tOtherFileList.SelectAt (aFileIndex : integer);
begin
  t_current_file := t_files [aFileIndex];
end;

function tOtherFileList.Name (index : integer) : string;
begin
	Name := t_files [index].Filename;
end;

procedure tOtherFileList.MarkDirty;
begin
	t_dirty := true;
end;

{$endregion}


end.

