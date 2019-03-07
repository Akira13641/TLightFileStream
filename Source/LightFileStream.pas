{@abstract(Implements a lightweight, high-performance, non-allocating advanced-record-based wrapper
           around the SysUtils file handling routines as an alternative to Classes.TFileStream.)}

unit LightFileStream;

{$mode Delphi}{$H+}{$J-}{$I-}

//Uncomment NoChecks when building production releases of thoroughly tested software.
{.$define NoChecks}

interface

uses SysUtils;

type
  //Type alias for a pointer to the TLightFileStream type.
  PLightFileStream = ^TLightFileStream;

  //@abstract(
  //The sole type implemented by this unit so far.
  //
  //Returns a pointer to itself from all functions to avoid unnecessary allocation or copying
  //and to allow for a convenient "method chaining" API.)
  TLightFileStream = record
  public type
    //Determines whether read or write operations are actively accepted.
    //
    //All functions return immediately without doing anything if the current state is "wrong" relative to them,
    //unless @code({$define NoChecks}) is set, which improves performance but is somewhat less "safe."
    TFileState = (fsReading, fsWriting);
  strict private
    //The name of the underlying file on disk.
    FFileName: PChar;
    //The handle of the underlying file on disk.
    FHandle: THandle;
    //Indicates whether the file is @noAutoLink(open) and can be accessed at all.
    //
    //All functions return immediately without doing anything in the event that this is false,
    //unless @code({$define NoChecks}) is set, which improves performance but is somewhat less "safe."
    FOpen: Boolean;
    //A private instance of TFileState, used as described above.
    FState: TFileState;
  public
    //Assumes @code(FileName) does not yet exist on disk. Calls @code(FileCreate()) internally.
    //FState is then set to fsWriting, and FOpen is set to True.
    class function Create(const FileName: PChar): TLightFileStream; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes @code(FileName) already exists on disk. Calls @code(FileOpen()) with either @code(fmOpenRead) or @code(fmOpenWrite) internally based on the provided @code(InitialState) value.
    //
    //FState is then set to @code(InitialState), and FOpen is set to True.
    class function Open(const FileName: PChar; const InitialState: TFileState): TLightFileStream; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Closes the underlying file. Does not return a self-pointer, as it should always be the last method called.
    procedure Close; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Calls @code(FileClose()) on FHandle, then calls @code(FileOpen()) with either @code(fmOpenRead) or @code(fmOpenWrite)
    //internally based on the provides @code(State) value. FState is then set to @code(State).
    function ChangeFileStateTo(const State: TFileState): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Sets the current @noAutoLink(position) of the underlying file to @code(ToPosition), relative to 0.
    function SeekFromBeginning(const ToPosition: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Sets the current @noAutoLink(position) of the underlying file to @code(ToPosition), relative to itself.
    function SeekFromCurrent(const ToPosition: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Sets the current @noAutoLink(position) of the underlying file to @code(ToPosition), relative to the end of the file.
    function SeekFromEnd(const ToPosition: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Returns the @noAutoLink(size) in bytes of the underlying file in @code(TheSize), and a self-pointer from the function itself.
    function GetSize(out TheSize: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Calls GetSize() internally and writes the value to the command line.
    function LogSize: PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Returns the current @noAutoLink(position) of the underlying file in @code(ThePosition), and a self-pointer from the function itself.
    function GetPosition(out ThePosition: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Calls GetPosition() internally and writes the value to the command line.
    function LogPosition: PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Truncates the underlying file at the current position.
    function Truncate: PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of T into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadType<T>(var Item: T): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of T from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteType<T>(constref Item: T): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of T from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendType<T>(constref Item: T): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes @code(Buffer) is something like the first value of an array of T. Reads @code(ItemCount) items into it from the underlying file at the current @noAutoLink(position).
    function ReadTypedBuffer<T>(var Buffer: T; const ItemCount: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes @code(Buffer) is something like the first value of an array of T. Writes @code(ItemCount) items from it to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteTypedBuffer<T>(constref Buffer: T; const ItemCount: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes @code(Buffer) is something like the first value of an array of T. Writes @code(ItemCount) items from it to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendTypedBuffer<T>(constref Buffer: T; const ItemCount: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes @code(Buffer) is pointing to something like the first value of an array of any type. Reads @code(NumBytesToRead) bytes into it from the underlying file at the current @noAutoLink(position).
    function ReadPointerBuffer(const Buffer: Pointer; const NumBytesToRead: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes @code(Buffer) is pointing to something like the first value of an array of any type.
    //
    //Writes @code(NumBytesToWrite) bytes from it to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WritePointerBuffer(const Buffer: Pointer; const NumBytesToWrite: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes @code(Buffer) is pointing to something like the first value of an array of any type. Writes @code(NumBytesToWrite) bytes from it to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendPointerBuffer(const Buffer: Pointer; const NumBytesToWrite: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Byte into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadByte(var Item: Byte): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Byte from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteByte(const Item: Byte): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Byte from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendByte(const Item: Byte): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Word into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadWord(var Item: Word): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Word from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteWord(const Item: Word): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Word from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendWord(const Item: Word): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of LongWord into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadLongWord(var Item: LongWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of LongWord from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteLongWord(const Item: LongWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of LongWord from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendLongWord(const Item: LongWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of QWord into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadQWord(var Item: QWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of QWord from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteQWord(const Item: QWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of QWord from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendQWord(const Item: QWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of ShortInt into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadShortInt(var Item: ShortInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of ShortInt from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteShortInt(const Item: ShortInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of ShortInt from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendShortInt(const Item: ShortInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of SmallInt into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadSmallInt(var Item: SmallInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of SmallInt from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteSmallInt(const Item: SmallInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of SmallInt from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendSmallInt(const Item: SmallInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of LongInt into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadLongInt(var Item: LongInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of LongInt from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteLongInt(const Item: LongInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of LongInt from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendLongInt(const Item: LongInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Int64 into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadInt64(var Item: Int64): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Int64 from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteInt64(const Item: Int64): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Int64 from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendInt64(const Item: Int64): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Single into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadSingle(var Item: Single): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Single from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteSingle(const Item: Single): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Single from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendSingle(const Item: Single): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Double into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadDouble(var Item: Double): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Double from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteDouble(const Item: Double): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Double from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendDouble(const Item: Double): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Currency into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadCurrency(var Item: Currency): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Currency from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteCurrency(const Item: Currency): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Currency from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendCurrency(const Item: Currency): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of TDateTime into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadDateTime(var Item: TDateTime): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of TDateTime from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteDateTime(const Item: TDateTime): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of TDateTime from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendDateTime(const Item: TDateTime): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of AnsiChar into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadAnsiChar(var Item: AnsiChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of AnsiChar from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteAnsiChar(const Item: AnsiChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of AnsiChar from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendAnsiChar(const Item: AnsiChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of WideChar into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadWideChar(var Item: WideChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of WideChar from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteWideChar(const Item: WideChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of WideChar from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendWideChar(const Item: WideChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of UnicodeChar into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadUnicodeChar(var Item: UnicodeChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of UnicodeChar from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteUnicodeChar(const Item: UnicodeChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of UnicodeChar from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendUnicodeChar(const Item: UnicodeChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads @code(NumChars) worth of AnsiChars into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadShortString(var Item: ShortString; const NumChars: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of ShortString from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteShortString(const Item: ShortString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of ShortString from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendShortString(const Item: ShortString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads @code(NumChars) worth of AnsiChars into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadAnsiString(var Item: AnsiString; const NumChars: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of AnsiString from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteAnsiString(const Item: AnsiString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of AnsiString from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendAnsiString(const Item: AnsiString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads @code(NumChars) worth of WideChars into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadWideString(var Item: WideString; const NumChars: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of WideString from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteWideString(const Item: WideString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of WideString from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendWideString(const Item: WideString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads @code(NumChars) worth of UnicodeChars into @code(Item) from the underlying file at the current @noAutoLink(position).
    function ReadUnicodeString(var Item: UnicodeString; const NumChars: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of UnicodeString from @code(Item) to the underlying file at the current @noAutoLink(position), possibly overwriting existing data.
    function WriteUnicodeString(const Item: UnicodeString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of UnicodeString from @code(Item) to the underlying file at the last @noAutoLink(position), behind any existing data.
    function AppendUnicodeString(const Item: UnicodeString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

implementation

class function TLightFileStream.Create(const FileName: PChar): TLightFileStream;
begin
  with Result do begin
    FFileName := FileName;
    FHandle := FileCreate(FFileName);
    FOpen := (FHandle <> THandle(-1));
    FState := fsWriting;
  end;
end;

class function TLightFileStream.Open(const FileName: PChar; const InitialState: TFileState): TLightFileStream;
begin
  with Result do begin
    FFileName := FileName;
    case InitialState of
      fsReading: FHandle := FileOpen(FFileName, fmOpenRead);
      fsWriting: FHandle := FileOpen(FFileName, fmOpenWrite);
    end;
    FOpen := (FHandle <> THandle(-1));
    FState := InitialState;
  end;
end;

procedure TLightFileStream.Close;
begin
  {$IFNDEF NOCHECKS}
  if not FOpen then Exit();
  {$ENDIF}
  FileClose(FHandle);
end;

function TLightFileStream.ChangeFileStateTo(const State: TFileState): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (State = FState) then Exit(@Self);
  {$ENDIF}
  FileClose(FHandle);
  case State of
    fsReading: FHandle := FileOpen(FFileName, fmOpenRead);
    fsWriting: FHandle := FileOpen(FFileName, fmOpenWrite);
  end;
  FOpen := (FHandle <> THandle(-1));
  FState := State;
  Result := @Self;
end;

function TLightFileStream.SeekFromBeginning(const ToPosition: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if not FOpen then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, ToPosition, fsFromBeginning);
  Result := @Self;
end;

function TLightFileStream.SeekFromCurrent(const ToPosition: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if not FOpen then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, ToPosition, fsFromCurrent);
  Result := @Self;
end;

function TLightFileStream.SeekFromEnd(const ToPosition: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if not FOpen then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, ToPosition, fsFromEnd);
  Result := @Self;
end;

function TLightFileStream.GetSize(out TheSize: SizeInt): PLightFileStream;
var CurrentPosition: SizeInt;
begin
  {$IFNDEF NOCHECKS}
  if not FOpen then Exit(@Self);
  {$ENDIF}
  CurrentPosition := FileSeek(FHandle, 0, fsFromCurrent);
  TheSize := FileSeek(FHandle, 0, fsFromEnd);
  FileSeek(FHandle, CurrentPosition, fsFromBeginning);
  Result := @Self;
end;

function TLightFileStream.LogSize: PLightFileStream;
var CurrentSize: SizeInt = 0;
begin
  GetSize(CurrentSize);
  WriteLn('Size of TLightFileStream with handle ', FHandle, ': ', CurrentSize);
  Result := @Self;
end;

function TLightFileStream.GetPosition(out ThePosition: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if not FOpen then Exit(@Self);
  {$ENDIF}
  ThePosition := FileSeek(FHandle, 0, fsFromCurrent);
  Result := @Self;
end;

function TLightFileStream.LogPosition: PLightFileStream;
var CurrentPosition: SizeInt = 0;
begin
  GetPosition(CurrentPosition);
  WriteLn('Position of TLightFileStream with handle ', FHandle, ': ', CurrentPosition);
  Result := @Self;
end;

function TLightFileStream.Truncate: PLightFileStream;
var CurrentSize: SizeInt;
begin
  {$IFNDEF NOCHECKS}
  if not FOpen then Exit(@Self);
  {$ENDIF}
  GetSize(CurrentSize);
  FileTruncate(FHandle, CurrentSize - (CurrentSize - FileSeek(FHandle, 0, fsFromCurrent)));
  Result := @Self;
end;

function TLightFileStream.ReadType<T>(var Item: T): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(T));
  Result := @Self;
end;

function TLightFileStream.WriteType<T>(constref Item: T): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(T));
  Result := @Self;
end;

function TLightFileStream.AppendType<T>(constref Item: T): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(T));
  Result := @Self;
end;

function TLightFileStream.ReadTypedBuffer<T>(var Buffer: T; const ItemCount: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Buffer, SizeOf(T) * ItemCount);
  Result := @Self;
end;

function TLightFileStream.WriteTypedBuffer<T>(constref Buffer: T; const ItemCount: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Buffer, SizeOf(T) * ItemCount);
  Result := @Self;
end;

function TLightFileStream.AppendTypedBuffer<T>(constref Buffer: T; const ItemCount: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Buffer, SizeOf(T) * ItemCount);
  Result := @Self;
end;

function TLightFileStream.ReadPointerBuffer(const Buffer: Pointer; const NumBytesToRead: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Buffer^, NumBytesToRead);
  Result := @Self;
end;

function TLightFileStream.WritePointerBuffer(const Buffer: Pointer; const NumBytesToWrite: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Buffer^, NumBytesToWrite);
  Result := @Self;
end;

function TLightFileStream.AppendPointerBuffer(const Buffer: Pointer; const NumBytesToWrite: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Buffer^, NumBytesToWrite);
  Result := @Self;
end;

function TLightFileStream.ReadByte(var Item: Byte): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(Byte));
  Result := @Self;
end;

function TLightFileStream.WriteByte(const Item: Byte): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(Byte));
  Result := @Self;
end;

function TLightFileStream.AppendByte(const Item: Byte): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(Byte));
  Result := @Self;
end;

function TLightFileStream.ReadWord(var Item: Word): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(Word));
  Result := @Self;
end;

function TLightFileStream.WriteWord(const Item: Word): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(Word));
  Result := @Self;
end;

function TLightFileStream.AppendWord(const Item: Word): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(Word));
  Result := @Self;
end;

function TLightFileStream.ReadLongWord(var Item: LongWord): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(LongWord));
  Result := @Self;
end;

function TLightFileStream.WriteLongWord(const Item: LongWord): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(LongWord));
  Result := @Self;
end;

function TLightFileStream.AppendLongWord(const Item: LongWord): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(LongWord));
  Result := @Self;
end;

function TLightFileStream.ReadQWord(var Item: QWord): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(QWord));
  Result := @Self;
end;

function TLightFileStream.WriteQWord(const Item: QWord): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(QWord));
  Result := @Self;
end;

function TLightFileStream.AppendQWord(const Item: QWord): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(QWord));
  Result := @Self;
end;

function TLightFileStream.ReadShortInt(var Item: ShortInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(ShortInt));
  Result := @Self;
end;

function TLightFileStream.WriteShortInt(const Item: ShortInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(ShortInt));
  Result := @Self;
end;

function TLightFileStream.AppendShortInt(const Item: ShortInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(ShortInt));
  Result := @Self;
end;

function TLightFileStream.ReadSmallInt(var Item: SmallInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(SmallInt));
  Result := @Self;
end;

function TLightFileStream.WriteSmallInt(const Item: SmallInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(SmallInt));
  Result := @Self;
end;

function TLightFileStream.AppendSmallInt(const Item: SmallInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(SmallInt));
  Result := @Self;
end;

function TLightFileStream.ReadLongInt(var Item: LongInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(LongInt));
  Result := @Self;
end;

function TLightFileStream.WriteLongInt(const Item: LongInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(LongInt));
  Result := @Self;
end;

function TLightFileStream.AppendLongInt(const Item: LongInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(LongInt));
  Result := @Self;
end;

function TLightFileStream.ReadInt64(var Item: Int64): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(Int64));
  Result := @Self;
end;

function TLightFileStream.WriteInt64(const Item: Int64): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(Int64));
  Result := @Self;
end;

function TLightFileStream.AppendInt64(const Item: Int64): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(Int64));
  Result := @Self;
end;

function TLightFileStream.ReadSingle(var Item: Single): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(Single));
  Result := @Self;
end;

function TLightFileStream.WriteSingle(const Item: Single): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(Single));
  Result := @Self;
end;

function TLightFileStream.AppendSingle(const Item: Single): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(Single));
  Result := @Self;
end;

function TLightFileStream.ReadDouble(var Item: Double): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(Double));
  Result := @Self;
end;

function TLightFileStream.WriteDouble(const Item: Double): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(Double));
  Result := @Self;
end;

function TLightFileStream.AppendDouble(const Item: Double): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(Double));
  Result := @Self;
end;

function TLightFileStream.ReadCurrency(var Item: Currency): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(Currency));
  Result := @Self;
end;

function TLightFileStream.WriteCurrency(const Item: Currency): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(Currency));
  Result := @Self;
end;

function TLightFileStream.AppendCurrency(const Item: Currency): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(Currency));
  Result := @Self;
end;

function TLightFileStream.ReadDateTime(var Item: TDateTime): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(TDateTime));
  Result := @Self;
end;

function TLightFileStream.WriteDateTime(const Item: TDateTime): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(TDateTime));
  Result := @Self;
end;

function TLightFileStream.AppendDateTime(const Item: TDateTime): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(TDateTime));
  Result := @Self;
end;

function TLightFileStream.ReadAnsiChar(var Item: AnsiChar): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(AnsiChar));
  Result := @Self;
end;

function TLightFileStream.WriteAnsiChar(const Item: AnsiChar): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(AnsiChar));
  Result := @Self;
end;

function TLightFileStream.AppendAnsiChar(const Item: AnsiChar): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(AnsiChar));
  Result := @Self;
end;

function TLightFileStream.ReadWideChar(var Item: WideChar): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(WideChar));
  Result := @Self;
end;

function TLightFileStream.WriteWideChar(const Item: WideChar): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(WideChar));
  Result := @Self;
end;

function TLightFileStream.AppendWideChar(const Item: WideChar): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(WideChar));
  Result := @Self;
end;

function TLightFileStream.ReadUnicodeChar(var Item: UnicodeChar): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item, SizeOf(UnicodeChar));
  Result := @Self;
end;

function TLightFileStream.WriteUnicodeChar(const Item: UnicodeChar): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item, SizeOf(UnicodeChar));
  Result := @Self;
end;

function TLightFileStream.AppendUnicodeChar(const Item: UnicodeChar): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item, SizeOf(UnicodeChar));
  Result := @Self;
end;

function TLightFileStream.ReadShortString(var Item: ShortString; const NumChars: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item[1], NumChars);
  Result := @Self;
end;

function TLightFileStream.WriteShortString(const Item: ShortString): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item[1], Length(Item));
  Result := @Self;
end;

function TLightFileStream.AppendShortString(const Item: ShortString): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item[1], Length(Item));
  Result := @Self;
end;

function TLightFileStream.ReadAnsiString(var Item: AnsiString; const NumChars: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item[1], NumChars);
  Result := @Self;
end;

function TLightFileStream.WriteAnsiString(const Item: AnsiString): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item[1], Length(Item));
  Result := @Self;
end;

function TLightFileStream.AppendAnsiString(const Item: AnsiString): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item[1], Length(Item));
  Result := @Self;
end;

function TLightFileStream.ReadWideString(var Item: WideString; const NumChars: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item[1], SizeOf(WideChar) * NumChars);
  Result := @Self;
end;

function TLightFileStream.WriteWideString(const Item: WideString): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item[1], TEncoding.Unicode.GetByteCount(Item));
  Result := @Self;
end;

function TLightFileStream.AppendWideString(const Item: WideString): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item[1], TEncoding.Unicode.GetByteCount(Item));
  Result := @Self;
end;

function TLightFileStream.ReadUnicodeString(var Item: UnicodeString; const NumChars: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsReading) then Exit(@Self);
  {$ENDIF}
  FileRead(FHandle, Item[1], SizeOf(UnicodeChar) * NumChars);
  Result := @Self;
end;

function TLightFileStream.WriteUnicodeString(const Item: UnicodeString): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Item[1], TEncoding.Unicode.GetByteCount(Item));
  Result := @Self;
end;

function TLightFileStream.AppendUnicodeString(const Item: UnicodeString): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Item[1], TEncoding.Unicode.GetByteCount(Item));
  Result := @Self;
end;

end.
