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
  //Returns a pointer to itself from all functions to avoid unnecessary allocation or copying
  //and to allow for a convenient "method chaining" API.)
  TLightFileStream = record
  public type
    //Determines whether read or write operations are actively accepted.
    //All functions return immediately without doing anything if the current state is "wrong" relative to them,
    //unless @code({$define NoChecks}) is set, which improves performance but is somewhat less "safe."
    TFileState = (fsReading, fsWriting);
  strict private
    //The name of the underlying file on disk.
    FFileName: PChar;
    //The handle of the underlying file on disk.
    FHandle: THandle;
    //Indicates whether the file is open and can be accessed at all.
    //All functions return immediately without doing anything in the event that this is false,
    //unless @code({$define NoChecks}) is set, which improves performance but is somewhat less "safe."
    FOpen: Boolean;
    //A private instance of TFileState, used as described above.
    FState: TFileState;
  public
    //Assumes FileName does not yet exist on disk. Calls @code(FileCreate()) internally. FState is then set to fsWriting, and FOpen is set to True.
    class function Create(const FileName: PChar): TLightFileStream; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes FileName already exists on disk. Calls @code(FileOpen()) with either @code(fmOpenRead) or @code(fmOpenWrite) internally based on the provided InitialState value.
    //FState is then set to InitialState, and FOpen is set to True.
    class function Open(const FileName: PChar; const InitialState: TFileState): TLightFileStream; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Calls @code(FileClose()) on FHandle, then calls @code(FileOpen()) with either @code(fmOpenRead) or @code(fmOpenWrite)
    //internally based on the provided State value. FState is then set to State.
    function ChangeFileStateTo(const State: TFileState): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of T into Item from the underlying file at the current position.
    function ReadType<T>(var Item: T): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of T from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteType<T>(constref Item: T): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of T from Item to the underlying file at the last position, behind any existing data.
    function AppendType<T>(constref Item: T): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes Buffer is something like the first value of an array of T. Reads ItemCount items into it from the underlying file at the current position.
    function ReadTypedBuffer<T>(var Buffer: T; const ItemCount: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes Buffer is something like the first value of an array of T. Writes ItemCount items from it to the underlying file at the current position, possibly overwriting existing data.
    function WriteTypedBuffer<T>(constref Buffer: T; const ItemCount: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes Buffer is something like the first value of an array of T. Writes ItemCount items from it to the underlying file at the last position, behind any existing data.
    function AppendTypedBuffer<T>(constref Buffer: T; const ItemCount: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes Buffer is pointing to something like the first value of an array of any type. Reads NumBytesToRead bytes into it from the underlying file at the current position.
    function ReadPointerBuffer(const Buffer: Pointer; const NumBytesToRead: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes Buffer is pointing to something like the first value of an array of any type. Writes NumBytesToRead bytes from it to the underlying file at the current position, possibly overwriting existing data.
    function WritePointerBuffer(const Buffer: Pointer; const NumBytesToRead: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Assumes Buffer is pointing to something like the first value of an array of any type. Writes NumBytesToRead bytes from it to the underlying file at the last position, behind any existing data.
    function AppendPointerBuffer(const Buffer: Pointer; const NumBytesToRead: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Byte into Item from the underlying file at the current position.
    function ReadByte(var Item: Byte): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Byte from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteByte(const Item: Byte): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Byte from Item to the underlying file at the last position, behind any existing data.
    function AppendByte(const Item: Byte): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Word into Item from the underlying file at the current position.
    function ReadWord(var Item: Word): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Word from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteWord(const Item: Word): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Word from Item to the underlying file at the last position, behind any existing data.
    function AppendWord(const Item: Word): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of LongWord into Item from the underlying file at the current position.
    function ReadLongWord(var Item: LongWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of LongWord from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteLongWord(const Item: LongWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of LongWord from Item to the underlying file at the last position, behind any existing data.
    function AppendLongWord(const Item: LongWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of QWord into Item from the underlying file at the current position.
    function ReadQWord(var Item: QWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of QWord from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteQWord(const Item: QWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of QWord from Item to the underlying file at the last position, behind any existing data.
    function AppendQWord(const Item: QWord): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of ShortInt into Item from the underlying file at the current position.
    function ReadShortInt(var Item: ShortInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of ShortInt from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteShortInt(const Item: ShortInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of ShortInt from Item to the underlying file at the last position, behind any existing data.
    function AppendShortInt(const Item: ShortInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of SmallInt into Item from the underlying file at the current position.
    function ReadSmallInt(var Item: SmallInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of SmallInt from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteSmallInt(const Item: SmallInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of SmallInt from Item to the underlying file at the last position, behind any existing data.
    function AppendSmallInt(const Item: SmallInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of LongInt into Item from the underlying file at the current position.
    function ReadLongInt(var Item: LongInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of LongInt from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteLongInt(const Item: LongInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of LongInt from Item to the underlying file at the last position, behind any existing data.
    function AppendLongInt(const Item: LongInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Int64 into Item from the underlying file at the current position.
    function ReadInt64(var Item: Int64): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Int64 from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteInt64(const Item: Int64): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Int64 from Item to the underlying file at the last position, behind any existing data.
    function AppendInt64(const Item: Int64): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Single into Item from the underlying file at the current position.
    function ReadSingle(var Item: Single): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Single from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteSingle(const Item: Single): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Single from Item to the underlying file at the last position, behind any existing data.
    function AppendSingle(const Item: Single): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of Double into Item from the underlying file at the current position.
    function ReadDouble(var Item: Double): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Double from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteDouble(const Item: Double): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of Double from Item to the underlying file at the last position, behind any existing data.
    function AppendDouble(const Item: Double): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of AnsiChar into Item from the underlying file at the current position.
    function ReadAnsiChar(var Item: AnsiChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of AnsiChar from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteAnsiChar(const Item: AnsiChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of AnsiChar from Item to the underlying file at the last position, behind any existing data.
    function AppendAnsiChar(const Item: AnsiChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads a single instance of WideChar into Item from the underlying file at the current position.
    function ReadWideChar(var Item: WideChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of WideChar from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteWideChar(const Item: WideChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of WideChar from Item to the underlying file at the last position, behind any existing data.
    function AppendWideChar(const Item: WideChar): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads NumChars worth of AnsiChars into Item from the underlying file at the current position.
    function ReadShortString(var Item: ShortString; const NumChars: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of ShortString from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteShortString(const Item: ShortString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of ShortString from Item to the underlying file at the last position, behind any existing data.
    function AppendShortString(const Item: ShortString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Reads NumChars worth of AnsiChars into Item from the underlying file at the current position.
    function ReadAnsiString(var Item: AnsiString; const NumChars: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of AnsiString from Item to the underlying file at the current position, possibly overwriting existing data.
    function WriteAnsiString(const Item: AnsiString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Writes a single instance of AnsiString from Item to the underlying file at the last position, behind any existing data.
    function AppendAnsiString(const Item: AnsiString): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Sets the current position of the underlying file to ToPosition, relative to 0.
    function Seek(const ToPosition: SizeInt): PLightFileStream; {$IFNDEF DEBUG}inline;{$ENDIF}
    //Closes the underlying file. Does not return a self-pointer, as it should always be the last method called.
    procedure Close; {$IFNDEF DEBUG}inline;{$ENDIF}
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

function TLightFileStream.WritePointerBuffer(const Buffer: Pointer; const NumBytesToRead: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileWrite(FHandle, Buffer^, NumBytesToRead);
  Result := @Self;
end;

function TLightFileStream.AppendPointerBuffer(const Buffer: Pointer; const NumBytesToRead: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if (not FOpen) or (FState <> fsWriting) then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, 0, fsFromEnd);
  FileWrite(FHandle, Buffer^, NumBytesToRead);
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

function TLightFileStream.Seek(const ToPosition: SizeInt): PLightFileStream;
begin
  {$IFNDEF NOCHECKS}
  if not FOpen then Exit(@Self);
  {$ENDIF}
  FileSeek(FHandle, ToPosition, fsFromBeginning);
  Result := @Self;
end;

procedure TLightFileStream.Close;
begin
  {$IFNDEF NOCHECKS}
  if not FOpen then Exit();
  {$ENDIF}
  FileClose(FHandle);
end;

end.
