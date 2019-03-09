program LightFileStreamExample;

{$mode Delphi}{$H+}{$J-}{$I-}{$R-}

uses SysUtils, LightFileStream;

type
  TDataRec = record
    S: Single;
    B: Boolean;
    C: AnsiChar;
  end;
  
const
  DAA: array[0..5] of Double = (1.11, 2.22, 3.33, 4.44, 5.55, 6.66);
  DRA: TDataRec = (S: 1.0; B: True; C: 'A');

var
  D: Double;
  DAB: array of Double;
  SA: AnsiString = 'hello';
  SB: AnsiString = '     ';
  IA: SizeInt;
  C: Char;
  CAA: array[0..5] of Char = (#0, #0, #0, #0, #0, #0);
  H: UnicodeString = '     ';
  G: UnicodeString = '       ';
  IB: LongInt;
  FA: Single;
  DRB: TDataRec;
  LS: TLightFileStream;
  PLS: PLightFileStream;

begin
  SetLength(DAB, 28);
  
  //If using the library in a {$mode ObjFPC} project, you can still use {$modeswitch AutoDeref}
  //to avoid having to manually dereference after each chained function call.
  
  TLightFileStream.Create('Example1.txt')
                  .WriteTypedBuffer<Double>(DAA[0], 6)
                  .WriteTypedBuffer<Double>(DAA[0], 6)
                  .SeekFromBeginning(0)
                  .AppendTypedBuffer<Double>(DAA[0], 6)
                  .WritePointerBuffer(@DAA[0], SizeOf(Double) * 6)
                  .WriteDouble(99.99)
                  .SeekFromBeginning(0)
                  .AppendDouble(128.12)
                  .WriteType<Double>(77.77)
                  .WriteDouble(345.34)
                  .ChangeFileStateTo(fsReading)
                  .ReadPointerBuffer(@DAB[0], SizeOf(Double) * 28)
                  .Close();
  for D in DAB do WriteLn(D : 0 : 2);
  
  TLightFileStream.Create('Example2.txt')
                  .WriteAnsiString(SA)
                  .ChangeFileStateTo(fsReading)
                  .ReadAnsiString(SB, 5)
                  .Close();
  WriteLn(SB);
  
  TLightFileStream.Create('Example3.txt')
                  .WriteQWord(1)
                  .WriteDouble(2.0)
                  .WriteSingle(3.9)
                  .WriteType<QWord>(4)
                  .WriteType<Double>(5.7)
                  .WriteType<Single>(6.8)
                  .SeekFromBeginning(0)
                  .LogSize()
                  .LogPosition()
                  .GetSize(IA)
                  .SeekFromBeginning(IA div 2)
                  .Truncate()
                  .LogSize()
                  .LogPosition()
                  .Close();
                  
  TLightFileStream.Create('Example4.txt')
                  .FillWith<Char>('Z', 6)
                  .ChangeFileStateTo(fsReading)
                  .ReadTypedBuffer<Char>(CAA[0], 6)
                  .Close();
  for C in CAA do WriteLn(C);
  
  {The library can also of course be used without fully chaining everything.
   The next line makes the LS variable our base non-pointer instance of the TLightFileStream record.}
  LS := TLightFileStream.Create('Example5.txt');
  
  {It's fine to call any of the functions that return a self-pointer without the result connecting to anything.
   In that case, the self-pointer is just discarded. The next line shows an example of this.}
  LS.WriteUnicodeString('hello').WriteUnicodeString('goodbye');
  
  {You can also have a named PLightFileStream variable and assign the self-pointer results to it, as shown on the next line.}
  PLS := LS.WriteLongInt(1).WriteSingle(2.94).WriteType<TDataRec>(DRA);

  {At this point, accessing either LS or PLS does the same thing, as PLS is just a pointer to LS.
   So on the next line, we'll do something similar to the one above, but both starting from and returning into PLS.}
  PLS := PLS.ChangeFileStateTo(fsReading)
            .ReadUnicodeString(H, 5)
            .ReadUnicodeString(G, 7)
            .ReadLongInt(IB)
            .ReadSingle(FA)
            .ReadType<TDataRec>(DRB);

  {Next we'll display the values...}
  WriteLn(H);
  WriteLn(G);
  WriteLn(IB);
  WriteLn(FA : 0 : 2);
  with DRB do begin
    WriteLn(S : 0 : 2);
    WriteLn(B);
    WriteLn(C);
  end;

  {And finally we'll close the file through PLS.}
  PLS.Close();
  
  DeleteFile('Example1.txt');
  DeleteFile('Example2.txt');
  DeleteFile('Example3.txt');
  DeleteFile('Example4.txt');
  DeleteFile('Example5.txt');
end.
