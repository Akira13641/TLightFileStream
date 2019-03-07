program LightFileStreamExample;

{$mode Delphi}{$H+}{$J-}{$I-}

uses SysUtils, LightFileStream;

const
  DAA: array[0..5] of Double = (1.11, 2.22, 3.33, 4.44, 5.55, 6.66);

var
  DAB: array of Double;
  D: Double;
  SA: Ansistring = 'hello';
  SB: AnsiString = '     ';
  IA: SizeInt;

begin
  SetLength(DAB, 28);
  //If using the library in a {$mode ObjFPC} project, you can still use {$modeswitch AutoDeref}
  //to avoid having to manually dereference after each chained function call.
  TLightFileStream.Create('Example.txt')
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
  DeleteFile('Example.txt');
  DeleteFile('Example2.txt');
  DeleteFile('Example3.txt');
end.
