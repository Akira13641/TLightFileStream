program LightFileStreamBenchmark;

{$mode Delphi}{$H+}{$J-}{$I-}

uses SysUtils, Classes, EpikTimer, LightFileStream;

const
  QC: QWord = 1844674407370955;

var
  QV, I, J: QWord;
  TimeA, TimeB, TimeC, TimeD: String;
  Timer: TEpikTimer;

begin
  Timer := TEpikTimer.Create(nil);
  Timer.Start();
  with TFileStream.Create('OutputA.bin', fmCreate) do begin
    for I := 0 to 2999999 do WriteQWord(QC);
    Free();
  end;
  Timer.Stop();
  TimeA := Timer.ElapsedStr();
  Timer.Clear();
  Timer.Start();
  with TLightFileStream.Create('OutputB.bin') do begin
    for J := 0 to 2999999 do WriteQWord(QC);
    Close();
  end;
  Timer.Stop();
  TimeB := Timer.ElapsedStr();
  Timer.Clear();
  Timer.Start();
  with TFileStream.Create('OutputA.bin', fmOpenRead) do begin
    for I := 0 to 2999999 do QV := ReadQWord;
    Free();
  end;
  Timer.Stop();
  TimeC := Timer.ElapsedStr();
  Timer.Clear();
  Timer.Start();
  with TLightFileStream.Open('OutputB.bin', fsReading) do begin
    for J := 0 to 2999999 do ReadQWord(QV);
    Close();
  end;
  Timer.Stop();
  TimeD := Timer.ElapsedStr();
  Timer.Free();
  WriteLn('TFileStream Write: ', TimeA);
  WriteLn('TLightFileStream Write: ', TimeB);
  WriteLn('TFileStream Read: ', TimeC);
  WriteLn('TLightFileStream Read: ', TimeD);
  DeleteFile('OutputA.bin');
  DeleteFile('OutputB.bin');
end.