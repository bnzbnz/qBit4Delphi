unit uqBitPatchChecker;

///
///  Author:  Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///
///  https://github.com/bnzbnz/qBit4Delphi:
///
///  License: MPL 1.1 / GPL 2.1
///

interface

implementation

uses REST.Json, Sysutils,
{$IF DECLARED(FireMonkeyVersion)}
  FMX.Dialogs;
{$ELSE}
  VCL.Dialogs;
{$IFEND}

type

  TqBitPatchCheckedType = class
    _DummyString: string;       // << if patched, properties starting by '_' are not serialized
  end;

initialization
  var Checker := TqBitPatchCheckedType.Create;
  var Str := TJson.ObjectToJsonString(Checker);
  Checker.Free;
  if Str <> '{}'  then
  begin
    var SB := TStringBuilder.Create;
    SB.Append('JSON/REST Files are not patched  or Delphi may have messsed up your project source file !!');
    SB.AppendLine; SB.AppendLine;
    SB.Append('Please use Patcher.exe or include/check the patched files (see Demo''s source project files)');
    ShowMessage(SB.ToString);
    SB.Free;
    Halt(1); // Memory Leaks, we halt so nevermind
  end;
end.
