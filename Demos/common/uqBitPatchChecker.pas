unit uqBitPatchChecker;

interface

implementation

uses REST.Json,
{$IF DECLARED(FireMonkeyVersion)}
  FMX.Dialogs;
{$ELSE}
  VCL.Dialogs;
{$IFEND}

type

  TqBitPatchCheckedType = class
    _DummyInteger: string;
  end;

initialization
  var Checker := TqBitPatchCheckedType.Create;
  var s := TJson.ObjectToJsonString(Checker);
  if s <> '{}'  then  // Properties starting by '_' shouldn't be serialized...
  begin
    ShowMessage('JSON/REST Files are not patched  or Delphi may have messsed up your project source file !!');
    ShowMessage('Please use Patcher.exe or include/check the patched files (see Demo''s source project file)');
    Halt;
  end;
  Checker.Free;
end.
