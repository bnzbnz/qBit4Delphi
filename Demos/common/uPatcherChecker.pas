unit uPatcherChecker;

interface

 procedure PatcherChecker;

implementation

uses REST.Json,
{$IF DECLARED(FireMonkeyVersion)}
  FMX.Dialogs;
{$ELSE}
  VCL.Dialogs;
{$IFEND}


type
  TPatcherCheckedType = class
    _DummyInteger: string;
  end;

procedure PatcherChecker;
var
  Checker: TPatcherCheckedType;
begin
  Checker := TPatcherCheckedType.Create;
  var s := TJson.ObjectToJsonString(Checker);
  if pos('_DummyInteger', s)>0  then  // Properties starting by '_' shouldn't be serialized...
  begin
    ShowMessage('JSON/REST Files are not patched  or Delphi may have messsed up your project source file !!');
    ShowMessage('Please use Patcher.exe or include/check the patched files (see Demo''s source project file)');
    Halt;
  end;
  Checker.Free;
end;

initialization
  PatcherChecker;
end.
