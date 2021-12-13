program FMXReport;

uses
  {$IFDEF VER340}
    REST.Json.Types in '..\..\JSON\Sydney.10.4.2\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\JSON\Sydney.10.4.2\REST.JsonReflect.pas',
    System.JSON in '..\..\JSON\Sydney.10.4.2\System.JSON.pas',
    REST.Json in '..\..\JSON\Sydney.10.4.2\REST.Json.pas',
  {$ENDIF }
  {$IFDEF VER350}
    REST.Json.Types in '..\..\JSON\Alexandria.11.0\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\JSON\Alexandria.11.0\REST.JsonReflect.pas',
    System.JSON in '..\..\JSON\Alexandria.11.0\System.JSON.pas',
    REST.Json in '..\..\JSON\Alexandria.11.0\REST.Json.pas',
  {$ENDIF}

  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',

  System.StartUpCopy,
  FMX.Forms,
  uFMXReport in 'uFMXReport.pas' {Form1},
  uPatcherChecker in '..\common\uPatcherChecker.pas';

{Form1}


{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

(*
  {$IFDEF VER340}
    REST.Json.Types in '..\..\JSON\Sydney.10.4.2\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\JSON\Sydney.10.4.2\REST.JsonReflect.pas',
    System.JSON in '..\..\JSON\Sydney.10.4.2\System.JSON.pas',
    REST.Json in '..\..\JSON\Sydney.10.4.2\REST.Json.pas',
  {$ENDIF }
  {$IFDEF VER350}
    REST.Json.Types in '..\..\JSON\Alexandria.11.0\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\JSON\Alexandria.11.0\REST.JsonReflect.pas',
    System.JSON in '..\..\JSON\Alexandria.11.0\System.JSON.pas',
    REST.Json in '..\..\JSON\Alexandria.11.0\REST.Json.pas',
  {$ENDIF}

  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',
*)

