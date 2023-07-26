unit uPatcher;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;
type
  TCmdExecuteSyncOutputEvent = procedure(const Output: string) of object;
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form2: TForm2;
implementation
uses registry, shellapi, ioutils;
{$R *.dfm}
function FileCopy(S, D: string): Boolean;
begin
  try
    Result := False;
    DeleteFile(D);
    try TFile.Copy(S, D, True); except end;
    Var Attrs := TFile.GetAttributes(D);
    Exclude(Attrs, TFileAttribute.faReadOnly);
    TFile.SetAttributes(D, Attrs);
    Result := True;
  except end;
end;
procedure TForm2.Button1Click(Sender: TObject);
begin
  var Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    for var Version := 21 to 23 do
    begin
      var R := Reg.OpenKey(Format('Software\Embarcadero\BDS\%d.0', [Version]), False);
      if not R then begin Memo1.Lines.add(Format('Edition %d.0 not found.', [Version])); continue; end;
      Memo1.Lines.add(Format('Edition %d.0 found, processing...', [Version]));
      var RootDir := Reg.ReadString('RootDir');
      var DestDir := GetCurrentDir + Format('\API\JSON\%d\', [Version]);
      var F := FileCopy((RootDir + 'source\data\rest\REST.Json.pas'), (DestDir + 'REST.Json.pas'));
      F := F and FileCopy((RootDir + 'source\data\rest\REST.Json.Types.pas'),  (DestDir + 'REST.Json.Types.pas'));
      F := F and FileCopy((RootDir + 'source\data\rest\REST.JsonReflect.pas'),  (DestDir + 'REST.JsonReflect.pas'));
      F := F and FileCopy((RootDir + 'source\rtl\common\System.JSON.pas'),  (DestDir + 'System.JSON.pas'));
      if not F then
      begin
         Memo1.Lines.add(Format('Edition %d.0, Copy Failed!', [Version]));
        Exit;
      end;
      Memo1.Lines.add(Format('Edition %d.0, Files copied to %s', [Version, DestDir]));
      var CurDir := GetCurrentDir;
      SetCurrentDir(DestDir);
      ShellExecute(0, nil, PChar(DestDir + 'patch.exe'), PChar('-u REST.JsonReflect.pas REST.JsonReflect.pas.patch'), nil, SW_HIDE);
      ShellExecute(0, nil, PChar(DestDir + 'patch.exe'), PChar('-u REST.Json.pas REST.Json.pas.patch'), nil, SW_HIDE);
      ShellExecute(0, nil, PChar(DestDir + 'patch.exe'), PChar('-u System.JSON.pas System.JSON.pas.patch'), nil, SW_HIDE);
      Memo1.Lines.add(Format('Edition %d.0, Files Patched!', [Version]));
      SetCurrentDir(CurDir);
      Reg.CloseKey;
    end;
    Memo1.Lines.Add('');
    Memo1.Lines.Add('==> Patch Successful');
  finally
    Reg.Free;
  end;
end;
end.
