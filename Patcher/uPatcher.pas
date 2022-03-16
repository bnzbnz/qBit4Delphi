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
uses registry, shellapi;
{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  var Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    for var Version := 21 to 22 do
    begin
      var R := Reg.OpenKey(Format('Software\Embarcadero\BDS\%d.0', [Version]), False);
      if not R then begin Memo1.Lines.add(Format('Edition %d.0 not found.', [Version])); continue; end;
      Memo1.Lines.add(Format('Edition %d.0 found, processing...', [Version]));
      var RootDir := Reg.ReadString('RootDir');
      var DestDir := GetCurrentDir + Format('\API\JSON\%d\', [Version]);
      var F := CopyFile(PWideChar(RootDir + 'source\data\rest\REST.Json.pas'), PWideChar(DestDir + 'REST.Json.pas.org'), False);
      F := F and CopyFile(PWideChar(RootDir + 'source\data\rest\REST.Json.pas'), PWideChar(DestDir + 'REST.Json.pas'), False);
      F := F and CopyFile(PWideChar(RootDir + 'source\data\rest\REST.Json.Types.pas'),  PWideChar(DestDir + 'REST.Json.Types.pas.org'), False);
      F := F and CopyFile(PWideChar(RootDir + 'source\data\rest\REST.Json.Types.pas'),  PWideChar(DestDir + 'REST.Json.Types.pas'), False);
      F := F and CopyFile(PWideChar(RootDir + 'source\data\rest\REST.JsonReflect.pas'),  PWideChar(DestDir + 'REST.JsonReflect.pas.org'), False);
      F := F and CopyFile(PWideChar(RootDir + 'source\data\rest\REST.JsonReflect.pas'),  PWideChar(DestDir + 'REST.JsonReflect.pas'), False);
      F := F and CopyFile(PWideChar(RootDir + 'source\rtl\common\System.JSON.pas'),  PWideChar(DestDir + 'System.JSON.pas.org'), False);
      F := F and CopyFile(PWideChar(RootDir + 'source\rtl\common\System.JSON.pas'),  PWideChar(DestDir + 'System.JSON.pas'), False);
      if not F then Exit;
      Memo1.Lines.add(Format('Edition %d.0, Original files copied to %s', [Version, DestDir]));
      var CurDir := GetCurrentDir;
      SetCurrentDir(DestDir);
      ShellExecute(0, nil, PChar(DestDir + 'patch.exe'), PChar('-u REST.JsonReflect.pas REST.JsonReflect.pas.patch'), nil, SW_HIDE);
      ShellExecute(0, nil, PChar(DestDir + 'patch.exe'), PChar('-u REST.Json.pas REST.Json.pas.patch'), nil, SW_HIDE);
      ShellExecute(0, nil, PChar(DestDir + 'patch.exe'), PChar('-u System.JSON.pas System.JSON.pas.patch'), nil, SW_HIDE);
      Memo1.Lines.Add('Files Patched!!!!');
      SetCurrentDir(CurDir);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

end.

