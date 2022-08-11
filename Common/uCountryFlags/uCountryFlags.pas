unit uCountryFlags;

///
///  Author:  Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///
///  https://github.com/bnzbnz/qBit4Delphi
///  https://ip-api.com/docs/api:json
///
///  Thanks to : https://github.com/joielechong/iso-country-flags-svg-collection
///
///  License: MPL 1.1 / GPL 2.1
///

///
///  !!! PLEASE ADD >>>> CountryFlags.rc <<<< TO YOUR PROJECT !!!
///

interface
uses Vcl.Graphics;

function LoadCountryFlags(Picture: TPicture; CountryCode: string): Boolean;

implementation
uses Vcl.Imaging.pngimage, Classes, windows;

function LoadCountryFlags(Picture: TPicture; CountryCode: string):Boolean;
var
  RS: TResourceStream;
  JPGImage: TPNGImage;
begin
  Result := False;
  RS := Nil;
  JPGImage := TPNGImage.Create;
  try
  try
    RS := TResourceStream.Create(hInstance, CountryCode, RT_RCDATA);
    JPGImage.LoadFromStream(RS);
    Picture.Graphic := JPGImage;
    Result := True;
  except end;
  finally
    RS.Free;
    JPGImage.Free;
  end;
end;

end.
