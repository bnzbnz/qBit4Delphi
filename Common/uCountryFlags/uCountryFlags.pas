// Thanks to : https://github.com/joielechong/iso-country-flags-svg-collection

///
///  !!! PLEASE ADD >>>> CountryFlags.rc <<<< TO YOUR PROJECT !!!
///

unit uCountryFlags;

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
