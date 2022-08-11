// Thanks to : https://github.com/joielechong/iso-country-flags-svg-collection

///
///  !!! PLEASE ADD FIRST >>>> CountryFlags.rc <<<< TO YOUR PROJECT !!!
///

unit uCountryFlags;

interface
uses Vcl.Graphics;

procedure LoadCountryFlags(Picture: TPicture; CountryCode: string);

implementation
uses Vcl.Imaging.pngimage, Classes, windows;

procedure LoadCountryFlags(Picture: TPicture; CountryCode: string);
var
  RS: TResourceStream;
  JPGImage: TPNGImage;
begin
  RS := Nil;
  JPGImage := TPNGImage.Create;
  try
  try
    RS := TResourceStream.Create(hInstance, CountryCode, RT_RCDATA);
   JPGImage.LoadFromStream(RS);
   Picture.Graphic := JPGImage;
  except end;
  finally
    RS.Free;
    JPGImage.Free;
  end;
end;

end.
