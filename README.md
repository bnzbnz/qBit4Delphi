# qBit4Delphi
A Free Delphi VCL/FMX implementation of qBittorrent's WebAPI


WARNING... WARNING... WARNING... (this is said) :

In order to build and use qBit4Delphi some Embarcadero units need to be patched (Fixes and Enhancements)
This version is tested and patched against Delphi 10.4.2 (Sydney, Community Edition) and Delphi 11 (Alexandria) :

Copy :
	REST.Json.pas
	REST.Json.Types.pas
	REST.JsonReflect.pas
	System.JSON.pas

from the original source folder : 
	C:\Program Files (x86)\Embarcadero\Studio\XX\source\...   (XX being 21.0 or 22.0)	
to
	JSON/Sydney.10.4.2 or JSON/Alexandria.11.0

then go in that folder and execute PatchDelphiUnits.bat: Build and Run the demo, it should work.
	
	(Remember to always add this patched units to your projects)

qBit4Delphi@ea4d.com
