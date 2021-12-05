# qBit4Delphi
qBit4Delphi is an open-source project created by Laurent Meyer to embed qBittorent WEBui API in applications developed with [Delphi VCL/FMX](https://www.embarcadero.com/products/delphi/starter).

Officiel API Docmentation: https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)

It requires no DLL, nor third-party extension. This is a pure Pascal. BUT before you try to compile qBit4Delphi, some embarcadero JSON .pas unit need to be patched (bugs / features). The procedure to do so is provided below. 
This API is developed with Delphi Community Edition 10.4 but also tested with Sydney 10.4.2 and Alexandria 11.0 (JSON patches provided) 

Alternatively, qNOXify is a thin client for qBittorent/NOX, it is a work in progress... so be gentle... Build as you wish, add features... contribute if you can... :)


## Installation in Delphi
* Download qBit4Delphi. Click on the green "Code" button and then click on "Download ZIP".
* Decompress the ZIP package in your projects directory.
* COPY FROM : the original Delphi source folder : C:\Program Files (x86)\Embarcadero\Studio\XX\source\...   (XX being 21.0 or 22.0) :
* "REST.Json.pas", "REST.Json.Types.pas", "REST.JsonReflect.pas" and "System.JSON.pas"
* TO : JSON/Sydney.10.4.2 or JSON/Alexandria.11.0
* Execute PatchDelphiUnits.bat to patch the files.
* You can build and run the demos...

* If you need help, open a ticket or contact me at : qBit4Delphi@ea4d.com

## Support
If you find this project useful, please consider making a donation.

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=N8SNLZRR6HEYE)

