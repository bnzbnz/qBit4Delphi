# qBit4Delphi
qBit4Delphi is an open-source project created by Laurent Meyer to embed qBittorent WEbUI API in applications developed with [Delphi VCL/FMX](https://www.embarcadero.com/products/delphi/starter), see [qNOXify](https://github.com/bnzbnz/qNOXify). This is a full implementation.

This package contains also :
- TTorrentReader: a torrent file parser (UTF8 BEncode, Format: V1, V2 and Hybrid).
- TvnStatClient : a vnStat client to get bandwith usage statistics from a remote linux server (metered connection)
- TIP_API : an interface to ip-api.com to get any IP data
- uCountryFlags: country flags .png images from app. resources (ISO 639)
            
Official WebUI API Documentation: https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)

It requires no DLL, nor third-party extension. This is pure Pascal VCL/FMX. BUT before you try to compile qBit4Delphi, some Embarcadero JSON .pas units need to be patched (bugs / features). The procedure to do so is provided below. 
This API is developed with Delphi Community Edition 10.4 but also tested with Sydney 10.4.2, Alexandria 11.3 (JSON patches provided) 

Alternatively, qNOXify is a thin client for qBittorent/NOX, it is a work in progress and has now its own repository : [qNOXify](https://github.com/bnzbnz/qNOXify)

## Installation in Delphi
* To download qBit4Delphi. Click the "Source code.zip" from the latest [Release](https://github.com/bnzbnz/qBit4Delphi/releases/tag/v1.100.2.8.3).
* Decompress the ZIP package in your projects directory.
* Execute : Patcher.exe in the main directory
* The patched units will be located in API/JSON/21 and/or API/JSON/22. Please add these units to your project.
* If you need help, open a ticket or contact me at : qBit4Delphi@ea4d.com

## Support
If you find this project useful, please consider making a donation.

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=N8SNLZRR6HEYE)

