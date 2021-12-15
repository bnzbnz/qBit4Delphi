# qBit4Delphi
qBit4Delphi is an open-source project created by Laurent Meyer to embed qBittorent WEBui API in applications developed with [Delphi VCL/FMX](https://www.embarcadero.com/products/delphi/starter).

Official API Documentation: https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)

It requires no DLL, nor third-party extension. This is pure Pascal VCL/FMX. BUT before you try to compile qBit4Delphi, some embarcadero JSON .pas units need to be patched (bugs / features). The procedure to do so is provided below. 
This API is developed with Delphi Community Edition 10.4 but also tested with Sydney 10.4.2 and Alexandria 11.0 (JSON patches provided) 

Alternatively, qNOXify is a thin client for qBittorent/NOX, it is a work in progress and has now its own repository : [qNOXify](https://github.com/bnzbnz/qNOXify)

## Installation in Delphi
* Download qBit4Delphi. Click on the green "Code" button and then click on "Download ZIP".
* Decompress the ZIP package in your projects directory.
* Execute : Patcher.exe in the main directory
* The patched units will be located in JSON/21 and/or JSON/22. Please add these units to you project.
* If you need help, open a ticket or contact me at : qBit4Delphi@ea4d.com

## Support
If you find this project useful, please consider making a donation.

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=N8SNLZRR6HEYE)

