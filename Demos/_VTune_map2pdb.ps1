function map2pdb {
	param ( $AppName )
	Write-Host "$AppName.exe : ";
	.\map2pdb.exe -bind:"$AppName.exe" "$AppName.map"
}
map2pdb "TorrentReaderDemo"
map2pdb "FMXReport"
map2pdb "NOXMon"
map2pdb "Simple"
map2pdb "SimpleThreaded"
map2pdb "VnStatDemo"
map2pdb "IPAPIDemo"

pause
