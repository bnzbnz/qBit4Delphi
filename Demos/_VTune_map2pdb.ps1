function mzp2pdb {
	param ( $AppName )
	Write-Host "$AppName.exe : ";
	.\map2pdb.exe -bind:"$AppName.exe" "$AppName.map"
}
mzp2pdb "TorrentReaderDemo"
mzp2pdb "FMXReport"
mzp2pdb "NOXMon"
mzp2pdb "Simple"
mzp2pdb "SimpleThreaded"
mzp2pdb "VnStatDemo"

pause
