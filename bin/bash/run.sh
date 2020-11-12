#!/bin/bash Rscript

echo Beginning MicroCyte data extraction protocol

echo Activating the purge.
if [ ! -z "$1" ]; then
	echo Running purge on on $1 mode...
	if [ "$1" == "manual" ]; then
		echo Using $2 and $3 as spectral overlap...
	fi
	Rscript ../Rscripts/purgo.R $1 $2 $3
else
	Rscript ../Rscripts/purgo.R
	echo Skipping the purge but generating PNGs anyway
fi

echo Renaming the images based on the schema...
Rscript ../Rscripts/reName.R

echo Opening ImageJ and running ImageJ macro YggData_multi...
echo When macro is complete, close ImageJ to trigger ImaGen.

../Fiji.app/ImageJ-linux64 -macro YggData_multi.ijm 

echo Running ImaGen...
Rscript ../Rscripts/imaGen.R

echo Combining image data into a single ID datasets...
Rscript ../Rscripts/concato.R

echo Process complete.
