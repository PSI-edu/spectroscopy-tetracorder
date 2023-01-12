#!/bin/sh

# This script make a new instrument rlib convolved spectral library.
#
# R. Clark  12/2021

# Example:
#                                                                   1     2   3       4       5   6
# ./AAA.make.new.instrument.convolved.rlib-spectral.library.sh  r06an21a  a  425   AVIRNG21a 12  [noX]

if [ -z "$5" ]
then
	echo "insufficient input"
	echo " "
	echo "$0 library_name"
        echo "$0 7char_library_name   1char_library_extension   number_of_channels   title_keyword  fwhm_record_number "
	echo " "
	echo "Example:"
	echo "           $0  r06an21  a  425   AVIRNG21a 12 "
	echo " "

	echo "exit 1"
	exit 1
fi

name7=$1
name1=$2
chans=$3
title_keyword=$4
fwhmrec=$5

xwin=" "   # default: do x-windows

if [ "$6" = "noX" ]
then
	xwin="noX"
fi

rlib06name=$name7$name1
splib06name=`echo $rlib06name | sed -e 's/^r06/s06/'`

echo "library: $rlib06name will be setup from $splib06name library"


if [ ! -f "/sl1/usgs/library06.conv/startfiles/$splib06name.start" ]
then
	echo "Could not find /sl1/usgs/library06.conv/startfiles/$splib06name.start"
	echo "build $splib06name in /sl1/usgs/library06.conv/ first"
	echo "exit 1"
	exit 1
fi
if [ ! -f "/sl1/usgs/library06.conv/restartfiles/r.$splib06name" ]
then
	echo "Could not find "/sl1/usgs/library06.conv/restartfiles/r.$splib06name
	echo "build $splib06name in /sl1/usgs/library06.conv/ first"
	echo "exit 1"
	exit 1
fi

if [ -f "startfiles/$rlib06name.start" ]
then
	echo "startfiles/$rlib06name.start already exists"

	echo "exit 1"
	exit 1
fi

if [ -f "restartfiles/r.$rlib06name" ]
then
	echo "restartfiles/r.$rlib06name already exists"
	echo "exit 1"
	exit 1
fi

if [ -f "$rlib06name" ]
then
	echo "$rlib06name already exists"
	echo "exit 1"
	exit 1
fi

echo "Making startfiles/$rlib06name.start"

cp /sl1/usgs/library06.conv/startfiles/$splib06name.start startfiles/$rlib06name.start
spsettitle startfiles/$rlib06name.start 1 "Digital Spectral Library: $rlib06name            "  force
spsetwave startfiles/$rlib06name.start  6  6 12  force
spsetwave startfiles/$rlib06name.start 12  6 12  force
spprint startfiles/$rlib06name.start


echo "Making restartfiles/r.$rlib06name"

cp /sl1/usgs/library06.conv/restartfiles/r.$splib06name restartfiles/r.$rlib06name

mv restartfiles/r.$rlib06name restartfiles/r.foo

cat restartfiles/r.foo | sed -e "s/ivfl=$splib06name/ivfl=$rlib06name     /" \
		-e "s/iyfl=splib06b/iyfl=sprlb06b/" \
		-e "s/isavt=      $splib06name/isavt=      $rlib06name  /" \
		-e "s/irfl=r.$splib06name/irfl=r.$rlib06name /"           \
		-e "s/inmy=       splib06b/inmy=       sprlb06b /" \
		> restartfiles/r.$rlib06name

#ex - restartfiles/r.$rlib06name  <<EOI
#s/ivfl=$splib06nam/ivfl=$rlib06name   /
#s/iyfl=splib06b/iyflsprlb06b/
#s/isavt=      s06pr01a/isavt=      $rlib06name  /
#s/inmy=       splib06b/inmy=       sprlb06b /
#w
#q
#EOI

# ***************************************************************
# ***************************************************************
# ***********                           *************************
# ***********    convolution commands   *************************
# ***********                           *************************
# ***************************************************************
# ***************************************************************

# Commands:

#                prog              lib       n      chan      title          fwhm
#                                                             keyword      rec number

#           ./mak.convol.library  r06av95    a      224       AVIRIS95       12  

      echo "./mak.convol.library  $name7  $name1  $chans   $title_keyword  $fwhmrec  $xwin"
            ./mak.convol.library  $name7  $name1  $chans   $title_keyword  $fwhmrec  $xwin



# now check that the number of spectra in the convolved library is consistent

convolcount=`spprint $rlib06name | grep -v 'Characters of TEXT' | grep -v '^ done$' | \
	grep -v 'Wavelengths ' | grep -v 'Resolution ' | grep -v 'Bandpass ' | \
	grep -v 'Data value =' | wc -l`

sprlb06bcount=`spprint sprlb06b | grep -v 'Characters of TEXT' | grep -v '^ done$' | \
	grep -v 'Wavelengths ' | grep -v 'Resolution ' | grep -v 'Bandpass ' | \
	grep -v 'Data value =' | wc -l`

sprlb06acount=`spprint sprlb06a | grep -v 'Characters of TEXT' | grep -v '^ done$' | \
	grep -v 'Wavelengths ' | grep -v 'Resolution ' | grep -v 'Bandpass ' | \
	grep -v 'Data value =' | wc -l`

echo " "
echo "number of spectra in sprlb06a = $sprlb06acount"
echo "number of spectra in sprlb06b = $sprlb06bcount"
echo "number of spectra in $rlib06name = $convolcount"
