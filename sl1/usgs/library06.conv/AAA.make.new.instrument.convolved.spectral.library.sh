#!/bin/sh


# This script make a new instrument convolved spectral library.
#
# R. Clark  12/2021

if [ ! -f "splib06b" ]
then
	echo "splib06b spectral library not found."
	echo "Please make it before running this script"
	echo "exit 1"
	exit 1
fi

# Example:
#                                                            1     2   3       4     5
# ./AAA.make.new.instrument.convolved.spectral.library.sh s06av95  a  224   AVIRIS95 12 \ 
#	'Convolved AVIRIS 1995  224 ch library   '  \ 
#       'Wavelengths in microns 224 ch AVIRIS95a ' \ 
#       'Resolution  in microns 224 ch AVIRIS95a '   -waves wavefile   -fwhm fwhmfile
#
#     args 1 + 2 = the mane of the output spectral linbrary
#     arg3 = number of channels
#     arg4 = name added into the specpr titles
#     arg5 = specpr record number for the resolution, full width half max, FWHM, in the convolved library file

if [ -z "$12" ]
then
	echo "insufficient input"
	echo " "
	echo "$0 7char_library_name   1char_library_extension   number_of_channels   title_keyword \ "
	echo "fwhm_record_number   title2   wave_title   resol_title   -waves wavefile   -fwhm fwhmfile"

	echo " "
	echo "Example:"
	echo " "
        echo "./AAA.make.new.instrument.convolved.spectral.library.sh s06av95  a  224   AVIRIS95 12 \ "
        echo "      'Convolved AVIRIS 1995  224 ch library   '  \ "
        echo "      'Wavelengths in microns 224 ch AVIRIS95a ' \ "
        echo "      'Resolution  in microns 224 ch AVIRIS95a ' \ "
	echo "      -waves aviris95_waves.txt -fwhm aviris95_fwhm.txt [noX]"
	echo " "
	echo "-waves and -fwhm are single column ascii lists in microns"
	echo "number_of_channels must in the range: 1 - 2171"
	echo "option noX does not make plots in an X-window"
	echo " "

fi

# note: in the following echo count, a newline is included, so 7 character string = 8 out of the wc"
c1=`echo "$1" | wc -c`
if [ "$c1" -ne "8" ]
then
	echo "7char_library_name is not 7 characters.  Found: $1"
	echo "exit 1"
	exit 1
fi
c2=`echo "$2" | wc -c`
if [ "$c2" -ne "2" ]
then
	echo "1char_library_extension is not 1 character.  Found: $1"
	echo "exit 1"
	exit 1
fi

library_name=$1$2
chans=$3            # number of channels
title_keyword=$4
fwhmrec=$5
title2=$6
wave_title=$7
resol_title=$8
xoption=" "     # do X-windows plots if blank

echo "Library name =  $library_name"
echo "Channels =      $chans"
echo "title keyword = $title_keyword"
echo "fwhm record =   $fwhmrec"
echo "title2 =        $title2"
echo "wavelength title = $wave_title"
echo "resolution title = $resol_title"

if [ -f "startfiles/${library_name}.start" ]
then
	echo " "
	echo "startfiles/${library_name}.start  already exists"
	echo "exit 1"
	exit 1
fi

if [ -f "restartfiles/r.${library_name}" ]
then
	echo " "
	echo "restartfiles/r.${library_name}  already exists"
	echo "exit 1"
	exit 1
fi
if [ -f "${library_name}" ]
then
	echo " "
	echo "library  ${library_name}  already exists"
	echo "exit 1"
	exit 1
fi


# to be added: more checks on number of channels and fwhm record number

if [ "$chans" -lt "0" ]
then
	echo "number of channels < 1: $chans"
	echo "exit 1"
	exit 1
fi
if [ "$chans" -gt "2171" ]
then
	echo "this script currently only works for channels < 2172, got: $chans"
	echo "exit 1"
	exit 1
fi

if [ "$9" = "-waves" ]
then
	if [ ! -f "${10}" ]
	then
		echo "expected a file after -waves, found: ${10}"
		echo "${10} does not exist"
		echo "exit 1"
		exit 1
	fi
else
	echo "expected -waves, found: $9"
	echo "exit 1"
	exit 1
fi
waves=${10}
echo "ascii wavelengths $9 = ${10}"

if [ "${11}" = "-fwhm" ]
then
	if [ ! -f "${12}" ]
	then
		echo "expected a file after -fwhm, found: ${12}"
		echo "${12} does not exist"
		echo "exit 1"
		exit 1
	fi
else
	echo "expected -fwhm, found: ${11}"
	echo "exit 1"
	exit 1
fi
fwhm=${12}
echo "ascii resolution ${11} = ${12}"

if [ "$13" = "noX" ]
then
	echo "plots will be ascii, no X-windows"
	xoption="noX"
fi

cp $waves  waves.txt
cp $fwhm   resol.txt

# make new library startfile

# Example:
#
#   ./make.new.convol.library.start.file  s06av18a 224 \ 
#              'Convolved AVIRIS 2018  224 ch library   '  \ 
#              'Wavelengths in microns 224 ch AVIRIS18a ' \ 
#              'Resolution  in microns 224 ch AVIRIS18a '
#
# NOTE: title must be 40 characters or less
#
#  ./make.new.convol.library.start.file makes the specpr startfile and the restart file

echo "./make.new.convol.library.start.file  $library_name  $chans  "$title2"  "$wave_title"  "$resol_title" force"
      ./make.new.convol.library.start.file  $library_name  $chans  "$title2"  "$wave_title"  "$resol_title" force

success=$?
if [ "$success" -ne "0" ]
then
	echo "exit 1"
	exit 1
fi

# set the wavelength and fwhm pointers.   The following is valid for up to 2171 channels

spsetwave startfiles/${library_name}.start   6  6  12  force
spsetwave startfiles/${library_name}.start  12  6  12  force
spsetwave startfiles/${library_name}.start  18  6  12  force
spprint   startfiles/${library_name}.start


# now convolve the new instrument library

#        ./mak.convol.library  library version channels name     resol_number

# example:

#        ./mak.convol.library  s06av95  a        224   AVIRIS95 12


echo "./mak.convol.library $1 $2 $chans  $title_keyword  $fwhmrec $xoption"
      ./mak.convol.library $1 $2 $chans  $title_keyword  $fwhmrec $xoption


echo "done"

echo " "
echo "please add the following command to AAAA.convolve.all.instrument.libraries.sh"
echo " "
echo "./mak.convol.library $1 $2 $chans  $title_keyword  $fwhmrec $xoption"
echo " "
