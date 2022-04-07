#!/bin/sh

# R. Clark, 2019 - 2020

# This software was written by Roger N. Clark and colleagues and falls under the following license:
# 
# Copyright (c) 1975 - 2020, Roger N. Clark,
#                            Planetary Science Institute, PSI
#                            rclark@psi.edu, and colleagues named in the code.
# 
# All rights reserved.
# 
# GNU General Public License https://www.gnu.org/licenses/gpl.html
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#   Redistributions of the program must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
#
#   Neither Roger N. Clark, Planetary Science Institute, nor the names of
#   contributors to this software may be used to endorse or promote products
#   derived from this software without specific prior written permission.
#
#   Translation/recoding into other languages: the translation and source code
#   must be made available for free.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS"
#   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
#   BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
#   OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
#   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
#   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
#   THE POSSIBILITY OF SUCH DAMAGE.


doinstall=0

# edit location as desired:
t1='/home/t1'     # tetracorder directories
sl1='/home/sl1'   # spectral library directories
src='/home/src'   # source code directories

# Note: will create /usr/spool/ /usr/spool/gplot directories and /usr/spool/plot.log file

echo '******* The is the system setup script to compile and run specpr, tetracorder *******'
echo '******* and to set up the data directories, including for spectral libraries. *******'
echo " "
echo "     WARNING: this script may add directories, links at root level, users, groups"
echo " "
echo " "

if [ "$1" = "install" -o "$1" = "-install" ]
then
	doinstall=1

	app1=`which apt-get | grep -v 'not found'`
	app2=`which yum     | grep -v 'not found'`

	if [ -n "$app1" ]
	then
		echo " "
		echo "app installer found: $app1"
		aget='apt-get -y install '   # program to install programs
		echo "using $aget"

	elif [ -n "$app2" ]
	then
		echo " "
		echo "app installer found: $app2"
		aget='yum install'   # program to install programs
		echo "using $aget"
	else
		echo " "
		echo "no app installer found.  will not do any package installs"
		echo "looked for apt-get and yum."
		echo "     If your system uses something else, change this script above."
		doinstall=0
	fi
fi

echo " "
echo "You must do this as root.  Read the script before proceeding."
echo "    You should understand what it does."
echo " "
echo "The following directories will be made and links created from the root directory."
echo "    If you want to put these directories somewhere else, stop and edit this script."
echo " "
echo "      $t1     # tetracorder directories"
echo "      $sl1   # spectral library directories"
echo "      $src   # source code directories"
echo " "
echo "      ln -s $src /src will also be made if not already existing"
echo " "
if [ "$doinstall" = "1" ]
then
	echo "Will install system packages if programs not found."
	echo "     installer= $aget"
fi
echo " "
echo "Type  y  to continue, anything else exits."
read y
if [ "$y" != "y" ]
then
	echo "exit 1"
	exit 1
fi

#######################################################################################

ifoundthem=0
numbertested=0
notfound=" "

echo "Checking for programs that must be installed"
echo "Note, this assumes programs are installed with $aget"

for i in  gfortran make gcc g++ ratfor tcsh csh aplay gnuplot gnuplot-x11 imagemagick  tgif javac
do
	if [ "$i" = "imagemagick" ]
	then
		a=`which ppmtopgm | grep -v 'not found'`
		numbertested=`expr $numbertested + 1`
	else
		a=`which $i | grep -v 'not found'`
		numbertested=`expr $numbertested + 1`
	fi

	if [ -n "$a" ]
	then
		echo " "
		ifoundthem=`expr $ifoundthem + 1`
		echo "found: $a"
	else
		if [ "$doinstall" = "1" ]
		then
			echo " "
			echo "NOT FOUND: $i  ... INSTALLING:"

			if [ "$i" = "imagemagick" ]
			then
				ifound1 = 0
				echo " "

				echo "$aget imagemagick imagemagick-common imagemagick-doc"
				$aget imagemagick imagemagick-common imagemagick-doc
				if [ $? -ne 0 ]
				then
					echo "INSTALL FAILED for $i"
					notfound=`echo "$notfound $i"`
					echo "exit 1"
					exit 1
				else
					ifound1 = `expr $ifound1 +1`
				fi

				echo "$aget netpbm"
				$aget netpbm
				if [ $? -ne 0 ]
				then
					echo "INSTALL FAILED for $i"
					notfound=`echo "$notfound $i"`
					echo "exit 1"
					exit 1
				else
					ifound1 = `expr $ifound1 +1`
				fi
				if [ $ifound1 -gt 1]
				then
					ifoundthem=`expr $ifoundthem + 1`
				fi
			elif [ "$i" = "javac" ]
			then
				ifound1 = 0
				echo " "
				echo "$aget java-common   # base package for java runtimes"
				$aget java-common   # base package for java runtimes
				if [ $? -ne 0 ]
				then
					echo "INSTALL FAILED for $i"
					notfound=`echo "$notfound $i"`
					echo "exit 1"
					exit 1
				else
					ifound1 = `expr $ifound1 +1`
				fi
				#old: echo "$aget openjdk-8-jdk  openjdk-8-doc # for compiling java"

				echo "$aget default-jdk  default-jdk-doc # for compiling java"
				$aget default-jdk  default-jdk-doc # for compiling java
				if [ $? -ne 0 ]
				then
					echo "INSTALL FAILED for $i"
					notfound=`echo "$notfound $i"`
					echo "exit 1"
					exit 1
				else
					ifound1 = `expr $ifound1 +1`
				fi
				if [ $ifound1 -gt 1]
				then
					ifoundthem=`expr $ifoundthem + 1`
				fi
			else
				echo " "
				echo "$aget $i"
				$aget $i
				ifoundthem=`expr $ifoundthem + 1`
				if [ $? -ne 0 ]
				then
					echo "INSTALL FAILED for $i"
					notfound=`echo "$notfound $i"`
					echo "exit 1"
					exit 1
				else
					ifoundthem=`expr $ifoundthem + 1`
				fi
			fi
		else
			echo " "
			echo "NOT FOUND: $i"
			notfound=`echo "$notfound $i"`
		fi
	fi
done

if [ "$doinstall" = "1" ]
then
	echo " "
	echo "Checking to be sure other packages are installed."
	for j in libx11-dev glibc-doc glibc-doc-reference libxpm-dev libxt-dev \
		libpng-dev libjbig-dev:amd64 libjbig0:amd64 libjbig0:i386 \
		libjbig2dec0 libjbig2dec0-dev jbig2dec jbigkit-bin \
		libjpeg8-dev zlib1g zlib1g-dev zlib1g:i386 \
		inotify-tools vim  vim-common  vim-runtime  vim-tiny \
		imagemagick imagemagick-common imagemagick-doc

	do
		echo " "
		echo "#########################################"
		echo "$aget $j"
		$aget $j
		if [ $? -ne 0 ]
		then
			echo "INSTALL FAILED for $j"
			echo "exit 1"
			exit 1
		fi
	done
fi

# echo "debug:  ifoundthem= $ifoundthem   numbertested= $numbertested" 

if [ "$ifoundthem" -lt "$numbertested" ]
then
	echo "tested for $numbertested packages, found $ifoundthem"
	echo "install programs: $notfound  not found.  Install them then run this script again"
	echo " "

	echo "exit 1"
	exit 1
else
	echo " "
	echo "tested for $numbertested packages, found $ifoundthem"
	echo " "
fi



echo " "
echo "Making directories and links"

# $t1       # tetracorder directory
# $sl1      # spectral library directory

for idir in  $t1 $sl1
do
	if [ -d "$idir" ]
	then
		echo "directory $idir found"
	else
		echo "mkdir $idir   # USGS spectrosocpy lab directories"
		mkdir $idir         # USGS spectrosocpy lab directories
			if [ $? -ne 0 ]
			then
				echo "ERROR: command failed"
				echo "exit 1"
				exit 1
			fi
	fi
done

#################  /t1  #############
if [ -L "/t1" ]
then
	echo "/t1 exists and is a symbolic link"
elif [ -d "/t1" ]
then
	echo "/t1 exists and is a directory"
else
	echo "ln -s $t1  /t1"
	ln -s $t1  /t1
		if [ $? -ne 0 ]
		then
			echo "ERROR: command failed"
			echo "exit 1"
			exit 1
		fi
fi

#################  /sl1  #############
if [ -L "/sl1" ]
then
	echo "/sl1 exists and is a symbolic link"
elif [ -d "/sl1" ]
then
	echo "/sl1 exists and is a directory"
else
	echo "ln -s $sl1 /sl1"
	ln -s $sl1 /sl1
		if [ $? -ne 0 ]
		then
			echo "ERROR: command failed"
			echo "exit 1"
			exit 1
		fi
fi

#################  /usr/spool directories  #############
if [ -L "/usr/spool" ]
then
	echo "/usr/spool exists and is a symbolic link"
elif [ -d "/usr/spool" ]
then
	echo "/usr/spool exists and is a directory"
else
	echo "mkdir /usr/spool"
	mkdir /usr/spool
		if [ $? -ne 0 ]
		then
			echo "ERROR: command failed"
			echo "exit 1"
			exit 1
		fi
fi
#################  /usr/spool/gplot directories  #############
if [ -L "/usr/spool/gplot" ]
then
	echo "/usr/spool/gplot exists and is a symbolic link"
elif [ -d "/usr/spool/gplot" ]
then
	echo "/usr/spool/gplot exists and is a directory"
else
	echo "mkdir /usr/spool/gplot"
	mkdir /usr/spool/gplot
		if [ $? -ne 0 ]
		then
			echo "ERROR: command failed"
			echo "exit 1"
			exit 1
		fi
	echo "chmod 777 /usr/spool/gplot"
	chmod 777 /usr/spool/gplot
fi

lspooldirperms=`ls -ld /usr/spool/gplot/ | grep rwxrwxrwx`
if [ -z "$lspooldirperms" ]
then
	echo "chmod 777 /usr/spool/gplot"
	chmod 777 /usr/spool/gplot
fi

if [ ! -f  "/usr/spool/plot.log" ]
then
	echo "cp /dev/null /usr/spool/plot.log"
	cp /dev/null /usr/spool/plot.log
	echo "chmod 666 /usr/spool/plot.log"
	chmod 666 /usr/spool/plot.log
fi

lplotlofperms=`ls -ld /usr/spool/plot.log | grep rwxrwxrwx`
if [ -z "$lplotlofperms" ]
then
	echo "chmod 666 /usr/spool/plot.log"
	chmod 666 /usr/spool/plot.log
fi



echo " "
echo "setting ownership and permissions on spectroscopy directories"
echo " "

for i in  $t1 $sl1 
do
	echo "chown rclark $i"
	chown rclark $i
		if [ $? -ne 0 ]
		then
			echo "ERROR: command failed"
			echo "exit 1"
			exit 1
		fi
	echo "chmod 775 $i"
	chmod 775 $i
		if [ $? -ne 0 ]
		then
			echo "ERROR: command failed"
			echo "exit 1"
			exit 1
		fi
done

echo " "
echo "settung up source code ditrectories"
echo " "

echo " "
if [ -d "$src" ]
then
	echo "$src already exists.  OK."
else
	echo "mkdir $src"
	mkdir $src
	if [ $? -ne 0 ]
	then
		echo "ERROR: command failed"
		echo "exit 1"
		exit 1
	fi
fi

if [ -L "/src" -o -d "/src" ]
then
	echo "/src found.  good."
else
	echo "ln -s $src /src"
	ln -s $src /src
	if [ $? -ne 0 ]
	then
		echo "ERROR: command failed"
		echo "exit 1"
		exit 1
	fi
fi

if [ -d "$src/local" ]
then
	echo "$src/local already exists.  OK."
else
	echo "mkdir $src/local"
	mkdir $src/local
	if [ $? -ne 0 ]
	then
		echo "ERROR: command failed"
		echo "exit 1"
		exit 1
	fi
fi


echo " "
echo "Done with this script"
echo "Spectroscopy system setup complete"
echo " "
