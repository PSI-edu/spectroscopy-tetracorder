#!/bin/sh

echo "This script is used when the spspecr system has already been installed"
echo "and is being upgraded.  It will also work for a first install if all"
echo "environment variables directories and links are set up correctly"
echo "  All environment variables must be set,"
echo "and the source directory for compiled programs is writable."
echo " "
echo "Press return to begin, or control c to quit"
read a

echo "checking environment variables"

if [ -z "$SPECPR" ] ; then
        echo "\$SPECPR NOT SET.  Exiting"
        exit 1
fi

if [ ! -d "$SPECPR" ] ; then
	echo "directory $SPECPR not found, exiting"
	exit 3
fi

echo "specpr source directory, \$SPECPR = $SPECPR"

if [ -z "$SP_LOCAL" ] ; then
	echo "\$SP_LOCAL NOT SET.  Exiting"
	exit 1
fi
echo "\$SP_LOCAL = $SP_LOCAL"

if [ ! -w "$SP_LOCAL/bin/" ] ; then
        echo "\$SP_LOCAL/bin/ = $SP_LOCAL/bin/ is not writeable, exiting"
        exit 2
fi

for i in \
	SSPPFLAGS	\
	SSPP		\
	CC		\
	F77		\
	SP_OBJ		\
	SP_LIB		\
	RF		\
	SP_RFLAGS	\
	SPSDIR		\
	SP_FFLAGS	\
	SP_FFLAGS1	\
	SP_FFLAGS2	\
	SPKLUDGE	\
	SP_FOPT		\
	SP_FOPT1	\
	SP_FOPT2	\
	SP_CFLAGS	\
	SP_GFLAGS	\
	SP_LDFLAGS	\
	SP_LDLIBS	\
	AR		\
	SP_ARFLAGS	\
	RANLIB
do
	a=`env | grep "^${i}="`
	if [ -z "$a" ] ; then
		echo "\$$i NOT SET.  Exiting"
		exit 1
	fi
	echo "$a"
done

# spectroscopy source code is generally owned by rclark, group = devel.
# cp -a only works for owner, so don't do archive copies if user is not rclark

myuid=`whoami`
cparchiv=" "
if [ "$myuid" = "rclark" ]
then
	cparchiv="-a"
fi

set -x -u

cp $cparchiv  $SPECPR/utility/check_env    $SP_LOCAL/bin/
		if [ $? -ne 0 ]; then
			exit 1
		fi

cd $SPECPR      # probably /src/local/specpr

cd $SPECPR/src.sspp
make
make install

echo "NOTE: if you want a specific logo on plots, edit:"
echo "      $SPECPR/src.specpr/logo/set.logo"
echo "      and recompile specpr"

# cd $SPECPR/src.specpr/logo
# vi set.logo

echo " "
echo "###################   $SPECPR/src.lib ######################"
cd $SPECPR/src.lib
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.specpr ######################"
cd $SPECPR/src.specpr/
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi


echo " "
echo "###################   $SPECPR/src.stamp ######################"
cd $SPECPR/src.sp_stamp
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.spprint ######################"
cd $SPECPR/src.spprint/SRC
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.spsetwave ######################"
cd $SPECPR/src.spsetwave/SRC
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
        
echo " "
echo "###################   $SPECPR/src.spsettitle ######################"
cd $SPECPR/src.spsettitle/SRC
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.spratio2spectra ######################"
cd $SPECPR/src.spratio2spectra/SRC
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

if [ -d "$SPECPR/src.radtran/SRC" ]
then
   echo " "
   echo "###################   $SPECPR/src.radtran ######################"
   cd $SPECPR/src.radtran/SRC
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
fi

echo " "
echo "###################   $SPECPR/src.spfeatures ######################"
cd $SPECPR/src.spfeatures/SRC
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.fstospecpr ######################"
echo "ASD field spectrometer to specpr format conversion"
cd $SPECPR/src.fstospecpr
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.tgifdaemon ######################"
echo "Plotting package for use with tgif"
cd $SPECPR/src.tgifdaemon
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.psplotdaemon ######################"
echo "Plotting package for postscript"
cd $SPECPR/src.psplotdaemon
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

# uncomment if needed

echo " "
echo "###################   $SPECPR/src.asciitosp ######################"
    echo "uncomment asciitosp section if needed"
cd $SPECPR/src.datatran/SRC.asciitosp
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.sptoasci ######################"
    echo "uncomment sptoascii section if needed"
cd $SPECPR/src.datatran/SRC.sptoascii
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.oceanopticstospecpr ######################"
cd $SPECPR/src.oceanopticstospecpr
	make
		if [ "$?" -ne 0 ]; then
			exit 1
		fi
	make install
		if [ "$?" -ne 0 ]; then
			exit 1
		fi

echo " "
echo "###################   $SPECPR/src.cgastosp ######################"
    echo "uncomment cgastosp section if needed"
#cd $SPECPR/src.datatran/SRC.cgastosp
#	make
#	make install

echo " "
echo "###################   $SPECPR/src.hpgl ######################"
    echo "uncomment hpgl section if needed for plotting to HPGL devices"
#cd $SPECPR/src.hpgl
#	make
#	make install

echo " "
echo "###################   $SPECPR/src.hppen ######################"
    echo "uncomment hppen section if needed for plotting to HP pen plotter"
#cd $SPECPR/src.hppen
#	make
#	make install

echo " "
echo "###################   $SPECPR/src.laserjet ######################"
    echo "uncomment laserjet section if needed for plotting to a laserjet printer"
# this directory needs routines from $SPECPR/src.hppen and a makefile
# cd $SPECPR/src.laserjet
#	make
#	make install


for i in specpr dspecpr radtran dradtran spfind sp_spool rule \
	spedit fstospecpr-multiple logtetracorder specprhelp specprnt 
do
		echo "cp $cparchiv  $SPECPR/utility/$i      $SP_LOCAL/bin/"
		cp $cparchiv  $SPECPR/utility/$i      $SP_LOCAL/bin/
		if [ "$?" -ne 0 ]; then
			echo "ERROR copying $SPECPR/utility/$i  to  $SP_LOCAL/bin/"
		fi
done

for i in davinci.print.specpr.to.ascii
do
		echo "cp $cparchiv  $SPECPR/utility-davinci/$i      $SP_LOCAL/bin/"
		cp $cparchiv  $SPECPR/utility-davinci/$i      $SP_LOCAL/bin/
		if [ "$?" -ne 0 ]; then
			echo "ERROR copying $SPECPR/utility-davinci/$i  to  $SP_LOCAL/bin/"
		fi
done

################## manual ##########################


cd $SPECPR/manual
if [ ! -d "/usr/local/man/" ]
then
	if [ -w "/usr/local/man/" ]
	then
		echo "mkdir /usr/local/man/"
		mkdir /usr/local/man/
	else
		echo "ERROR: can not make directory: /usr/local/man/man1 because /usr/local/man/ is not writeable"
	fi
fi	
if [ ! -d "/usr/local/man/man1" ]
then
	echo "mkdir /usr/local/man/man1"
	mkdir /usr/local/man/man1
fi
if [ -w "/usr/local/man/man1" ]
then
	for i in `ls *.1`
	do
		echo "cp -a $i /usr/local/man/man1/"
		cp -a $i /usr/local/man/man1/
	done

	echo "CUSTOMIZATION:"
	echo "NOTE: if you want a specific logo on plots, edit:"
	echo "      $SPECPR/src.specpr/logo/set.logo"
	echo "      and recompile specpr"

else
	echo "ERROR: No or no write permission for /usr/local/man/man1 directory -- can not install man pages"
fi
