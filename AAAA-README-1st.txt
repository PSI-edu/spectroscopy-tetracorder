
The Tetracorder and spectroscopy system runs on linux.

The following packages will be needed.  Commands are based in Ubuntu/mint:

apt-get -y install java-common   # base package for java runtimes
                                 # should already be installed

# for compiling java:
apt-get -y install  default-jdk  default-jdk-doc # for compiling java

# simple audio player
apt-get -y aplay

# gcc         # C compiler
# g++         # needed to compile davinci
apt-get -y install  gcc  g++

# gfortran    # fortran compiler
# ratfor      # rational fortran
apt-get -y install gfortran ratfor  gfortran-doc

# make
apt-get -y install make

# davinci image processing:
#                           http://davinci.asu.edu/
#     download davinci source code from above.  
#     To compile, you also need these packages:
#
# note on compiling davinci 2.10+ on linux mint 17+:
# edit Makefile (after the ./configure)
# add to the line: LIBS = -lpng -lz -lm -lX11  (may have other entries)
# and add -ljbig:
#              LIBS = -lpng -lz -lm -lX11 -ljbig


# not in mint19: apt-get -y install libpng12-dev
apt-get -y install libpng-dev
apt-get -y install libjbig-dev:amd64 libjbig0:amd64 libjbig0:i386 libjbig2dec0 libjbig2dec0-dev
apt-get -y install jbig2dec jbigkit-bin
apt-get -y install libjpeg8-dev
apt-get -y install zlib1g zlib1g-dev zlib1g:i386

# to compile some X11 programs (e.g. specpr, spectral processing,
#     and tetracorder, imaging spectroscopy analysis) you'll need this:
apt-get -y install libx11-dev

# for developers:
apt-get -y install glibc-doc glibc-doc-reference

# inotifywait monitors a directory for new files.
# this is needed for processes that need to monitor incoming data,
# e.g. security systems,  science data to be analyzed (spectra coming in).
# for example, this is used to monitor real time spectroscopy data being
# fed to tetracorder, e.g. from a rover.
apt-get -y install  inotify-tools

# imagemagick pbm    # image processing tools
apt-get -y install imagemagick imagemagick-common imagemagick-doc
apt-get -y install netpbm

apt-get -y install libxpm-dev
apt-get -y install libxt-dev
apt-get -y install libpng-dev
apt-get -y install libjbig-dev:amd64 libjbig0:amd64 libjbig0:i386 libjbig2dec0 libjbig2dec0-dev
apt-get -y install jbig2dec jbigkit-bin
apt-get -y install libjpeg8-dev
apt-get -y install zlib1g zlib1g-dev zlib1g:i386



#################################################################################

# do the things in:

AAAA.README-spectroscopy-setup


#################################################################################

download  davinci image processing:
                          http://davinci.asu.edu/
compile and install

#################################################################################

Set up environment variables.  Environment variable definitions are in the
etc directory for bash and csh (tcsh).  Edit the system files:
    /etc/bash.bashrc
    /etc/csh.cshrc
and add the environment variables for the shells that users use.

#################################################################################

Copy the source code  from github into the source directories:

/src/local/specpr
/src/local/tetracorder5.26


#################################################################################

Run:
     env --version

If the version is 8.30 or newer:

       Copy scripts for tetracorder:

       cp  -s /t1/tetracorder.cmds/tetracorder5.26e.cmds/davinci-cmds.for.usr.local.bin/*  /usr/local/bin

otherwise:

       cp  -s /t1/tetracorder.cmds/tetracorder5.26e.cmds/davinci-cmds.for.usr.local.bin-pre-env8.30/*  /usr/local/bin


If you use vi (vim) and want syntax in color when editing davinci scripts or
the  tetracorder expert system:

      cd /t1/tetracorder.cmds/tetracorder5.26e.cmds/vim/

       and follow the AAA.readme.txt on how to add the definitions to the vi system.

#################################################################################

Making the specpr and tetracoder packages:

The specpr package must be made before tetracorder, as tetracorder uses many subroutines
in the specpr package.

Specpr:
         cd /src/local/specpr

         ./AAA.INSTALL.specpr+support-progs-linux-upgrade.1.7.sh |& tee ./AAA.INSTALL.specpr+support-progs-linux-upgrade.1.7.sh.out.txt

Check for errors in the ./AAA.INSTALL.specpr+support-progs-linux-upgrade.1.7.sh.out.txt file.

         cd /src/local/tetracorder5.26

         Follow the instructions in AAA.readme.how.to.make.txt

Setup is now complete.

#################################################################################

Watch the training videos at:

      https://www.youtube.com/user/PSITucson/featured

   Go to: Spectroscopy and Tetracorder Training

      The youtube left scroll shows only the firrst 12 videos.  To see all videos,
      click on the PLAY ALL button and the full list of videos is on the right.



