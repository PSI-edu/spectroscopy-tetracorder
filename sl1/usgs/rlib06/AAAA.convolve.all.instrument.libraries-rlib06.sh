#!/bin/sh

# To convolve a library after the specpr restart and startup files are created,
# run the command:
# 
# 
#      mak.convol.library  library version channels name     resol_number
# 
# 
# example:
# 
#      mak.convol.library  r06av95  a        224   AVIRIS95 12
# 
# creates spectral library r06av95a
# Each convolution takes about 6.2 minutes on speclab1 Feb 2008 for 352 vims channels
#                        about 4.5 minutes for aviris
#      on fast x-terminals.
# 
# Research lib on cassini server: 27 second to comvolve 04/2012
# 
# starting specpr:
# 
#      dspecpr restartfiles/r1.sprlb06b -gxterm2 -
# 
# 
# ***************************************************************
# ***************************************************************
# ***********                           *************************
# ***********    convolution commands   *************************
# ***********                           *************************
# ***************************************************************
# ***************************************************************

# Note: the sleep can be 0, 0.1 or 0.2  or greater to delay the plot long enough to
#       see the result without taking too long of a time to complete.
#       Example: 0.1 takes about 25 seconds 3/2022 148 spectra on an I9 computer
#                0.2 takes  40 seconds
# 
#       Change sleep to noX for no X-window plots (and then the seconds is not needed.


# Commands:

#                prog              lib     n  chan  title    fwhm
#                                                   keyword  rec number


# AVIRIS:
            ./mak.convol.library  r06av95  a  224   AVIRIS95 12  sleep 0.1 # aviris 1995
            ./mak.convol.library  r06av97  a  224   AVIRIS97 12  sleep 0.1 # aviris 1997
            ./mak.convol.library  r06av98  a  224   AVIRIS98 12  sleep 0.1 # aviris 1998
            ./mak.convol.library  r06av99  a  224   AVIRIS99 12  sleep 0.1 # aviris 1999
            ./mak.convol.library  r06av00  a  224   AVIRIS00 12  sleep 0.1 # aviris 2000
            ./mak.convol.library  r06av01  a  224   AVIRIS01 12  sleep 0.1 # aviris 2001
            ./mak.convol.library  r06av05  a  224   AVIRIS10 12  sleep 0.1 # aviris 2005
            ./mak.convol.library  r06av06  a  224   AVIRIS10 12  sleep 0.1 # aviris 2006
            ./mak.convol.library  r06av08  a  224   AVIRIS08 12  sleep 0.1 # aviris 2008a
            ./mak.convol.library  r06av08  b  224   AVIRIS08 12  sleep 0.1 # aviris 2008b
            ./mak.convol.library  r06av10  a  224   AVIRIS10 12  sleep 0.1 # aviris 2010
            ./mak.convol.library  r06av11  a  224   AVIRIS10 12  sleep 0.1 # aviris 2011
            ./mak.convol.library  r06av12  a  224   AVIRIS12 12  sleep 0.1 # aviris 2012
            ./mak.convol.library  r06av13  a  224   AVIRIS13 12  sleep 0.1 # aviris 2013
            ./mak.convol.library  r06av14  a  224   AVIRIS14 12  sleep 0.1 # aviris 2014
            ./mak.convol.library  r06av17  a  224   AVIRIS17 12  sleep 0.1 # aviris 2017
            ./mak.convol.library  r06av18  a  224   AVIRIS18 12  sleep 0.1 # aviris 2018

# AVIRIS 3
            ./mak.convol.library  r06a325  a  284   AVIRIS3-25 12  sleep 0.1  # AVIRIS 3 2025

# AVIRIS-NG:
            ./mak.convol.library  r06an14  a  426   AVIRNG14 12  sleep 0.1 # aviris-ng 2014
            ./mak.convol.library  r06an14  b  425   AVIRNG14 12  sleep 0.1 # aviris-ng 2014
            ./mak.convol.library  r06an21  a  425   AVIRNG21 12  sleep 0.1  # aviris-ng 2021

# EMIT:
            ./mak.convol.library  r06emit  a  288   EMIT22a  12  sleep 0.1  # EMIT before launch to Space Station
            ./mak.convol.library  r06emit  c  285   EMIT22c  12  sleep 0.1  # EMIT 022-08-13 in orbit wavelengths
 

# VIMS:
            ./mak.convol.library  r06vm07  a  352   VIMS07   12  sleep 0.1

# CRISM:
            ./mak.convol.library  r06cr08  a  438   CRISM08a 12  sleep 0.1
            ./mak.convol.library  r06cr08  b   72   CRISM08a 12  sleep 0.1  # global mode
            ./mak.convol.library  r06crj3  a  489   CRISMj3a 12  sleep 0.1  # 2012 J_MTR3 merged V+IR version a

# Moon Mineralogy Mapper:

            ./mak.convol.library  r06mm07  a  260   MMM      12  sleep 0.1 # 2007 targeted waves
            ./mak.convol.library  r06mm07  c   86   MMM      12  sleep 0.1 # 2007 global waves
            ./mak.convol.library  r06mm07  s  260   MMM      12  sleep 0.1 # 2007 synthetic test 1 waves
            ./mak.convol.library  r06mm09  c   85   MMM      12  sleep 0.1 # 2009 85 channel global waves
            ./mak.convol.library  r06mm10  t  255   MMM255t  12  sleep 0.1 # 2010 255 channel targeted waves

# NIRS3 on Hayabusa2 to the asteroid Ryugu, 2018+ mission

            ./mak.convol.library  r06hyb2  a   76   HYB2ryug 12  sleep 0.1 # Hayabusa2 to the asteroid Ryugu


# PRISMA 2021 ESA Earth orbiter, e.g. s06pr01a 238 channels:

            ./mak.convol.library   r06pr01  a 238  PRISMA21a 12  sleep 0.1 # PRISMA 2021

# ASD Field spectrometer, standard waves and resolution, 2151 channels:

           ./mak.convol.library  r06fs00   a 2151  ASDfs00a  12  sleep 0.1

# FTIR instruments:

           ./mak.convol.library  r06ag21   a  900  AGFTIRa   12  sleep 0.1

# Resonon Pika IR line-scan hyperspectral camera

           ./mak.convol.library  r06pkir   a 168   PIKAIRa   12  sleep 0.1   # Resonon Pika IR 0.9 - 1.7 microns


