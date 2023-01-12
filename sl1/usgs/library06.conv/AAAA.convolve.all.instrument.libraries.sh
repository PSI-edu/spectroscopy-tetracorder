#!/bin/sh

# To convolve a library after the specpr restart and startup files are created,
# run the command:
# 
# 
#      ./mak.convol.library  library version channels name     resol_number
# 
# 
# example:
# 
#      ./mak.convol.library  s06av95  a        224   AVIRIS95 12
# 
# creates spectral library s06av95a
# This takes several minutes.
# 
# ***************************************************************
# ***************************************************************
# ***********                           *************************
# ***********    convolution commands   *************************
# ***********                           *************************
# ***************************************************************
# ***************************************************************
# 
# Commands:
# 
# #                prog              lib     n  chan  title    fwhm
#                                                   keyword  rec number

# AVIRIS:
            ./mak.convol.library  s06av95  a  224   AVIRIS95 12  # aviris 1995
            ./mak.convol.library  s06av97  a  224   AVIRIS97 12  # aviris 1997
            ./mak.convol.library  s06av98  a  224   AVIRIS98 12  # aviris 1998
            ./mak.convol.library  s06av99  a  224   AVIRIS99 12  # aviris 1999
            ./mak.convol.library  s06av00  a  224   AVIRIS00 12  # aviris 2000
            ./mak.convol.library  s06av01  a  224   AVIRIS01 12  # aviris 2001
            ./mak.convol.library  s06av05  a  224   AVIRIS05 12  # aviris 2005
            ./mak.convol.library  s06av06  a  224   AVIRIS06 12  # aviris 2006
            ./mak.convol.library  s06av10  a  224   AVIRIS10 12  # aviris 2010
            ./mak.convol.library  s06av11  a  224   AVIRIS11 12  # aviris 2011
            ./mak.convol.library  s06av12  a  224   AVIRIS12 12  # aviris 2012
            ./mak.convol.library  s06av13  a  224   AVIRIS13 12  # aviris 2013
            ./mak.convol.library  s06av17  a  224   AVIRIS17 12  # aviris 2017
            ./mak.convol.library  s06av18  a  224   AVIRIS17 12  # aviris 2018

# AVIRIS-NG:
            ./mak.convol.library  s06an14  a  426   AVIRNG14 12  # aviris-ng 2014
            ./mak.convol.library  s06an14  b  425   AVIRNG14 12  # aviris-ng 2014
            ./mak.convol.library  s06an21  a  425   AVIRNG14 12  # aviris-ng 2014

# EMIT:
            ./mak.convol.library  s06emit  a  288   EMIT22a  12  # EMIT before launch to Space Station
            ./mak.convol.library  s06emit  c  285   EMIT22c  12  # EMIT 022-08-13 in orbit wavelengths


# VIMS:
            ./mak.convol.library  s06vm07  a  352   VIMS07   12

# HYMAP:
            ./mak.convol.library  s06hy09  a  125   HYM09ja  12  Hymap Jan 30 2009
            ./mak.convol.library  s06hy09  b  125   HYM09f1b 12  Hymap feb 1 2009

# CRISM:
            ./mak.convol.library  s06cr08  a  438   CRISM08a 12   # 2008 IR only
            ./mak.convol.library  s06cr08  b   72   CRISM08b 12   # 2008 waves global tile mode
            ./mak.convol.library  s06cr11  v  107   CRISM11v 12   # 2011 VIS only
            ./mak.convol.library  s06crj3  a  489   CRISMj3a 12   # 2012 J_MTR3 merged V+IR version a
                                                                  # note: this took 9 minutes running
                                                                  # remote X over DSL (on cassini)

# Moon Mineralogy Mapper:

            ./mak.convol.library  s06mm07  a  260   MMM      12  # 2007 targeted waves
            ./mak.convol.library  s06mm07  c   86   MMM      12  # 2007 global waves
            ./mak.convol.library  s06mm07  s  260   MMM      12  # 2007 synthetic test 1 waves

            ./mak.convol.library  s06mm09  a  260   MMM      12  # 2009 targeted waves
            ./mak.convol.library  s06mm09  c   85   MMM      12  # 2009 global waves


# PRISMA ESA Earthorbiter:
            ./mak.convol.library  s06pr01  a  238   PRISMA1a 12  # 2021 PRISMA


# ASD Field spectrometer, standard waves and resolution, 2151 channels:

           ./mak.convol.library  s06fs00   a 2151  ASDfs00a  12  


# Generic set:   0.366 to 2.495
            ./mak.convol.library s06gn20   a 106   GN20nma   12  # 20nm bandpasses
            ./mak.convol.library s06gn30   a  71   GN30nma   12  # 30nm bandpasses
            ./mak.convol.library s06gn40   a  53   GN40nma   12  # 40nm bandpasses
            ./mak.convol.library s06gn50   a  42   GN50nma   12  # 50nm bandpasses

# Generic set b:   0.2961 to 3.0012
            ./mak.convol.library s06gn07   b 387   GN07nmb   12  # 07nm bandpasses
            ./mak.convol.library s06gn10   b 271   GN10nmb   12  # 10nm bandpasses
            ./mak.convol.library s06gn20   b 136   GN20nmb   12  # 20nm bandpasses
            ./mak.convol.library s06gn30   b  91   GN30nmb   12  # 30nm bandpasses
            ./mak.convol.library s06gn40   b  68   GN40nmb   12  # 40nm bandpasses
            ./mak.convol.library s06gn50   b  55   GN50nmb   12  # 50nm bandpasses


# Agilent FTIR:
            ./mak.convol.library s06ag21   a 900   AGFTIRa   12  # 8 cm^1 bandpasses

