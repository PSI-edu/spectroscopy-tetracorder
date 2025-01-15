#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# specpr
# package opalpy/splib/io

import sys
import numpy as np

"""
Created on Fri Sep  9 09:52:59 2022

@author: elivo
"""

class SpecPR:

    # Specpr variables
    icflag = 0
    ititle = ""
    usernm = ""

    # Data records
    iscta = 0
    isctb = 0
    jdatea = 0
    jdateb = 0
    istb = 0
    isra = 0
    isdec = 0
    itchan = 0
    irmas = 0
    revs = 0

#    iband[2]
    iband0 = 0
    iband1 = 0

    irwav = 0
    irespt = 0
    irecno = 0
    itpntr = 0
    ihist = ""

#    mhist[4]
    mhist0 = ""
    mhist1 = ""
    mhist2 = ""
    mhist3 = ""

    nruns = 0
    siangl = 0
    seangl = 0
    sphase = 0
    iwtrns = 0
    itimch = 0
    xnrm = 0.0
    scatim = 0.0
    timint = 0.0
    tempd = 0.0
    data = np.array([], dtype=float)
    cdata = np.array([], dtype=float)

# Text records
    itxtpt = 0
    itxtch = 0
    itext = ""
    tdata = ""  #continuation itext
    byteArrayText = []

# Class Variables
    filename = ""
    iContCountData = 0
    iContCountText = 0
    specpr_file_record_number = 0
    spSampleByteArray = []


    # constructor initialization of method variables
    def __init__(self):
        self.nameStr = 'OpalPy SpecprRec'
        #self.sp_data_rec.ititle = self.concatenate_n_spaces(40)
        #self.sp_data_rec.usernm = self.concatenate_n_spaces(8)
        #self.sp_data_rec.ihist = self.concatenate_n_spaces(60)

        # zero the Specpr Rec variables
        # Specpr variables
        self.icflag = 0
        self.ititle = ""
        self.usernm = ""

        # Data records
        self.iscta = 0
        self.isctb = 0
        self.jdatea = 0
        self.jdateb = 0
        self.istb = 0
        self.isra = 0
        self.isdec = 0
        self.itchan = 0
        self.irmas = 0
        self.revs = 0

    #    iband[2]
        self.iband0 = 0
        self.iband1 = 0

        self.irwav = 0
        self.irespt = 0
        self.irecno = 0
        self.itpntr = 0
        self.ihist = ""

    #    mhist[4]
        self.mhist0 = ""
        self.mhist1 = ""
        self.mhist2 = ""
        self.mhist3 = ""

        self.nruns = 0
        self.siangl = 0
        self.seangl = 0
        self.sphase = 0
        self.iwtrns = 0
        self.itimch = 0
        self.xnrm = 0.0
        self.scatim = 0.0
        self.timint = 0.0
        self.tempd = 0.0
        self.data = np.array([], dtype=float)
        self.cdata = np.array([], dtype=float)

    # Text records
        self.itxtpt = 0
        self.itxtch = 0
        self.itext = ""
        self.tdata = ""  #continuation itext
        self.byteArrayText = []

    # Class Variables
        self.filename = ""
        self.iContCountData = 0
        self.iContCountText = 0
        self.specpr_file_record_number = 0
        self.spSampleByteArray = []


    ''' specpr record variables '''

    # ***** icflag case 0 - first data record *****
    # 1536 bytes per record (icflag % 4 = 0, with 1st rec data[256])
    #                                       number of bytes
    def sp_data_rec(self, spData0RecTuple):
        #                                             bytes
        self.icflag = spData0RecTuple[0]  # integer   #   4
        self.ititle = str(spData0RecTuple[1].decode('utf-8', errors='ignore')) # characters# 40
        self.usernm = str(spData0RecTuple[2].decode('utf-8', errors='ignore')) # char #   8
        self.iscta =  spData0RecTuple[3]  # int       #   4
        self.isctb =  spData0RecTuple[4]  # int       #   4
        self.jdatea = spData0RecTuple[5]  # int       #   4
        self.jdateb = spData0RecTuple[6]  # int       #   4
        self.istb =   spData0RecTuple[7]  # int       #   4
        self.isra =   spData0RecTuple[8]  # int       #   4
        self.isdec =  spData0RecTuple[9]  # int       #   4
        self.itchan = spData0RecTuple[10] # int       #   4
        self.irmas =  spData0RecTuple[11] # int       #   4
        self.revs =   spData0RecTuple[12] # int       #   4

#       iband[2]        # int[2]       #   8
        self.iband0 = spData0RecTuple[13] # int       #   4
        self.iband1 = spData0RecTuple[14] # int       #   4
        
        self.irwav =  spData0RecTuple[15] # int       #   4
        self.irespt = spData0RecTuple[16] # int       #   4
        self.irecno = spData0RecTuple[17] # int       #   4
        self.itpntr = spData0RecTuple[18] # int       #   4
        self.ihist =  str(spData0RecTuple[19].decode('utf-8', errors='ignore')) # 60 char   #  60

#        mhist[4]        # 74 chars/line (4 lines)#296
        self.mhist0 = str(spData0RecTuple[20].decode('utf-8', errors='ignore')) # 74 char   #  74
        self.mhist1 = str(spData0RecTuple[21].decode('utf-8', errors='ignore')) # 74 char   #  74
        self.mhist2 = str(spData0RecTuple[22].decode('utf-8', errors='ignore')) # 74 char   #  74
        self.mhist3 = str(spData0RecTuple[23].decode('utf-8', errors='ignore')) # 74 char   #  74
        
        self.nruns =  spData0RecTuple[24] # int       #   4
        self.siangl = spData0RecTuple[25] # int       #   4
        self.seangl = spData0RecTuple[26] # int       #   4
        self.sphase = spData0RecTuple[27] # int       #   4
        self.iwtrns = spData0RecTuple[28] # int       #   4
        self.itimch = spData0RecTuple[29] # int       #   4
        self.xnrm =   spData0RecTuple[30] # float     #   4
        self.scatim = spData0RecTuple[31] # float     #   4
        self.timint = spData0RecTuple[32] # float     #   4
        self.tempd =  spData0RecTuple[33] # float     #   4

        #self.data = np.arange(self.itchan, dtype=np.float32)
        self.data = np.zeros((self.itchan), dtype=np.float32)

#        data[256]       # float[256] #1024
        self.iContCountData = 0
        self.iContCountText = 0
        elementMax = 256

        if (self.itchan < elementMax):
            elementMax = self.itchan
        self.data[0:elementMax] = np.array(spData0RecTuple[34:34+elementMax], dtype=float)
        #print("irecno:", self.irecno, " array size:", elementMax, " data:", self.data[elementMax-1])


    # ***** icflag case 1 - data continuation record *****
    # 1536 bytes per record (icflag % 4 = 1, with continuation rec data[383])
    #                                       number of bytes
    def sp_continuation_data_rec(self, spData0RecTuple):
        if(self.iContCountText == 0):  # check for mangled data cont recs
            self.iContCountData = self.iContCountData + 1

            #                                             bytes
            #self.icflag = spData0RecTuple[0]  # integer   #   4
#            cdata[383]       # float[383] #1532
            #self.cdata = np.array(spData0RecTuple[1:384], dtype=float)

            elementMax = (self.iContCountData * 383) + 256
            elementStart = elementMax - 383
            if (self.itchan < elementMax):
                elementMax = self.itchan
            self.data[elementStart:elementMax] = np.array(spData0RecTuple[1:1+(elementMax-elementStart)], dtype=float)
            #print("irecno:", self.irecno, " array size:", elementMax, " data:", self.data[elementMax-1])


    # ***** icflag case 2 - first text record *****
    # 1536 bytes per record (icflag % 4 = 2, with 1st rec text[1476])
    #                                       number of bytes
    def sp_text_rec(self, spText0RecTuple):
        #                                             bytes
        self.icflag = spText0RecTuple[0]  # integer   #   4
        self.ititle = str(spText0RecTuple[1].decode('utf-8', errors='ignore')) # characters# 40
        self.usernm = str(spText0RecTuple[2].decode('utf-8', errors='ignore')) # char #   8
        self.itxtpt = spText0RecTuple[3]  # integer   #   4
        self.itxtch = spText0RecTuple[4]  # integer   #   4
#        itext[1476]    # str[1476]
        iContCountData = 0
        iContCountText = 0
        elementMax = 1476
        if(self.itxtch < elementMax):
            elementMax = self.itxtch

        # Note: Description Pages within spText0RecTuple[5] have HTML formatting (non-utf-8 encoding)
        tmpStr = str(spText0RecTuple[5]) # characters# char 1476
        #tmpStr = str(spText0RecTuple[5].decode()) # characters# char 1476
        #tmpStr = str(spText0RecTuple[5].decode('utf-8', 'replace')) # characters# char 1476
        #tmpStr = str(spText0RecTuple[5].decode('utf-8', 'backslashreplace')) # characters# char 1476
        #tmpStr = str(spText0RecTuple[5].decode('utf-8', errors='ignore')) # characters# char 1476

        self.itext = tmpStr
        #self.itext = tmpStr[0:elementMax]

        print("at SpecPR.sp_text_rec(), array size:", elementMax, " text:", self.itext)
        #print("array size:", elementMax, " text:", tmpStr)


    # ***** icflag case 3 - text continuation record *****
    # 1536 bytes per record (icflag % 4 = 3, with continuation rec text[1532])
    #                                       number of bytes
    def sp_continuation_text_rec(self, spText0RecTuple):
        if(self.iContCountData == 0):  # check for mangled text cont recs
            self.iContCountText = self.iContCountText + 1

            #                                             bytes
            #self.icflag = spText0RecTuple[0]  # integer   #   4
#            tdata[1532]    # str[1532]

            elementMax = (self.iContCountText * 1532) + 1476
            #elementStart = elementMax - 1532
            if(self.itxtch < elementMax):
                elementMax = self.itxtch

            tmpStr = self.itext + str(spText0RecTuple[1]) # characters# char 1532

            self.itext = tmpStr
            #self.itext = tmpStr[:elementMax]

            print("at SpecPR.sp_continuation_text_rec(), array size:", elementMax, " text:", self.itext)
            print("array size:", elementMax, " text:", tmpStr)

        else:
            print("TEXT ERROR - irecno:", self.irecno, " array size:", elementMax, " text:", self.itext)
            #sys.exit()






"""____________________________________________________________
APPENDIX A
SPECPR STANDARD FORMAT OF DATA FILES

SPECtrum Processing Routines
Data Format
3/4/88

***** 1536 bytes per record *****
                                                                  Length
Variable                  Description                             (bytes)
-------------------------------------------------------------------------
icflag          32 one bit flags:                                     4
                (low bit = 0, high bit = 31)
                bit 00 continuation data flag
                        =0 first record of a spectrum consists of:
                                header then 256 data channels
                        =1 continuation data record consisting of:
                                bit flags followed by 1532 bytes of
                                real data (bit 1=0) (383 channels)
                                or 1532 bytes of text (bit 1=1).
                                A maximum of 12 continuation records
                                are allowed for a total of 4852
                                channels (limited by arrays of 4864)
                                or 19860 characters of text (bit 1=1).
                bit 01 text/data flag:
                       =0 the data in the array "data" is data
                       =1 the data in the array "data" is ascii text
                                as is most of the header info.
                bit 02 flag to indicate whether or not the data for
                       the error bar (1 sigma standard deviation of
                       the mean) is in the next record set.
                       =0: no errors, =1: errors in next record set.
                bit 03 RA, Dec / Long., Lat flag:
                       =0 the array "ira" and "idec" corresponds to
                          the right ascension and declination of an
                          astronomical object.
                       =1 the array "ira" and "idec" correspond to
                          the longitude and latitude of a spot on a
                          planetary surface.
                bit 04 variable iscta universal time/civil time flag
                       =0 cta is civil time
                       =1 cta is universal time
                bit 05 variable isctb universal time/civil time flag
                       =0 ctb is civil time
                       =1 ctb is universal time
                bit 06 unused
                bit 07 unused
                bit 08 unused
                bit 09 unused
                bit 10 unused
                bit 11 unused
                bit 12 unused
                bit 13 unused
                bit 14 unused
                bit 15 unused
                bit 16 unused
                bit 17 unused
                bit 18 unused
                bit 19 unused
                bit 20 unused
                bit 21 unused
                bit 22 unused
                bit 23 unused
                bit 24 unused
                bit 25 unused
                bit 26 unused
                bit 27 unused
                bit 28 unused
                bit 29 unused
                bit 30 unused
                bit 31 unused
                bit 32 unused

*************** case 1: bit 00 = 0, bit 01 = 0 ****************

icflag          Bit flags: 32 bits, see above.                        4

ititl           40 character title which describes the               40
                data (ascii, character*40).

usernm          The name of the user that created the data record     8
                (ascii, character*8).

iscta           Civil or Universal time when data was                 4
                last processed (integer*4 in scaled seconds).
                Scaled by 24000.  See flag #04.

isctb           Civil or Universal time at the start of               4
                the spectral run (integer*4 in scaled seconds).
                Scaled by 24000. See flag #05.

jdatea          Date when data was last processed                     4
                Stored as integer*4 Julian Day number *10

jdateb          Date when the spectral run began                      4
                Stored as integer*4 Julian Day number *10

istb            Sidereal time when the spectral run started.          4
                (integer*4 in scaled seconds).
                Scaled by 24000. See flag #05.

isra            Right ascension coordinates of an astronomical        4
                object, or longitude on a planetary surface
                (integer*4 numbers in seconds *1000)
                (RA in RA seconds, Longitude in arc-seconds)
                See flag #06.

isdec           Declination coordinates of an astronomical            4
                object, or latitude on a planetary
                surface (integer*4 number in arc-seconds *1000).
                See flag #06.

itchan          Total number of channels in the spectrum              4
                (integer*4 value from 1 to 4852)

irmas           The equivalent atmospheric thickness through          4
                which the observation was obtained (=1.0
                overhead scaled: airmass*1000; integer*4).

revs            The number of independent spectral scans              4
                which were added to make the spectrum
                (integer*4 number).

iband(2)        The channel numbers which define the band             8
                normalization (scaling to unity). (integers*4)

irwav           The record number within the file where the           4
                wavelengths are found (integer*4).

irespt          The record pointer to where the resolution can        4
                be found (or horizontal error bar) (integer*4).

irecno          The record number within the file where the           4
                data is located (integer*4 number).

itpntr          Text data record pointer. This pointer points         4
                to a data record where additional text describing
                the data may be found.  (32 bit integer)

ihist           The program automatic 60 character history.          60
                (ascii, character*60)

mhist           Manual history (4 lines of 74 characters            296
                each.  Program automatic for large history
                requirements (ascii, character*296).

nruns           The number of independent spectral runs               4
                which were summed or averaged to make this
                spectrum (integer*4).

siangl          The angle of incidence of illuminating                4
                radiation (Integer*4 number, in arc-seconds*6000).
                (90 degrees=1944000000; -90 deg <= angle <= 90 deg)
                integrating sphere = 2000000000
                Geometric albedo   = 2000000001

seangl          The angle of emission of illuminating                 4
                radiation (Integer*4 number, in arc-seconds*6000).
                (90 degrees=1944000000; -90 deg <= angle <= 90 deg)
                integrating sphere = 2000000000
                Geometric albedo   = 2000000001

sphase          The phase angle between iangl and eangl               4
                (Integer*4 number, in arc-seconds*1500).
                (180 degrees=972000000; -180 deg <= phase <= 180 deg)
                integrating sphere = 2000000000

iwtrns          Weighted number of runs (the number of runs           4
                of the spectrum with the minimum runs which was
                used in processing this spectrum, integer*4).

itimch          The time observed in the sample beam for              4
                each half chop in milliseconds (for chopping
                spectrometers only). (integer*4)

xnrm            The band normalization factor. For data scaled        4
                to 1.0, multiply by this number to recover
                photometric level (32 bit real number).

scatim          The time it takes to make one scan of the             4
                entire spectrum in seconds (32 bit real number).

timint          Total integration time (usually=scatime * nruns)      4
                (32 bit real number).

tempd           Temperature in degrees Kelvin                         4
                (32 bit real number).

data(256)       The spectral data (256 channels of 32 bit          1024
                real data numbers).
-------------------------------------------------------------------------
        case 1: total (bytes):                                     1536
=========================================================================

*************** case 2: bit 00 = 1, bit 01 = 0 ****************

icflag          Flags: see case 1                                     4

cdata(383)      The continuation of the data values (383 channels  1532
                of 32 bit real numbers).
-------------------------------------------------------------------------
        case 2: total (bytes):                                     1536
=========================================================================

*************** case 3: bit 00 = 0, bit 01 = 1 ****************


icflag          Flags: see case 1                                     4

ititle          40 character title which describes the               40
                data (ascii, character*40).

usernm          The name of the user who created the data record      8
                (ascii, character*8).

itxtpt          Text data record pointer. This pointer points         4
                to a data record where additional text may be
                may be found.  (32 bit integer)

itxtch          The number of text characters (maximum= 19860).       4

itext           1476 characters of text.  Text has embedded        1476
                newlines so the number of lines available is
                limited only by the number of characters available.
-------------------------------------------------------------------------
        case 3: total (bytes):                                     1536
=========================================================================

*************** case 4: bit 00 = 1, bit 01 = 1 ******************

icflag          Flags: see case 1                                     4

tdata           1532 characters of text.                           1532
-------------------------------------------------------------------------
        case 4: total (bytes):                                     1536
=========================================================================
"""
