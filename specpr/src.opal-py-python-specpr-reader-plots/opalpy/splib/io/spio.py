#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# spio
# package opalpy/splib/io

"""
Created on Tue Sep  6 21:38:34 2022

@author: elivo
"""

import sys
import numpy as np
import struct
from pathlib import Path
from collections import namedtuple
from PyQt5 import QtWidgets as qtw
from PyQt5 import QtGui as qtg
from PyQt5 import QtCore as qtc
#from tkinter import *
#from tkinter import filedialog

from splib.io.specpr import SpecPR
from splib.io.sparray import SpArray

""" SpecPR Class with various Input/output, and helper methods """
class SpIO:

    #global spLibFileName

    def __init__(self):
        'instance initializer of class variables'
        #open spectral library file
        self.spLibFileName = None
        #self.sp_open_and_operate()


    def sp_open_and_operate(self):
        '''tarnsfer SpecPR library data into internal python data structure list'''

        """ select spectral library file """
        spFileName = self.sp_select_lib()
        #test = type(spFileName)
        #print("string length: ", test, len(spFileName))
        if(spFileName =="()"):
            spFileName = None
            #print("empty selection cancelled, parenthesis pair returned", spFileName)
            #return
        elif(spFileName == ""):
            spFileName = None
            #print("selection cancelled, double quote pair returned", spFileName)
            #return

        if not spFileName:
            return

        self.putSpLibFileName(spFileName)
        #self.spLibFileName = spFileName

        """ choose file operations """
        selectedOpStr = self.sp_file_op()  #TO BE implimented
        #selectedOpStr is to list file in GUI Window

        """ read full SpecPR file into spLibByteArray (all records) """
        spLibByteArray = self.get_sp_bytearray(self.getSpLibFileName())
        #print("spLibByteArray length:", len(spLibByteArray))

        spArray = SpArray()
        spListArray = spArray.SpListArray  #empty list (spListArray link to spArray.SpListArray)
        spRecDict = spArray.SpRecDict      #empty dictionary  (spRecDict link to spArray.SpRecDict)

        #print("spArray isinstance of SpArray", isinstance(spArray, SpArray))
        #print("length of spListArray:", len(spListArray))
        #print("lengths: spListArray & spRecDict", len(spListArray), len(spRecDict))

        """ analyze every sample bytedata Data or Text Tuple  (1536 bytes per file record) """
        for spRec in range(len(spLibByteArray)//1536):

            # *** get SpecPR Record variables tuple from record ByteArray ***

            spByteOffsetStart = spRec * 1536

            #subset Byte Array - one SpecPR record
            spRecByteArray = spLibByteArray[spByteOffsetStart+0:spByteOffsetStart+1536]

            #read record type for SpecPR Data or Text sample (series of records)
            spRecTuple = struct.unpack('>i', spRecByteArray[0:4])
            icflag = spRecTuple[0]  #IEEE byte (SpecPR record flag)
            recType = icflag % 4

            #create SpecPR Record tuple  (data or Text)
            if (recType >= 0) and (recType <= 3):  # icflag % 4 = 0 (data) or 2 (text) start rec
            #update value          #create Tuple Variables from data bytearray record
                if (recType == 0):
                    spRecTuple = self.get_sp_data_rec_tuple(spRecByteArray)

                #create Tuple Variables from data continuation bytearray record
                if (recType == 1):
                    spRecTuple = self.get_sp_cont_data_rec_tuple(spRecByteArray)

                #create Tuple Variables from text bytearray record
                if (recType == 2):
                    spRecTuple = self.get_sp_text_rec_tuple(spRecByteArray)

                #create Tuple Variables from text continuation bytearray record
                if (recType == 3):
                    spRecTuple = self.get_sp_cont_text_rec_tuple(spRecByteArray)

            """ create SpecPR Record Variables  (data or Text) """
            spRecDict[spRec] = ((len(spListArray) - 1), self.getSpLibFileName())  # spRecDict[spRec] for SpecPR header record 0 = -1
            #print("spRec:", spRec, "spRecDict[spRec]:", spRecDict[spRec])

            if (recType >= 0) and (recType <= 3):  # icflag % 4 = 0 (data) or 2 (text) start rec
                #Initialize SpecPR Variables from data bytearray record
                if (recType == 0):
                    sp = self.sp_data_rec(spRecTuple, spRec, spRecByteArray)
                    spListArray.append(sp)
                    spRecDict[spRec] = ((len(spListArray) - 1), self.getSpLibFileName())  #update value

                #Initialize SpecPR Variables from data continuation bytearray record
                if (recType == 1):
                    self.sp_continuation_data_rec(spRecTuple, spListArray, spRecDict, spRec, spRecByteArray)
                    #print("spRec:", spRec, "spRecDict[spRec]:", spRecDict[spRec])

                #Initialize SpecPR Variables from text bytearray record
                if (recType == 2):
                    sp = self.sp_text_rec(spRecTuple, spRec, spRecByteArray)  # itext needs work
                    spListArray.append(sp)
                    spRecDict[spRec] = ((len(spListArray) - 1), self.getSpLibFileName())  #update value
                    #print(spRec, ":", spRecDict[spRec], spRecDict[spRec(0)], sp.ititle)

                
                #Initialize SpecPR Variables from text continuation bytearray record
                if (recType == 3):
                    #tpl = spRecDict[spRec]
                    #listCurrentDataRec = tpl[0]
                    listCurrentDataRec = spRecDict[spRec][0]
                    if(listCurrentDataRec >= 0):
                        self.sp_continuation_text_rec(spRecTuple, spListArray, spRecDict, spRec, spRecByteArray)

        spArray.spListArray = spListArray
        spArray.spRecDict = spRecDict

        return spArray


    def sp_open_and_operate_single_data_spectrum(self, spLib, spRec):
        '''tarnsfer SpecPR library data into internal python data structure list'''

        spArray = SpArray()
        spListArray = spArray.SpListArray  #empty list (spListArray link to spArray.SpListArray)
        spRecDict = spArray.SpRecDict      #empty dictionary  (spRecDict link to spArray.SpRecDict)

        # read single data spectrum
        spLibByteArraySingleSpectrum = self.get_sp_bytearray_single_spectrum(spLib, spRec)
        #spLibByteArrayFirstRec = spLibByteArraySingleSpectrum[0:1536]

        for spDataRec in range(len(spLibByteArraySingleSpectrum)//1536):
            spByteOffsetStart = spDataRec * 1536

            #subset Byte Array - one SpecPR record
            spRecByteArray = spLibByteArraySingleSpectrum[spByteOffsetStart+0:spByteOffsetStart+1536]

            if(spDataRec == 0):

                spRecTuple = self.get_sp_data_rec_tuple(spRecByteArray)

                sp = self.sp_data_rec(spRecTuple, spDataRec, spRecByteArray)
                spListArray.append(sp)
                spRecDict[spRec] = ((len(spListArray) - 1), spLib)  #update value
                
            else:
                
                spRecTuple = self.get_sp_cont_data_rec_tuple(spRecByteArray)
                #self.sp_continuation_data_rec(spRecTuple, spListArray, spRecDict, spDataRec, spRecByteArray)

        spArray.spListArray = spListArray
        spArray.spRecDict = spRecDict

        return spArray


    def sp_select_lib(self):
        # select spectral library file
        #root = Tk()
        #root.title("Open File")
        #root.filename = filedialog.askopenfilename(initialdir="/src/local/spyder/projects/opalpy",
        #filename = filedialog.askopenfilename(initialdir="/src/local/spyder/projects/opalpy",
        #  title="Select Spectral Library File", 
        #  filetypes=(("all files", "*"), ("png files", "*.png")) )

        filename, _ = qtw.QFileDialog.getOpenFileName()
        # without ", _" filename is: ('selected-path-n-filename', 'All Files (*)')
        print('filename selected: ', filename)
        
        #(self, 'Select Spectral Library File',
        #  '/src/local/spyder/projects/opalpy', 'all files ("*")')

        # filename returned as string
        #cancel clicked: nothing selected: unicode, file selected: empty tuple
        spLibSelected = str(filename)

        #spLibSelected = root.filename
        #print(spLibSelected)
        #    spLibSelected = "splib07a"
        #root.destroy()
        return spLibSelected


    def putSpLibFileName(self, spFileName):
        self.spLibFileName = spFileName


    def getSpLibFileName(self):
        # spLibFileName is: None, when uninitialized - if not spLibFileName:  #true
        # spLibFileName is full name including path; ie. /src/local/spyder/projects/opalpy/splib07a
        return self.spLibFileName


    def sp_file_op(self):
        # choose file operations
        return "opList"


    def get_sp_bytearray(self, spLib):
        # read full SpecPR file into Byte Array (all records)
        spLibByteArray = self.read_binfile(spLib)
        return spLibByteArray


    def get_sp_bytearray_single_spectrum(self, spLib, spRecArg):
        spRec = int(spRecArg)

        # read single spectrum from SpecPR file into Byte Array
        spLibByteArraySingleSpectrum = self.read_binfile_rec(spLib, spRec)
        if (spLibByteArraySingleSpectrum):

            #read record type for SpecPR Data or Text sample (series of records)
            spRecTuple = struct.unpack('>i', spLibByteArraySingleSpectrum[0:4])
            icflag = spRecTuple[0]  #IEEE byte (SpecPR record flag)
            recType = icflag % 4

            numContRecs = 0

            #create Tuple Variables from data bytearray record
            if (recType == 0):
                spRecTuple = self.get_sp_data_rec_tuple(spLibByteArraySingleSpectrum)
                itchan = spRecTuple[10]

                if (itchan > 256):
                    numContChans = itchan - 256
                    numContRecs = int((numContChans - 1) / 383) + 1

                for spRecCont in range(numContRecs):
                    # read single spectrum from SpecPR file into Byte Array
                    spReci = spRec + spRecCont + 1
                    spLibByteArrayi = self.read_binfile_rec(spLib, spReci)
                    spLibByteArraySingleSpectrum = spLibByteArraySingleSpectrum + spLibByteArrayi

                #print("NRecs & ByteArray length", numContRecs + 1, len(spLibByteArraySingleSpectrum))

        return spLibByteArraySingleSpectrum


    def get_sp_data_rec_tuple(self, spRecByteArray):
        spRecTuple = struct.unpack('>i40s8s16i60s74s74s74s74s6i260f', spRecByteArray)
        return spRecTuple


    def get_sp_cont_data_rec_tuple(self, spRecByteArray):
        spRecTuple = struct.unpack('>i383f', spRecByteArray)
        return spRecTuple


    def get_sp_text_rec_tuple(self, spRecByteArray):
        spRecTuple = struct.unpack('>i40s8s2i1476s', spRecByteArray)
        return spRecTuple


    def get_sp_cont_text_rec_tuple(self, spRecByteArray):
        spRecTuple = struct.unpack('>i1532s', spRecByteArray)
        return spRecTuple

    '''
    def get_sp_data_rec(self, spRecTuple):
        sp.sp_data_rec(spDataRecTuple)
        sp = SpecPR()  #instance for first data/text record only


    def get_sp_cont_data_rec(self, spRecTuple):
        


    def get_sp_text_rec(self, spRecTuple):
        


    def get_sp_cont_text_rec(self, spRecTuple):
    '''      





    '''
    def sp_lib_read(self, spLib):
        # read spectral library file

        # read full SpecPR file into Byte Array (all records)
        #dataByteArraySplib = self.read_binfile(spLib)

        # Python (non-numpy) array of SpecPR objects
        spListArray = []

        # create full spListArray listfile (list of SP objects)
          #to do

        #calculate File Offset for one SpecPR record
        #Test spRec = 24205

        spNumRec = 0  #set spNumRec as int
        spByteArrayLen = len(dataByteArraySplib)    #splib07a: 37,192,704 bytes
        spNumRec = spByteArrayLen // 1536           #splib07a: 24,214 recs

        maxLines = 0
        for spRec in range(spNumRec):
            spByteOffsetStart = spRec * 1536

            #subset Byte Array - one SpecPR record
            spRecByteArray = dataByteArraySplib[spByteOffsetStart+0:spByteOffsetStart+1536]

            #create (SpecPR) icflag single value Tuple from byte array record
            spRecTuple = struct.unpack('>i', spRecByteArray[0:4])

            icflag = spRecTuple[0]
            recType = icflag % 4

            if (recType >= 0) or (recType <= 3):  # icflag % 4 = 0 (data) or 2 (text) start rec

                #print(spRecTuple)

                #TEST-good sp = splib.util.specpr.SpecprRec

                if (recType == 0):  #recType equals 0 - starting data record

                    # parse Specpr record variables from tuple
                    sp = SpecPR()  #instance for first data/text record only

                    #create Tuple Variables from byte array record
                    spDataRecTuple = struct.unpack('>i40s8s16i60s74s74s74s74s6i260f', spRecByteArray)

                    sp.sp_data_rec(spDataRecTuple)
                    spListArray.append(sp)
                    maxLines = maxLines + 1  #(len(spListArray))
                    print(maxLines-1, spRec, spListArray[maxLines-1].icflag, spListArray[maxLines-1].ititle, spListArray[maxLines-1].itchan, spListArray[maxLines-1].itxtch)

                elif (recType == 1):  #recType equals 1 - continuation data record

                    #create Tuple Variables from byte array record
                    spDataRecTuple = struct.unpack('>i383f', spRecByteArray)

                    elementCur = len(spListArray) - 1
                    if(elementCur >= 0):
                        spListArray[elementCur].sp_continuation_data_rec(spDataRecTuple)
                        #sp.sp_continuation_data_rec(spDataRecTuple)

                elif (recType == 2):  #recType equals 2 - starting text record

                    # parse Specpr record variables from tuple
                    sp = SpecPR()  #instance for first data/text record only

                    #create Tuple Variables from byte array record
                    spTextRecTuple = struct.unpack('>i40s8s2i1476s', spRecByteArray)
                    #print(spTextRecTuple)

                    sp.sp_text_rec(spTextRecTuple)
                    spListArray.append(sp)
                    maxLines = maxLines + 1  #(len(spListArray))
                    print(maxLines-1, spRec, spListArray[maxLines-1].icflag, spListArray[maxLines-1].ititle, spListArray[maxLines-1].itchan, spListArray[maxLines-1].itxtch)

                elif (recType == 3):  #recType equals 3 - continuation text record

                    #create Tuple Variables from byte array record
                    spDataRecTuple = struct.unpack('>i1532s', spRecByteArray)

                    elementCur = len(spListArray) - 1
                    if(elementCur >= 0):
                        x = 1
                        spListArray[elementCur].sp_continuation_text_rec(spDataRecTuple)

        #print(maxLines-1, spRec)  #last record number (start or continuation) rec
        #spRec = 24205
        elementSp = 5739

        #spli07a maxLines = 5743  #number of index records, same as: len(spListArray)

        elementSp = 5739  #spRec = 24205
        print()
        print(spListArray[elementSp].icflag)
        print(spListArray[elementSp].ititle)
        print(spListArray[elementSp].usernm)
        print(spListArray[elementSp].iscta)
        print("spListArray index No:", elementSp)
        print("irecno:", spListArray[elementSp].irecno, "irwav:", spListArray[elementSp].irwav, "irespt:", spListArray[elementSp].irespt)
        print(spListArray[elementSp].data[0], spListArray[elementSp].data[4], spListArray[elementSp].data[2150])

        return spListArray
    '''
# ----
    """
        # TEST - read one record of SpecPR file into Byte Array
        #   WORKS GREAT

        TESTspRec = 24205
        TESTspRecByteArray = self.read_binfile_rec(self.spLib, TESTspRec)

        #create Tuple Variables from byte array record
        TESTspRecTuple = struct.unpack('>i40s8s16i60s74s74s74s74s6i260f', TESTspRecByteArray)
        print(TESTspRecTuple)

        tFlag = TESTspRecTuple[0]
        tItitle = str(TESTspRecTuple[1].decode('utf-8'))
        tData = TESTspRecTuple[289]
        print(tFlag)
        print(tItitle)
        print(tData)
    """

    ''' read binary file - all records '''
    def read_binfile(self, specLib):
        #print("reading: ", specLib, "\n")

        #dataByteArray = Path("splib07a").read_bytes()
        bfile = open(specLib, "rb")
        dataByteArray = bfile.read(-1)
        bfile.close()

        #print("file read done")
        return dataByteArray


    ''' read binary file - single record '''
    def read_binfile_rec(self, specLib: str, spRecArg: int):
        spRec = int(spRecArg)
        #print("reading: ", specLib, "SpecPR Record:", spRec)

        #dataByteArray = Path("splib07a").read_bytes()
        spByteOffsetStart = spRec * 1536
        bfile = open(specLib, "rb")
        bfile.seek(spByteOffsetStart)
        dataByteArray = bfile.read(1536)
        bfile.close()

        #print("file record read done")
        return dataByteArray


    ''' specpr record variables '''

    # ***** icflag case 0 - first data record *****
    # 1536 bytes per record (icflag % 4 = 0, with 1st rec data[256])
    #                                       number of bytes
    def sp_data_rec(self, spData0RecTuple, spRec, spRecByteArray):
        sp = SpecPR()  #instance for first data/text record only
        #                                             bytes
        sp.icflag = spData0RecTuple[0]  # integer   #   4
        sp.ititle = str(spData0RecTuple[1].decode('utf-8', errors='ignore')) # characters# 40
        sp.usernm = str(spData0RecTuple[2].decode('utf-8', errors='ignore')) # char #   8
        sp.iscta =  spData0RecTuple[3]  # int       #   4
        sp.isctb =  spData0RecTuple[4]  # int       #   4
        sp.jdatea = spData0RecTuple[5]  # int       #   4
        sp.jdateb = spData0RecTuple[6]  # int       #   4
        sp.istb =   spData0RecTuple[7]  # int       #   4
        sp.isra =   spData0RecTuple[8]  # int       #   4
        sp.isdec =  spData0RecTuple[9]  # int       #   4
        sp.itchan = spData0RecTuple[10] # int       #   4
        sp.irmas =  spData0RecTuple[11] # int       #   4
        sp.revs =   spData0RecTuple[12] # int       #   4

#       iband[2]        # int[2]       #   8
        sp.iband0 = spData0RecTuple[13] # int       #   4
        sp.iband1 = spData0RecTuple[14] # int       #   4
        
        sp.irwav =  spData0RecTuple[15] # int       #   4
        sp.irespt = spData0RecTuple[16] # int       #   4
        sp.irecno = spData0RecTuple[17] # int       #   4
        sp.itpntr = spData0RecTuple[18] # int       #   4
        sp.ihist =  str(spData0RecTuple[19].decode('utf-8', errors='ignore')) # 60 char   #  60

#        mhist[4]        # 74 chars/line (4 lines)#296
        sp.mhist0 = str(spData0RecTuple[20].decode('utf-8', errors='ignore')) # 74 char   #  74
        sp.mhist1 = str(spData0RecTuple[21].decode('utf-8', errors='ignore')) # 74 char   #  74
        sp.mhist2 = str(spData0RecTuple[22].decode('utf-8', errors='ignore')) # 74 char   #  74
        sp.mhist3 = str(spData0RecTuple[23].decode('utf-8', errors='ignore')) # 74 char   #  74
        
        sp.nruns =  spData0RecTuple[24] # int       #   4
        sp.siangl = spData0RecTuple[25] # int       #   4
        sp.seangl = spData0RecTuple[26] # int       #   4
        sp.sphase = spData0RecTuple[27] # int       #   4
        sp.iwtrns = spData0RecTuple[28] # int       #   4
        sp.itimch = spData0RecTuple[29] # int       #   4
        sp.xnrm =   spData0RecTuple[30] # float     #   4
        sp.scatim = spData0RecTuple[31] # float     #   4
        sp.timint = spData0RecTuple[32] # float     #   4
        sp.tempd =  spData0RecTuple[33] # float     #   4

        #sp.data = np.arange(sp.itchan, dtype=np.float32)
        sp.data = np.zeros((sp.itchan), dtype=np.float32)

#        data[256]       # float[256] #1024
        elementMax = 256

        if (sp.itchan < elementMax):
            elementMax = sp.itchan
        sp.data[0:elementMax] = np.array(spData0RecTuple[34:34+elementMax], dtype=float)
        #print("irecno:", sp.irecno, " array size:", elementMax, " data:", sp.data[elementMax-1])
        #print("ititle:", sp.ititle)

        sp.spSampleByteArray = spRecByteArray  # full SpecPR sample (all file recs) in byte array format
        sp.specpr_file_record_number = spRec  # specpr file: start record number for this sp object
        sp.filename = self.getSpLibFileName()  # Specpr filename for this sample

        return sp


    # ***** icflag case 1 - data continuation record *****
    # 1536 bytes per record (icflag % 4 = 1, with continuation rec data[383])
    #                                       number of bytes
    def sp_continuation_data_rec(self, spData0RecTuple, spListArray, spRecDict, spRec, spRecByteArray):
        spCurrentDataFirstRec = spRec
        listCurrentDataRec = spRecDict[spRec][0]
        #listCurrentDataRec = spRecDict[spRec]
        #listCurrentDataRec = spArray.spRecDict[spRec][0]
        #find starting (first) data record
        while(listCurrentDataRec == spRecDict[spCurrentDataFirstRec][0]):
            spCurrentDataFirstRec = spCurrentDataFirstRec - 1
        spCurrentDataFirstRec = spCurrentDataFirstRec + 1
        numContRecs = spRec - spCurrentDataFirstRec

        #                                             bytes
        #self.icflag = spData0RecTuple[0]  # integer   #   4
        #        cdata[383]       # float[383] #1532
        #self.cdata = np.array(spData0RecTuple[1:384], dtype=float)

        elementMax = (numContRecs * 383) + 256
        elementStart = elementMax - 383
        if (spListArray[listCurrentDataRec].itchan < elementMax):
            elementMax = spListArray[listCurrentDataRec].itchan

        #print(spCurrentDataFirstRec, spRec, listCurrentDataRec, elementStart, elementMax)
        spListArray[listCurrentDataRec].data[elementStart:elementMax] = np.array(spData0RecTuple[1:1+(elementMax-elementStart)], dtype=float)
        #spListArray[listCurrentDataRec].data[elementStart:elementMax].append(np.array(spData0RecTuple[1:1+(elementMax-elementStart)], dtype=float))
        spListArray[listCurrentDataRec].spSampleByteArray += spRecByteArray

        #return


    #Needs corrected for decode str length increase of itext
    # ***** icflag case 2 - first text record *****
    # 1536 bytes per record (icflag % 4 = 2, with 1st rec text[1476])
    #                                       number of bytes
    def sp_text_rec(self, spText0RecTuple, spRec, spRecByteArray):
        sp = SpecPR()  #instance for first data/text record only
        #                                             bytes
        sp.icflag = spText0RecTuple[0]  # integer   #   4
        sp.ititle = str(spText0RecTuple[1].decode('utf-8', errors='ignore')) # characters# 40
        sp.usernm = str(spText0RecTuple[2].decode('utf-8', errors='ignore')) # char #   8
        sp.itxtpt = spText0RecTuple[3]  # integer   #   4
        sp.itxtch = spText0RecTuple[4]  # integer   #   4
        sp.itext = str(spText0RecTuple[5].decode('utf-8', errors='ignore')) # char #   8

        elementMax = 1476      # (max number of text elements total)
        if(sp.itxtch < elementMax):
            elementMax = sp.itxtch

        firstTextElement = 60  # (zero based)
        lastTextElement = elementMax + firstTextElement  # last text position (+1) within 1536 byte array

        spTextRecByteArray = spRecByteArray[firstTextElement:lastTextElement]
        sp.byteArrayText = spTextRecByteArray  # itext in byte array format
        sp.spSampleByteArray = spRecByteArray
        sp.specpr_file_record_number = spRec  # specpr file: start record number for this sp object
        sp.filename = self.getSpLibFileName()  # Specpr filename for this sample

        # Note: Description Pages within spText0RecTuple[5] have HTML formatting (non-utf-8 encoding)
        #tmpStr = str(spText0RecTuple[5]) # characters# char 1476
        #tmpStr = str(spText0RecTuple[5].decode()) # characters# char 1476
        #tmpStr = str(spText0RecTuple[5].decode('utf-8', 'replace')) # characters# char 1476
        #tmpStr = str(spText0RecTuple[5].decode('utf-8', 'backslashreplace')) # characters# char 1476
        #tmpStr = str(spText0RecTuple[5].decode('utf-8', errors='ignore')) # characters# char 1476

        #sp.itext = tmpStr
        #sp.itext = tmpStr[0:elementMax]

        #print("at SpecPR.sp_text_rec(), array size:", elementMax, " text:", sp.itext)
        #print("array size:", elementMax, " text:", tmpStr)

        return sp


    #Needs corrected for decode str length increase
    # ***** icflag case 3 - text continuation record *****
    # 1536 bytes per record (icflag % 4 = 3, with continuation rec text[1532])
    #                                       number of bytes
    def sp_continuation_text_rec(self, spText0RecTuple, spListArray, spRecDict, spRec, spRecByteArray):
        #                                             bytes
        #self.icflag = spText0RecTuple[0]  # integer   #   4
        #        tdata[1532]    # str[1532]

        spCurrentDataFirstRec = spRec
        listCurrentDataRec = spRecDict[spRec][0]
        #find starting (first) text record
        while(listCurrentDataRec == spRecDict[spCurrentDataFirstRec][0]):
            spCurrentDataFirstRec = spCurrentDataFirstRec - 1
        spCurrentDataFirstRec = spCurrentDataFirstRec + 1
        numContRecs = spRec - spCurrentDataFirstRec

        #spArray.spListArray[listCurrentDataRec].spSampleByteArray.append(spRecByteArray)
        #spListArray[listCurrentDataRec].spSampleByteArray += spRecByteArray
        #spListArray[listCurrentDataRec].spSampleByteArray += spRecByteArray

        firstTextElement = 4  # (zero based)
        contTextElementMax = 1532
        elementMax = (numContRecs * 1532) + 1476  # (max number of text elements total)

        if(spListArray[listCurrentDataRec].itxtch < elementMax):
            contTextElementMax = 1532 - (elementMax - spListArray[listCurrentDataRec].itxtch)

        lastTextElement = contTextElementMax + firstTextElement  # last text position (+1) within 1536 byte array

        spContTextRecByteArray = spRecByteArray[firstTextElement:lastTextElement]
        spListArray[listCurrentDataRec].byteArrayText += spContTextRecByteArray  # itext in byte array format
        spListArray[listCurrentDataRec].spSampleByteArray += spRecByteArray






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
