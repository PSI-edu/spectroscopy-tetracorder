#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# titlestrlistarray
# package opalpy/splib/ui

"""
#package opalpy/splib/ui

Created on Thu Sep 22 19:33:01 2022

@author: elivo
"""

from PyQt5 import QtWidgets as qtw
from PyQt5 import QtGui as qtg
from PyQt5 import QtCore as qtc

from splib.io.sparray import SpArray
from splib.ui.pyqtmatplotlib import PyQtMatplotlib
from splib.ui.pyqthtmlwin import PyQtHtmlWin
from splib.util.splibfind import SpLibFind
from splib.io.spio import SpIO

""" Generate and display string list of Specra Titles using a list GUI """
class TitleStrListArray(qtw.QMainWindow):

    def __init__(self, parent = None) -> None:
        super().__init__(parent)
        self.spArray = None

        self.pyQtMatplotlib = None
        self.resize(400, 750)
        self.move(0, 80)

        self.specListWidget = qtw.QListWidget()
        self.specListWidget.currentRowChanged.connect(self.row_changed)
        #self.specListWidget.currentItemChanged.connect(self.index_changed)
        #self.specListWidget.currentItemChanged.connect(self.text_changed)
        self.setCentralWidget(self.specListWidget)
        self.show()


    def get_sp_lib_title_str_array(self, spArray: SpArray) -> None:
    #def get_sp_lib_title_str_array(self, mainMenuHandle, spLibFileName: str, spArray: SpArray) -> None:
        self.spArray = spArray
        titleStrList = []

        for SpecPRFirstRec in range (0, len(self.spArray.spListArray)):  #SpecPR rec 0 is magic number
            #print(self.spArray.spListArray[SpecPRFirstRec].irecno, self.spArray.spListArray[SpecPRFirstRec].ititle)
            
            #listStr = str(self.spArray.spListArray[SpecPRFirstRec].specpr_file_record_number) \
            #  + "  " + self.spArray.spListArray[SpecPRFirstRec].ititle
            #  + "  " + len(self.spArray.spListArray[SpecPRFirstRec].byteArrayText)
            spRecNumStr = str(self.spArray.spListArray[SpecPRFirstRec].specpr_file_record_number)
            spTitle = self.spArray.spListArray[SpecPRFirstRec].ititle
            spBytesDataorText = str(len(self.spArray.spListArray[SpecPRFirstRec].byteArrayText))

            spReflFlag = self.spArray.spListArray[SpecPRFirstRec].icflag
            if ((spReflFlag % 4) == 0):
                spBytesDataorText = str(len(self.spArray.spListArray[SpecPRFirstRec].data))
            else:
                spBytesDataorText = str(len(self.spArray.spListArray[SpecPRFirstRec].byteArrayText))
            listStr = spRecNumStr + "  " + spTitle + "  " + spBytesDataorText

            #titleStrList.append(self.spArray.spListArray[SpecPRFirstRec].ititle)
            titleStrList.append(listStr)
        self.specListWidget.addItems(titleStrList)


    def sp_lib_title_str_array(self, ops, matchList):
        self.spArray = None
        self.matchList = matchList
        self.specListWidget.addItems(self.matchList)


    def row_changed(self, spListArrayIndex):
        print(spListArrayIndex)

        if (self.spArray):
            spReflFlag = self.spArray.spListArray[spListArrayIndex].icflag

            if ((spReflFlag % 4) == 0):  # SpecPR data

                spFileName = self.spArray.spListArray[spListArrayIndex].filename
                spReflTitle = self.spArray.spListArray[spListArrayIndex].ititle

                # assign spectrum data
                spReflRec = self.spArray.spListArray[spListArrayIndex].specpr_file_record_number
                spRefl = self.spArray.spListArray[spListArrayIndex].data
                self.rmDelPtstoZero(spRefl)  #replace SpecPR del pt with -0.05

                # read wavelengths used by SpecPR record spReflRec
                spWaveRec = self.spArray.spListArray[spListArrayIndex].irwav
                spWaves = self.spArray.spListArray[self.spArray.SpRecDict[spWaveRec][0]].data
                self.rmDelPtstoZero(spWaves)  #replace SpecPR del pt with -0.05

                #print(spWaves[0], spWaves[200], spWaves[440])
                #print(spReflRec, self.spArray.SpRecDict[spReflRec], spWaveRec)

                # spReflRec = spArray.spListArray[spListArrayIndex].irecno  #Spec-Data Only (no irecno in Text)
                # print("icflag:", spReflFlag, "irecno:", spReflRec, "specpr_file_record_number:", spFileRec, "ititle:", spReflTitle, "\n")

                #if (self.pyQtMatplotlib is None):
                #    self.pyQtMatplotlib = PyQtMatplotlib()
                self.pyQtMatplotlib = PyQtMatplotlib(spFileName)
                self.pyQtMatplotlib.update_plot(spReflTitle, spRefl, spWaves)

            if ((spReflFlag % 4) == 2):  # SpecPR text

                spTextTitle = self.spArray.spListArray[spListArrayIndex].ititle

                # assign spectrum text
                #text rec has no irecno variable
                spFileTextRec = self.spArray.spListArray[spListArrayIndex].specpr_file_record_number

                spSampleByteArray = self.spArray.spListArray[spListArrayIndex].spSampleByteArray
                htmlText = self.spArray.spListArray[spListArrayIndex].byteArrayText.decode('ascii')
                print("byteArrayText Length:", len(self.spArray.spListArray[spListArrayIndex].byteArrayText))

                self.pyQtHtmlWin = PyQtHtmlWin()
                self.pyQtHtmlWin.displayWin(spTextTitle, htmlText)

        else:
            basePathStr = ""
            #basePathStr = "/specpr-database"

            #print(self.matchList[spListArrayIndex])
            # 22
            # splib07a:   6077  Kaolinite KGa-2 (pxl)         BECKb AREF   480  08:29:44.00  01/29/1986

            spFileName = self.matchList[spListArrayIndex][0:8]
            spRecData= self.matchList[spListArrayIndex][10:17]

            args = ["specfind", "-f", spFileName]
            spLibFind = SpLibFind()
            pathList = spLibFind.ops(basePathStr, args)  # list of found SpecPR librarys with full path & name
            if (pathList):
                spPath = pathList[0].rstrip('\n')  # remove trailing newline
                #print("PathList:", spPath)  # PathList: /d1/speclib/splib06a
                #print(spFileName, spRecData)    # splib06a  11917

                # read data and waves from SpecPR file  (single spectrum)
                spIO = SpIO()             # instance SpIO class
                spArray = spIO.sp_open_and_operate_single_data_spectrum(spPath, spRecData)  # call SpIO instanced method

                spRecWaves = spArray.spListArray[0].irwav
                spArrayWaves = spIO.sp_open_and_operate_single_data_spectrum(spPath, spRecWaves)  # call SpIO instanced method

                if ((spArray.spListArray) and (spArrayWaves.spListArray)):
                    #print(spArray.spListArray[0].ititle, "\n" + spArrayWaves.spListArray[0].ititle)
                    #print(spArray.spListArray[spListArrayIndex].ititle)

                    spFileName = spArray.spListArray[0].filename
                    spReflTitle = spArray.spListArray[0].ititle

                    # assign spectrum data
                    spRefl = spArray.spListArray[0].data
                    self.rmDelPtstoZero(spRefl)  #replace SpecPR del pt with -0.05

                    # read wavelengths used by SpecPR record spReflRec
                    spWaves = spArrayWaves.spListArray[0].data
                    self.rmDelPtstoZero(spWaves)  #replace SpecPR del pt with -0.05

                    self.pyQtMatplotlib = PyQtMatplotlib(spFileName)
                    self.pyQtMatplotlib.update_plot(spReflTitle, spRefl, spWaves)


    def index_changed(self, i):  # Not an index, i is a QListItem
        print(i.text())


    def text_changed(self, s):  # s is a str
        print(s)

    
    def rmDelPtstoZero(selp, spData):
        for ich in range(len(spData)):
            if(spData[ich] < -10): spData[ich] = -0.05
