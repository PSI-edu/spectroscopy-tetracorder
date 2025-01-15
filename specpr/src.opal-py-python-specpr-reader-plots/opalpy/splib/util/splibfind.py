#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# splibfind
# package opalpy/splib/util

"""
#package opalpy/splib/util

Created on Sun Nov 27 18:35:45 2022

@author: elivo
"""

import sys
import os

from splib.io.spio import SpIO
#from splib.ui.titlestrlistarray import TitleStrListArray

class SpLibFind:
    global basePathStr
    global searchALLDir

    def __init__(self):
        super().__init__()
        self.basePathStr = "/specpr-database"
        #self.basePathStr = "/specpr-database" + "/"
        self.searchALLDir = "ALL"  # appended later with: os.path.join(path, filename)

        self.matchList = []


    def ops(self, pathStr, args):
        activeArg = args[1]

        if (pathStr == ""):
            pass
        else:
            self.basePathStr = pathStr

        if (activeArg == "-a"):    # search all libraries
            activeArg = args[2]
            caseSensitivity = True
            if (activeArg == "-i"):
                caseSensitivity = False
            searchStr = args[len(args) - 1]
            dirPathStr = os.path.join(self.basePathStr, self.searchALLDir)
            #dirPathStr = self.basePathStr + self.searchALLDir

            fileList = self.listfilesindir(dirPathStr)
            self.matchList = self.matchstr(dirPathStr, fileList, searchStr, caseSensitivity)

            """
            # Open Spectral data file, read Python (non-numpy) array of SpecPR objects
            spIO = SpIO()             # instance SpIO class
            spArray = spIO.sp_open_and_operate_single_data_spectrum(spLibFileName, spRec)  # call SpIO instanced method
            #spArray = spIO.sp_open_and_operate()  # call SpIO instanced method

            #self.titleStrListArray = TitleStrListArray()
            #self.titleStrListArray.get_sp_lib_title_str_array(self, spLibFileName, spArray)
            """

        elif (activeArg == "-i"):  # search case sensitivity
            pass

        elif (activeArg == "-f"):  # find library path (location)
            searchStr = args[len(args) - 1]
            spLibsDir = "specpr.files"
            fullname = os.path.join(self.basePathStr, spLibsDir)
            self.matchList = []

            fileObj = open(fullname, 'r')
            for index, line in enumerate(fileObj):
                if searchStr.lower() in line.lower():
                    self.matchList.append(line)
            

        else:
            print("default")

        #for line in self.matchList:
        #    print(line, end="")

        return(self.matchList)


    def listfilesindir(self, dirPathStr: str) -> list:
        #dir_path =r'data'
        fileList = []

        for path in os.listdir(dirPathStr):
            # check if current path is a file
            #if os.path.isfile(path):
            if os.path.isfile(os.path.join(dirPathStr, path)):
                fileList.append(path)
                #fileList.append(dirPathStr + path)
        print(fileList)
        return(fileList)


    def matchstr(self, dirPathStr: str, fileList: list, searchStr: str, caseSensitivity: bool) -> None:
        matchList = []
        for fileName in fileList:
            fullname = os.path.join(dirPathStr, fileName)
            fileObj = open(fullname, 'r')
            #fileObj = open(dirPathStr + "/" + fileName, 'r')
            # iterate through file, index is count i (0 start), line is record text string
            for index, line in enumerate(fileObj):
                line = line.rstrip()
                if (caseSensitivity):
                    if searchStr in line:
                        matchList.append(fileName + ": " + line)
                else:
                    if searchStr.lower() in line.lower():
                        matchList.append(fileName + ": " + line)
        return(matchList)



        """
        #self.strInFile = StrInFile()
        #fileList = self.strInFile.listfilesindir(dirPathStr)
        #matchList = self.strInFile.matchstr(dirPathStr, fileList, searchStr, caseSensitivity)
        

        fullfilename = self.basePathStr + self.searchALLDir + "/" + "splib07a"
        path = os.path.dirname(fullfilename)
        basepath = os.path.basename(path)
        print("fullfilename:", fullfilename)
        print("path:", path)
        print("basepath:", basepath)
        # Results:
        #fullfilename: /home/specpr-database/ALL/splib07a
        #path: /home/specpr-database/ALL
        #basepath: ALL


        # match / case requires Pythod ver. 3.10 & up
        match opStr:
            case "-a":
                pass
            case "-i":
                pass
            case "-f":
                pass
            case _:
                print("default")
        """