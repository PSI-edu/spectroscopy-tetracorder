#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# opalpy_mainmenu
# package opalpy/splib/ui

"""
opalpy main menu

Created on Tue Sep  6 12:34:22 2022
@author: elivo
"""

import sys
from PyQt5 import QtWidgets as qtw
from PyQt5 import QtGui as qtg
from PyQt5 import QtCore as qtc

from splib.io.spio import SpIO
from splib.ui.titlestrlistarray import TitleStrListArray
from splib.util.splibfind import SpLibFind

""" Program Main Dropdown GUI Menu """
class OpalpyMenu(qtw.QMainWindow):

    def __init__(self, parent=None) -> None:
        super().__init__(parent)

        self.setWindowTitle('opalpy Main Menu')
        self.resize(400, 50)
        self.move(0, 0)

        menubar = self.menuBar()

        fileMenu = menubar.addMenu('File')
        #open_action = fileMenu.addAction('Open')
        open_action = qtw.QAction('Open', self)
        open_action.triggered.connect(self.file_menu_open)
        fileMenu.addAction(open_action)

        close_action = fileMenu.addAction('Close')
        fileMenu.addSeparator()
        exit_action = fileMenu.addAction('Exit')
        exit_action.triggered.connect(self.file_menu_exit)
        #exit_action = fileMenu.addAction('Exit', self.destroy)
        #exits main menu (OpalpyMenu), but not opalpy (now without any windows)

        windowsMenu = menubar.addMenu('Windows')
        list_action = windowsMenu.addAction('List')
        plot_action = windowsMenu.addAction('Plot')
        html_action = windowsMenu.addAction('Spec Find')
        html_action.triggered.connect(self.window_menu_spec_find)

        self.show()


    def file_menu_open(self) -> None:
        # Open Spectral data file, read Python (non-numpy) array of SpecPR objects

        spIO = SpIO()             # instance SpIO class
        spArray = spIO.sp_open_and_operate()  # call SpIO instanced method
        spFileName = spIO.getSpLibFileName()

        if not spFileName:  #file operation canceled
            return

        self.titleStrListArray = TitleStrListArray()
        self.titleStrListArray.get_sp_lib_title_str_array(spArray)
        #self.titleStrListArray.get_sp_lib_title_str_array(spLibFileName, spArray)

        
    def file_menu_close(self) -> None:
        pass

    def file_menu_exit(self):
        sys.exit()

    def window_menu_list(self) -> None:
        pass

    def window_menu_plot(self) -> None:
        pass

    def window_menu_spec_find(self) -> None:
        dirPathStr = ""
        ops = ["specfind", "-a", "-i", "kaol"]
        #ops = ["specfind", "-a", "kaol"]
        #ops = ["specfind", "-f", "splib07a"]

        self.spLibFind = SpLibFind()
        matchList = self.spLibFind.ops(dirPathStr, ops)

        #for line in matchList:
        #    print(line, end="")

        self.titleStrListArrayFind = TitleStrListArray()
        self.titleStrListArrayFind.sp_lib_title_str_array(ops, matchList)


        """
        searchStr = "kaol"
        self.strInFile = StrInFile()
        fileList = self.strInFile.listfilesindir(dirPathStr)
        #self.strInFile.matchstr(fileList, searchStr)
        #fileList = [dirPathStr + 'splib07a']
        matchList = self.strInFile.matchstr(dirPathStr, fileList, searchStr)
        for line in matchList:
            print(line, end="")
        dirPathStr = "/home/specpr-database/ALL/"
        searchStr = "kaol"
        self.strInFile = StrInFile()
        fileList = self.strInFile.listfilesindir(dirPathStr)
        #self.strInFile.matchstr(fileList, searchStr)
        #fileList = [dirPathStr + 'splib07a']
        matchList = self.strInFile.matchstr(dirPathStr, fileList, searchStr)
        for line in matchList:
            print(line, end="")
        """


    def init_menu():
        #print('here at OpalpyMenu.init_menu()')
        app = qtw.QApplication(sys.argv)
        win = OpalpyMenu()
        win.show()
        sys.exit(app.exec())


if __name__ == '__main__':
    main = OpalpyMenu
    main.init_menu()
