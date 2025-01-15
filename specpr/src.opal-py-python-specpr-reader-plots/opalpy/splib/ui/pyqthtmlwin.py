#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# pyqthtmlwin
# package opalpy/splib/ui


"""
HTML GUI Window

Created on Sun Nov  6 11:10:19 2022
@author: elivo
"""

import sys
from PyQt5 import QtWidgets as qtw
from PyQt5 import QtGui as qtg
from PyQt5 import QtCore as qtc

#from splib.util.sparray import SpArray

""" HTML GUI Window (Main Window) """
class PyQtHtmlWin(qtw.QMainWindow):

    """MainWindow Initializer"""
    def __init__(self):
        super().__init__()

        self.main = qtw.QTextBrowser()
        self.setCentralWidget(self.main)

        self.resize(600, 830)
        #self.move(0, 0)

        #htmlText = '<b>Hello World</b> in bold'
        #self.main.insertHtml(htmlText)
        self.show()


    def displayWin(self, spTextTitle, htmlText):
        self.setWindowTitle(spTextTitle)
        self.main.insertHtml(htmlText)
        #self.show()


if __name__ == '__main__':
    app = qtw.QApplication(sys.argv)
    mw = PyQtHtmlWin()
    sys.exit(app.exec())
