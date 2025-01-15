#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# pyqtmatplotlib
# package opalpy/splib/ui

"""
Created on Thu Nov 10 18:19:52 2022

@author: elivo
"""

import sys
from PyQt5 import QtWidgets as qtw
from PyQt5 import QtGui as qtg
from PyQt5 import QtCore as qtc
from PyQt5.QtWidgets import QDialog, QApplication, QPushButton, QVBoxLayout

from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt5agg import NavigationToolbar2QT as NavigationToolbar
import matplotlib.pyplot as plt

#import random


class PyQtMatplotlib(QDialog):
    #def __init__(self, parent=None):
    def __init__(self, spLibFile, parent=None):
        super(PyQtMatplotlib, self).__init__(parent)

        # a figure instance to plot on
        self.figure = plt.figure()

        # this is the Canvas Widget that displays the 'figure'
        # it takes the 'figure' intance as a parameter to __init__
        self.canvas = FigureCanvas(self.figure)

        # this is the Navigation Widget
        # it takes the Canas Widget and a parent
        self.toolbar = NavigationToolbar(self.canvas, self)

        # just some button connected to 'plot' method
        #self.button = QPushButton('Plot')
        #self.button.clicked.connect(self.plot)

        # set the layout
        layout = QVBoxLayout()
        layout.addWidget(self.toolbar)
        layout.addWidget(self.canvas)
        #layout.addWidget(self.button)
        self.setLayout(layout)

        # create an axis
        self.ax = self.figure.add_subplot(111)
        self.ax.set_xlabel("Wavelength (micrometers)")
        self.ax.set_ylabel("% Reflectance")

        # reference object to line-plot
        self._plot_ref = None
        #self.update_plot()

        self.setWindowTitle(spLibFile)
        self.show()

    '''
    # OBSOLETE: not used
    def plot(self, spReflTitle, spRefl, spWaves):
        #self.setWindowTitle(spLibFileName)

        # instead of ax.hold(False)
        self.figure.clear()

        # create an axis
        ax = self.figure.add_subplot(111)

        ax.set_title(spReflTitle)
        ax.set_xlabel("Wavelength (micronmeters)")
        ax.set_ylabel("% Reflectance")

        # discards the old graph
        # ax.hold(False) # deprecated

        # plot data
        #ax.plot(data, '*-')
        line1, = ax.plot(spWaves, spRefl)  #plot line object

        # refresh canvas
        self.canvas.draw()
    '''

    def update_plot(self, spReflTitle, spRefl, spWaves):
        self.ax.set_title(spReflTitle)
        #print ('self._plot_ref is None: ', (self._plot_ref is None))

        plot_refs = self.ax.plot(spWaves, spRefl, 'k')
        self._plot_ref = plot_refs[0]  # plot_refs[0] is list of line plots

        self.canvas.draw()

    def dropEvent(self, e):
        '''
        This function will enabe the drop file directly on to the
        main window.  The file location will be stored in the self.filename
        '''
        if e.mimeData().hasUrls:
            e.setDropAction(qtc.Qt.CopyAction)
            e.accept()
            for url in e.mimeData().urls():
                if url.op_sys == 'Darwin':
                    fname = str(url.NSURL.URLWithString_(str(url.toString())).filePathURL().path())
                else:
                    fname = str(url.toLocalFile())
            self.filename = fname
            print("GOT ADDRESS:",self.filename)
            self.readData()
        else:
            e.ignore()  # just like  above functions


if __name__ == '__main__':
    app = QApplication(sys.argv)

    main = PyQtMatplotlib()
    main.show()

    sys.exit(app.exec_())
