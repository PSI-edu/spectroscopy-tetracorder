#!/usr/bin/env python
# -*- coding: utf-8 -*-

# opalpy
# package opalpy

"""
Spyder Editor

This is the main script file.

@author: elivo
"""

import sys
from splib.ui import opalpy_mainmenu as mm
#import splib.ui.opalpy_mainmenu

"""
dependencies:
    HTML SpecPR Description Pages:
        tkhtmlview (https://pypi.org/project/tkhtmlview/ - python3 -m pip install tkhtmlview)
            Python 3.4 or later with tcl/tk support
            Pillow 5.3.0 (https://github.com/python-pillow/Pillow - python3 -m pip install Pillow)
            requests (python3 -m pip install requests)
"""

""" non-parameterized main class: display and analysis of spectroscopy data """
class OpalPy:

    # constructor initialization of class variables
    def __init__(self) -> None:
        self.nameStr = 'OpalPy Main'

    # print intialization string
    def print_name(self) -> None:
        print(self.nameStr)

    """Start of program (opalpy) - default Main method"""
    def main(self) -> None:
        args = sys.argv[1:]  #command line arg list; skipping script name at argv[0]
        if len(args) > 0:
            for element in range(0, len(args)):
                print (args[element])

        # print value of __name__ context
        #print("the value of __name__ context is:", repr(__name__))
        #   for module (file) execution: name context is __main__
        #   for method import (file opalpy.py): name context is opalpy

        self.print_name()

        # call main menu gui module
        opalpyMenu = mm.OpalpyMenu
        opalpyMenu.init_menu()


# start program opalpy main method when this module is executed
if __name__ == "__main__":
    # creating object of the class & run method main
    opalPyObj = OpalPy()
    opalPyObj.main()


# ------------------- Example ----------------------------
#""" parameterized class constructor example (not used)"""
#class OpalPyPara:
#    
#    # constructor initialization of class variables
#    def __init__(self, nStr):
#        self.nameParaStr = nStr
#        
#    # print intialization string
#    def print_name_para(self):
#        print(self.nameParaStr)
