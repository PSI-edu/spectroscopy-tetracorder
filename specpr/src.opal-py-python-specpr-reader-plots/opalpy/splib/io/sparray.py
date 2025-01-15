#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# sparray
# package opalpy/splib/io

"""
Created on Mon Sep 19 10:09:33 2022

@author: elivo
"""
""" Data structure to contain SpecPR Library file records into internal Python ListArray """
class SpArray():
    SpListArray = []
    SpRecDict = {}

    # constructor initialization of class variables (& empty for next use)
    def __init__(self):
        'initialize class instance variables as empty'
        self.SpListArray = []  #empty ListArray   #list array element of SpecPR spec-data or text data blocks (using all material file records)
        self.SpRecDict = {}    #empty Dictionary  #Key/Value: specPR file record number key, SpListArray element data-block number value & filename tuple
        # can use SpRecDict key for SpecPR text record numbers