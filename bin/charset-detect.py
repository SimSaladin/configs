#!/usr/bin/python2
#-----------------------------------------------------------------------------
# File:          charset-detect.py
# Creation Date: Oct 20 2011
# Last Modified: Nov 09 2012 [20:05:30]
# Created By: Samuli Thomasson [SimSaladin] samuli.thomassonATgmail.com
#-----------------------------------------------------------------------------
#
# detects character encoding of the file
# the file is the first argument

import chardet, sys
rawdata=open(sys.argv[1],"r").read()
print(chardet.detect(rawdata))
