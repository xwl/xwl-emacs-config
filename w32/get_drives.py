#!/usr/bin/python

## Author: William Xu

## Get drive information,  return a list of (drive letter . drive name), which
## could be read directly in elisp.  For example: 
#   '(("C" . "XP") ("D" . "software"))

import win32api
import pywintypes
import sys

sys.stdout.write("(")
for drive in win32api.GetLogicalDriveStrings().strip("\000").split("\000"): 
    try: 
        sys.stdout.write("(\"" + drive[0] + "\" . \"" 
                         + win32api.GetVolumeInformation(drive)[0] + "\")")
    except (pywintypes.error): 
        pass
print (")")
