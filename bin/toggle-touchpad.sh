#!/bin/bash
#-----------------------------------------------------------------------------
# File:          toggle-touchpad.sh
# Creation Date: Jan 29 2013 [23:19:51]
# Last Modified: Jan 29 2013 [23:23:52]
# Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
#-----------------------------------------------------------------------------

DEV="SynPS/2 Synaptics TouchPad"

xinput list "$DEV" | grep disabled >/dev/null \
   && (xinput enable "$DEV"; notify-send "$DEV enabled.") \
   || (xinput disable "$DEV"; notify-send "$DEV disabled.")
