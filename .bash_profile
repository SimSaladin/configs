#!/bin/bash
# file: ~/.bash_profile
# Read by login shells

# {{{1 source .bashrc even if we're non-interactive
[[ -f ~/.bashrc ]] && . ~/.bashrc

# {{{1 start graphical if this is tty n. 1
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
   exec startx > ~/.local/share/xorg/startx.log 2>&1
fi
