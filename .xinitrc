#!/bin/bash
# shellcheck disable=SC1090

for xinit in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -r "$xinit" ] && . "$xinit"
done
unset xinit

systemctl --user import-environment PATH PAGER SSH_ASKPASS NIX_PATH NIX_SSL_CERT_FILE NIX_REMOTE CM_LAUNCHER

if [ -r ~/.Xresources ]; then
  xrdb -merge ~/.Xresources
fi

# <url:file:~/.config/xkb/README.md> xkbcomp can create the keymaps if xorg keymap isn't installed
# XXX sudo install -vTC -m 0644 -g 0 -o 0 ~/.config/xkb/symbols/dvp /usr/share/X11/xkb/symbols/dvp

#if ! setxkbmap -verbose 10 -layout dvp -variant intl -option lv3:ralt_switch; then
#  xkbcomp -I ~/.config/xkb ~/.config/xkb/keymap.xkb "$DISPLAY"
#fi

systemctl --user start --wait graphical.target
