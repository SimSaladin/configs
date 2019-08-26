#!/bin/bash -xe
#-----------------------------------------------------------------------------
# File:          ~/.xinitrc
# Author:        Samuli Thomasson <samuli.thomasson@paivola.fi>
# License:       WTFPL
#-----------------------------------------------------------------------------

# Called by startx (xinit). Logs to <lnk:~/.local/share/xorg/startx.log>.

# {{{1 Systemd user session variables

# Load whatever the global system has defined (includes exporting some variables
# to systemd user session, at least)
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

systemctl --user import-environment \
  PATH PAGER SSH_ASKPASS NIX_PATH NIX_SSL_CERT_FILE NIX_REMOTE

# {{{1 X resources
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# {{{1 Xkb <lnk:~/.config/xkb/README.md>
true || sudo install -vTC -m 0644 -g 0 -o 0 ~/.config/xkb/symbols/dvp /usr/share/X11/xkb/symbols/dvp
# XXX above should be root to be able to install those paths
if ! setxkbmap -verbose 10 -layout dvp -variant intl -option lv3:ralt_switch; then
  # xkbcomp can create the keymaps
  xkbcomp -I"$HOME/.config/xkb" "$HOME/.config/xkb/keymap.xkb" $DISPLAY
fi

# {{{1 Window manager
systemctl --user start xmonad.service
systemctl --user --wait start de.target

# vim:ft=sh:
