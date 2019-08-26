# Directory `~/.config/xkb`

- This directory is used to amend system defaults.
- Use like `xkbcom -I$HOME.config/xkb [...]`
- See `/usr/share/X11/xkb` for expected layout and file types.

## Single-user setup with `keymap.xkb`

Call from `~/.xinitrc`:

```sh
xkbcomp -I$HOME/.config/xkb $HOME/.config/xkb/keymap.xkb $DISPLAY
```

## Multi-user setup

```sh
sudo cp ./symbols/dvp /usr/share/X11/xkb/symbols/dvp
```

Add `setxkbmap -layout dvp -variant dvp,intl` to e.g. `~/.xinitrc`.

### System default

To make this layout the X default, create following file:

```x86conf
# file: /etc/X11/xorg.conf.d/90-custom-kbd.conf

Section "InputClass"
   Identifier      "keyboard defaults"
   MatchIsKeyboard "yes"
   Option          "XkbLayout"  "dvp"
   Option          "XkbVariant" "dvp,intl"
EndSection
```
