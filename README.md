# dotfiles (`~/`)

## git worktrees and git-crypt

```sh
git config --worktree --unset filter.git-crypt.clean
git config --worktree --unset filter.git-crypt.smudge
```

## X11 keyboard layout

`.config/xkb/README.md`

```sh
sudo install -vTC -m 0644 -g 0 -o 0 ~/.config/xkb/symbols/dvp /usr/share/X11/xkb/symbols/dvp
setxkbmap -verbose 10 -layout dvp -variant intl -option lv3:ralt_switch
xkbcomp -I ~/.config/xkb ~/.config/xkb/keymap.xkb "$DISPLAY"
```

# Syncthing

Web UI <http://localhost:8384>

# Vim-like

<https://vim.reversed.top/>
