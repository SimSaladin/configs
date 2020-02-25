What is all this?
=================

A public repo I use to track my configuration files (aka. dotfiles) among many
systems.

```sh
git clone --bare git@gitlab.com:funaali/dotfiles.git ~/.dotfiles

alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

config submodule update --init --recursive

config config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'
config config status.showUntrackedFiles no
config crypt init
config crypt unlock
```

## git worktrees and git-crypt

```sh
git config --worktree --unset filter.git-crypt.clean
git config --worktree --unset filter.git-crypt.smudge
```

## VIM (*`~/.vimrc`*)

The plugin manager (dein.vim) + plugins will be installed automatically when
starting vim (if needed). Network connection required.

# Xorg

## Keyboard layout

- .config/xkb/README.md

```sh
sudo install -vTC -m 0644 -g 0 -o 0 ~/.config/xkb/symbols/dvp /usr/share/X11/xkb/symbols/dvp
setxkbmap -verbose 10 -layout dvp -variant intl -option lv3:ralt_switch
xkbcomp -I ~/.config/xkb ~/.config/xkb/keymap.xkb "$DISPLAY"
```

# Rclone (Onedrive, Google Drive etc.)

`rclone config`
