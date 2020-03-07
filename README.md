dotfiles
========

A public repo I use to track my configuration files (aka. dotfiles) among many
systems.

# Setup

```sh
cd ~
git init .
git remote add origin git@gitlab.com:funaali/dotfiles.git
git fetch

....

git submodule update --init --recursive

git crypt unlock
```

## Alternative setup: separate bare repository

```sh
git clone --bare git@gitlab.com:funaali/dotfiles.git ~/.dotfiles

alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

config submodule update --init --recursive

config checkout -b node-$HOSTNAME
config worktree add --track .dotfiles-work origin/master

config config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'
config config status.showUntrackedFiles no
config crypt init
config crypt unlock
config config filter.git-crypt.required false
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

# `~/.password-store` (GNU pass)

```sh
git clone git@gitlab.com:funaali/password-store.git ~/.password-store
cat ~/.password-store/README.md
```

# Adding changes

```sh
cd .dotfiles-work
git checkout master
git up
git cherry-pick $(git rev-list --reverse --ancestry-path --right-only --no-merges HEAD...node-$HOSTNAME)
config merge master
config push --all
```

# Media repositories

```sh
git clone sim@applicative:Music.git Music
git clone git@applicative:Pictures.git Pictures
git clone sim@applicative:Videos.git Videos

for repo in Music Pictures Videos; do
  git -C $repo annex init $HOSTNAME
  git -C $repo annex sync
done

```
