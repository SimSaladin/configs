What is all this?
=================

A public repo I use to track my configuration files (aka. dotfiles) among many
systems.


## git worktrees and git-crypt

```sh
git config --worktree --unset filter.git-crypt.clean
git config --worktree --unset filter.git-crypt.smudge
```

## VIM (*`~/.vimrc`*)

The plugin manager (dein.vim) + plugins will be installed automatically when
starting vim (if needed). Network connection required.
