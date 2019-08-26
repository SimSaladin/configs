" File: ~/.vim/ftplugin/json.vim
" Author: Samuli Thomasson

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal formatprg=jq\ .
let b:undo_ftplugin = 'setlocal fp<'
