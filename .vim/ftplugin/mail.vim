" File: ~/.vim/ftplugin/mail.vim
" Author: Samuli Thomasson

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal shiftwidth=4 noet formatoptions=1na
let b:undo_ftplugin = 'setlocal sw< et< fo<'
