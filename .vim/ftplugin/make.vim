" File: ~/.vim/ftplugin/make.vim
" Author: Samuli Thomasson

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1
let b:undo_ftplugin = 'setlocal et< ts< vts< vsts< fo< com< cms< inc<'

setlocal noexpandtab ts=2 vts=2,8 vsts=2,8

" Set 'formatoptions' to break comment lines but not other lines,
" and insert the comment leader when hitting <CR> or using "o".
setlocal fo-=t fo+=croql

" Set 'comments' to format dashed lists in comments
setlocal com=sO:#\ -,mO:#\ \ ,b:#

" Set 'commentstring' to put the marker after a #.
setlocal commentstring=#\ %s

" Including files.
let &l:include = '^\s*include'
