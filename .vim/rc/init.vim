" File: ~/.vim/rc/init.vim
" Author: Samuli Thomasson
" License: RELEX Solutions proprietary

scriptencoding utf-8

if &compatible
  set nocompatible
endif

language message C    " Use the standard locale for vim UI

" Both leaders expand when a mapping is defined, so it's useful to define them early.
let g:mapleader      = ','
let g:maplocalleader = '\'

let s:cache    = exists('$XDG_CACHE_HOME') ? $XDG_CACHE_HOME.'/vim' : $HOME.'/.cache/vim'
let g:deindir  = s:cache . '/dein'

let g:datadir = s:cache . '/data' " XXX

" XXX: The directory, backupdir, undodir options get in set in
" /usr/share/vim/vimfiles/archlinux.vim on Arch.
" Can't depend on that though, so create everything always.
let &g:undodir      = s:cache . '/undo//'
let &g:backupdir    = s:cache . '/backup//'
let &g:directory    = s:cache . '/swap//'
let &g:viewdir      = s:cache . '/view//'
let &g:viminfofile  = s:cache . '/viminfo'

" system() uses $TMPDIR and doesn't consider &g:shelltemp.
let $TMPDIR = exists('$TMPDIR') ? $TMPDIR :
      \ exists('$XDG_RUNTIME_DIR') ? $XDG_RUNTIME_DIR . '/vim' :
      \ s:cache . '/tmp'

" Create missing
for var in [expand(g:datadir),expand(&g:undodir),expand(&g:backupdir),expand(&g:directory),expand(&g:viewdir),expand($TMPDIR)]
  if !isdirectory(var)
    silent! call mkdir(var, 'p', 0700)
  endif
endfor

" Don't load default plugins
let g:loaded_2html_plugin      = 1
let g:loaded_logiPat           = 1
let g:loaded_getscriptPlugin   = 1
let g:loaded_gzip              = 1
let g:loaded_man               = 1
let g:loaded_matchit           = 1
let g:loaded_matchparen        = 1
let g:loaded_netrwFileHandlers = 1
let g:loaded_netrwPlugin       = 1
let g:loaded_netrwSettings     = 1
let g:loaded_rrhelper          = 1
let g:loaded_shada_plugin      = 1
let g:loaded_spellfile_plugin  = 1
let g:loaded_tarPlugin         = 1
let g:loaded_tutor_mode_plugin = 1
let g:loaded_vimballPlugin     = 1
let g:loaded_zipPlugin         = 1
