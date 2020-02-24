" File: ~/.vim/rc/init.vim
" Note: directory, backupdir, undodir are (also) set in /usr/share/vim/vimfiles/archlinux.vim on ArchLinux.

if exists('g:did_myvimrc_init')
  finish
endif
let g:did_myvimrc_init = 1

" Save our original runtimepath                                      {{{1
let g:myvimrc#initrtp = &g:rtp

" Set language                                                       {{{1
language message C

" Set mapleaders                                                     {{{1
" NOTE: Leaders expand when a mapping is defined, so it's useful to define them early.
let g:mapleader      = ','
let g:maplocalleader = '\'

" Files and directories                                              {{{1

if empty($XDG_CACHE_HOME)
  call setenv('XDG_CACHE_HOME',$HOME.'/.cache')
endif
let g:myvimrc#cachedir = $XDG_CACHE_HOME . '/vim'

if empty($XDG_DATA_HOME)
  call setenv('XDG_DATA_HOME',$HOME.'/.local/share')
endif
let g:myvimrc#datadir = $XDG_DATA_HOME  . '/vim'

" Note: system() does not consider 'shelltemp', so we setup $TMPDIR if it doesn't exist yet.
if empty($TMPDIR) || $TMPDIR ==# '/tmp'
  if isdirectory($XDG_RUNTIME_DIR) && filewritable($XDG_RUNTIME_DIR)
    call setenv('TMPDIR',$XDG_RUNTIME_DIR.'/vim')
  else
    call setenv('TMPDIR',g:myvimrc#cachedir.'/tmp')
  endif
endif
let g:myvimrc#tmpdir     = $TMPDIR
let g:myvimrc#runtimedir = $TMPDIR

for s:dir in [g:myvimrc#cachedir,g:myvimrc#datadir,g:myvimrc#tmpdir,g:myvimrc#runtimedir]
  if !isdirectory(s:dir)
    call mkdir(s:dir,'p',0700)
  endif
endfor

" Vim default plugins                                                {{{1

let g:loaded_getscriptPlugin  = 1
let g:loaded_gzip             = 1
let g:loaded_logiPat          = 1
let g:loaded_matchparen       = 1
let g:loaded_netrwPlugin      = 1
let g:loaded_rrhelper         = 1
let g:loaded_spellfile_plugin = 1
let g:loaded_tarPlugin        = 1
let g:loaded_2html_plugin     = 1
let g:loaded_zipPlugin        = 1

" If you happen to install a VBA some day. Open the .vba and source it. Make
" sure to comment out the g:loaded_vimballPlugin first and start new session.
let g:vimball_home            = $HOME.'/.vim/local-vba'
let g:loaded_vimballPlugin    = 1

" See <url:vimhelp:ft-bash-syntax> and syntax/sh.vim
let g:is_bash         = 1
let g:sh_fold_enabled = 7 " For foldmethod=syntax

" cecutil plugin                                                     {{{1

" The cecutil plugin is included in so many other plugins
let g:no_cecutil_maps = 1
