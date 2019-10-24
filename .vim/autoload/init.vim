" File: ~/.vim/autoload/init.vim

if &compatible
  set nocompatible
endif

function! init#rc(name) abort
  let l:file = resolve($HOME.'/.vim/rc/'.fnamemodify(a:name,':t').'.vim')
  if filereadable(l:file)
    execute 'source ' . fnameescape(l:file)
  else
    echoerr 'File ' . l:file . ' not readable!'
  endif
endfunction

function! init#on_filetype() abort
  if execute('filetype') =~# 'OFF'
    silent! filetype plugin indent on
    filetype detect
  endif
endfunction

function! init#redetect_filetype() abort
  if &l:filetype ==# '' || exists('b:ftdetect')
    unlet! b:ftdetect
    filetype detect
  endif
endfunction

" Re-sourcing workarounds
function! init#on_colors() abort
  if exists('#utl_highl#BufWinEnter')
    doautoall utl_highl BufWinEnter
  endif
  if exists('#QuickFixSignsVscdiff#ColorScheme')
    doautoall QuickFixSignsVscdiff ColorScheme
  endif
  if exists(':AirlineRefresh')
    AirlineRefresh
  endif
endfunction

function! init#setup_dein(to, src) abort
  " return value:
  " 0: setup failed
  " 1: setup succeeded (rtp updated)
  " 2: already setup (probably)
  if stridx(&g:runtimepath, a:to) > 0 || exists('#dein')
    return 2
  endif
  if !isdirectory(a:to) && !get(g:, 'myvimrc#dein_only_cached')
    try
      exe '!git clone -q' shellescape(a:src) shellescape(a:to)
    catch /.*/
      echomsg 'WARNING: couldn''t perform initial dein checkout from ' . a:src . ' to ' . a:to . ' the exception was: ' . v:exception
      return 0
    endtry
  endif
  exe 'set runtimepath+=' . a:to
  return 1
endfunction

" Check for existance of a python3 module (not actually importing it).
function! init#py3module(module) abort                                   "{{{1
  if has('python3')
    py3 import importlib
    return py3eval('getattr(importlib.util.find_spec("'.a:module.'"),"origin",None)')
  endif
  return v:none
endfunction

" If the python3 module a:module is not available, install it for the local
" user if pip3 is available. If the module is installed for local user,
" attempt to upgrade it.
function! init#pip3installupgrade(module) abort                          "{{{1
  let l:check = vimrc#py3module(a:module)
  if has('python3') && (empty(l:check) || filewritable(l:check))
    py3 import pip; pip.main(['install', '--user', '--upgrade', a:module])
  endif
endfunction
