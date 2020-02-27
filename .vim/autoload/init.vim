" File: ~/.vim/autoload/init.vim

function! init#rc(name) abort                                        "{{{1
  " Sources file from ~/.vim/rc/
  let l:file = resolve($HOME.'/.vim/rc/'.fnamemodify(a:name,':t').'.vim')
  if !filereadable(l:file)
    echoerr 'File ' . l:file . ' not readable!'
    return v:none
  endif
  execute 'source' fnameescape(l:file)
endfunction

function! init#on_filetype() abort                                   "{{{1
  if execute('filetype') =~# 'OFF'
    silent! filetype plugin indent on
    filetype detect
  endif
endfunction

function! init#redetect_filetype() abort                             "{{{1
  if &l:filetype ==# '' || exists('b:ftdetect')
    unlet! b:ftdetect
    filetype detect
  endif
endfunction

function! init#on_colors() abort                                     "{{{1
  if exists('#QuickFixSignsVscdiff#ColorScheme')
    doautoall QuickFixSignsVscdiff ColorScheme
  endif
endfunction

function! init#setup_dein(to, src) abort                             "{{{1
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
function! init#py3module(module) abort                               "{{{1
  if has('python3')
    py3 import importlib.util
    return py3eval('getattr(importlib.util.find_spec("'.a:module.'"),"origin",None)')
  endif
  return v:none
endfunction

function! init#pip3installupgrade(module) abort                      "{{{1
  " If the python3 module a:module is not available, install it for the local
  " user if pip3 is available. If the module is installed for local user,
  " attempt to upgrade it.
  " package: python-pip
  if has('python3')
    py3 from pip._internal.main import main as pipmain
    if filereadable('requirements.txt')
      exe 'py3' 'pipmain(["install","--user","--upgrade","'.a:module.'","-r","requirements.txt"])'
    else
      exe 'py3' 'pipmain(["install","--user","--upgrade","'.a:module.'"])'
    endif
  endif
endfunction
