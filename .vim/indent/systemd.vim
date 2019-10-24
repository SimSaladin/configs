" ~/.vim/indent/systemd.vim
" Language: systemd
" Author:   Samuli Thomasson
" License:  WTFPL
scriptencoding utf-8
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetSystemdIndent() indentkeys+=0=\#\ ,0=\;
let b:undo_indent = 'setl indentexpr< indentkeys<'

" Define once                                                             {{{1
if exists('*GetSystemdIndent')
  finish
endif
let s:keepcpo = &cpo
set cpo&vim

let s:lineContPat = '^[^;#].*\\\s*$'

function! GetSystemdIndent()
  " Comments always zero indent
  let cur_txt = getline(v:lnum)
  if cur_txt =~ '^[;#]\|^\s*[;#]\s'
    return 0
  endif

  " Find previous non-blank not-comment line
  let lnum = prevnonblank(v:lnum - 1)
  while lnum > 0 && getline(lnum) =~ '^[;#]'
    let lnum = prevnonblank(lnum - 1)
  endwhile

  " Start of file use zero indent.
  if lnum == 0
    return 0
  endif

  " If previous non-blank-or-comment is continuing (ends in backslash) and has indent 0, indent current by shiftwidth.
  " If it's continuing but not zero indent, use that indent.
  " Otherwise reset to zero indent.
  let prevtxt = getline(lnum)
  if prevtxt =~ '\\\s*$'
    return (prevtxt =~ '^\S') ? shiftwidth() : indent(lnum)
  else
    return 0
  endif
endfunction

" [Section B]
" Setting="something" "some thing" "..."
" KeyTwo=value 2 \
"        value 2 continued
" [Section C]
" KeyThree=value 2\
" # this line is ignored
" ; this line is ignored too
"        value 2 continued

let &cpo = s:keepcpo
unlet s:keepcpo
