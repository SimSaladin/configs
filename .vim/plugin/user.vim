" File: plugin/user.vim
" Author: Samuli Thomasson
" License: WTFPL

" Preamble {{{1
scriptencoding utf-8

if exists('g:loaded_user_plugin')
  finish
endif
let g:loaded_user_plugin = 1

let s:cpo_save = &cpo
set cpo&vim
" commands {{{1

" Note: also see plugin/manpager.vim
if !exists(':Man')
  runtime ftplugin/man.vim
endif

command! Syn echo synIDattr(synID(line("."),col("."),1),"name")
command! DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

command! LogAutocmds call logautocmds#toggle()
command! IndentInfo call vimrc#indent_info()
command! -complete=shellcmd -nargs=+ Shell call vimrc#shell(<q-args>)

" n[nore]map {{{1

nmap _ <Leader>
nmap - <LocalLeader>
nnoremap ; :
nnoremap Q q
nnoremap <C-@> <C-^>
nnoremap gs :Shell<space>
nnoremap <silent> <Leader>H ml:execute 'match Search /\%'.line('.').'l/'<CR>

" c[nore]map {{{1

cnoremap w!! silent w !test -f $'%' -a \\! -w $'%' && /usr/bin/sudo /usr/bin/dd status=none of=$'%'<NL>e!<CR>

" buffer-local options {{{1

" assumed global defaults: ts=8 sw=2 sts=-1 et sr tw=78 fo=croanlj fex= fp=

" XXX
let g:findent#samples = [2, 4, 8]

function! s:SetLocalOptions()
  if empty(&l:ft)
    setlocal noet nonu nornu signcolumn=no foldcolumn=0
  elseif !&l:modifiable || exists('b:did_indent')
    return
  elseif &l:ft =~# '^\(ruby\|python\|elm\|java\|groovy\)$'
    setlocal sw=4
  elseif &l:ft ==# 'help'
    setlocal fo-=c
  elseif &l:ft ==# 'text'
    setlocal fo=1na sw=0 noet
  else
    execute 'Findent --no-messages --no-warnings'
  endif
endfunction

" autocmds {{{1

augroup vimrc_user
  autocmd!
  " Insert header from snippet if available
  autocmd BufNewFile * call vimrc#insert_header()
  " Create missing parents on write
  autocmd BufNewFile *
        \ autocmd BufWritePre <buffer=abuf> ++once call vimrc#mkdir_missing(expand('<afile>:p:h'))
  " Set buffer-local options for new buffer
  autocmd BufNewFile,BufReadPre *
        \ autocmd BufWinEnter <buffer=abuf> ++once call <SID>SetLocalOptions()
  autocmd OptionSet tabstop,expandtab,shiftwidth
        \ if v:option_type ==# 'local' | let b:did_indent = v:option_command | endif
  autocmd OptionSet foldenable
        \ if v:option_type ==# 'local' | let &l:foldcolumn = v:option_new ? &g:foldcolumn : 0 | endif
  autocmd OptionSet foldlevel
        \ if v:option_type ==# 'local' | let &l:foldcolumn = v:option_new ==# '0' ? 0 : &g:foldcolumn | endif
augroup END

" END {{{1
let &cpo = s:cpo_save
unlet s:cpo_save
