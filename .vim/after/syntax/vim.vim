" File: ~/.vim/after/syntax/vim.vim
" Vim: Random additions to vimscript syntax.
if !exists('b:current_syntax')
   finish
endif

" There can be line comment after map {rhs}, if written as .. |" a comment.
" Highlight that.
syntax match vimLineComment +[=\\]\@<!|".*$+ containedin=vimMapRhs

" default: cterm=none ctermfg=14
highlight helpExample cterm=bold ctermfg=14
