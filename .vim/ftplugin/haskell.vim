" File: ~/.vim/ftplugin/haskell.vim

if exists('b:did_haskell_plugs')
  finish
endif
let b:did_haskell_plugs = 1

" TODO: ghcid
" TODO: use basic some syntactic extensions in hindent
" TODO: b:hindent_sort_imports easy access
" TODO: b:hindent_on_save easy access
" TODO: implement on_save
" TODO: multi-use formatprg (?)

let b:hindent_sort_imports = 0
let b:hindent_line_length  = &textwidth
let b:hindent_indent_size  = &shiftwidth
