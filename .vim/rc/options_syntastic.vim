" File: ~/.vim/rc/options_syntastic.vim
" Author: Samuli Thomasson
" Description: Options for syntastic plugin

scriptencoding utf-8

let g:syntastic_aggregate_errors         = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_jump                = 1
let g:syntastic_auto_loc_list            = 1
let g:syntastic_check_on_wq              = 0

" Symbols
let g:syntastic_error_symbol             = '║E'
let g:syntastic_warning_symbol           = '║W'
let g:syntastic_style_error_symbol       = '│E'
let g:syntastic_style_warning_symbol     = '│W'

" Checkers
let g:syntastic_extra_filetypes          = []
"let g:syntastic_id_checkers             = 1

let g:syntastic_python_checkers          = ['python', 'pylint']

" XXX: needs pip install vim-vint (pacman -S vint)
let g:syntastic_vim_checkers             = ['vint']
let g:syntastic_help_checkers            = ['vimhelplint']

let g:syntastic_haskell_checkers         = ['hlint']

" XXX: should be more intelligent
let g:syntastic_yaml_checkers            = ['yamllint']
let g:syntastic_yaml_yamllint_args       = '-c ~/.yamllint.yaml'
