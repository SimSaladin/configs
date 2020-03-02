" File: ~/.vim/rc/options_syntastic.vim
" Author: Samuli Thomasson
" Description: Options for syntastic plugin

scriptencoding utf-8

" Use :SyntasticInfo to see available checkers for a given filetype.
" Checkers documentation: <url:vimhelp:syntastic-checkers.txt>

let g:syntastic_aggregate_errors         = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_jump                = 1
let g:syntastic_auto_loc_list            = 1
let g:syntastic_check_on_open            = 1
let g:syntastic_check_on_wq              = 0

let g:syntastic_error_symbol             = '║E'
let g:syntastic_warning_symbol           = '║W'
let g:syntastic_style_error_symbol       = '│E'
let g:syntastic_style_warning_symbol     = '│W'

let g:syntastic_extra_filetypes          = []
"let g:syntastic_id_checkers             = 1

let g:syntastic_haskell_checkers         = ['hlint']
let g:syntastic_help_checkers            = ['vimhelplint']
let g:syntastic_python_checkers          = ['python', 'pylint']
let g:syntastic_vim_checkers             = ['vint']
let g:syntastic_yaml_checkers            = ['yamllint']

" yaml/yamllint <url:vimhelp:syntastic-yaml-yamllint>
" vim/vint: needs pip install vim-vint (pacman -S vint)

