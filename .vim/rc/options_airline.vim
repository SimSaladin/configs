" File: ~/.vim/rc/options_airline.vim
" Author: Samuli Thomasson
" Description: Options for the airline plugin

scriptencoding utf-8

" NOTE: See :AirlineExtensions for available/loaded extensions

let g:airline_inactive_collapse  = 1 " default 1
let g:airline_inactive_alt_sep   = 0 " default 1
let g:airline_detect_iminsert    = 1
let g:airline_powerline_fonts    = 1

"let g:airline_exclude_filenames = []
"let g:airline_exclude_filetypes = []

let g:airline_filetype_overrides = {
      \ 'defx': ['Defx','%{b:defx.paths[0]}'],
      \ 'help': ['Help','%f'],
      \ 'minibufexpl': ['MiniBufExplorer',''],
      \ 'nerdtree': [get(g:,'NERDTreeStatusline','NERD'),''],
      \ 'startify': ['startify',''],
      \ 'vimfilter': ['vimfilter','%{vimfilter#get_status_string()}'],
      \ 'vimshell': ['vimshell','%{vimshell#get_status_string()}'],
      \ }

let g:airline_exclude_preview     = 0
let g:airline_skip_empty_sections = 1
let g:airline_highlighting_cache  = 1
let g:airline_focuslost_inactive  = 1

let g:airline#parts#ffenc#skip_expected_string = 'utf-8[unix]'

" Theme                                                             {{{1
let g:airline_theme              = 'solarized'
let g:airline_theme_patch_func   = 'AirlineThemePatch'

function! AirlineThemePatch(palette)
  if g:airline_theme ==# 'solarized'
    " XXX
    "for colors in values(a:palette.inactive)
    "  let colors[3] = 245
    "endfor
  endif
endfunction

" Mode indicators                                                   {{{1
let g:airline_mode_map = {
      \ '__'   :'?',
      \ 'c'    :'*',
      \ 'i'    :'I',
      \ 'ic'   :'I*',
      \ 'ix'   :'X',
      \ 'multi':'M',
      \ 'n'    :'N',
      \ 'ni'   :'iN',
      \ 'no'   :'O',
      \ 'R'    :'R',
      \ 'Rv'   :'vR',
      \ 's'    :'S',
      \ 'S'    :'Ss',
      \ ''   :'Sc',
      \ 't'    :'T',
      \ 'v'    :'V',
      \ 'V'    :'L',
      \ ''   :'B',
      \ }

" Separators                                                        {{{1
let g:airline_left_sep           = ''
let g:airline_left_alt_sep       = ''
let g:airline_right_sep          = ''
let g:airline_right_alt_sep      = ''

" Symbols                                                           {{{1
let g:airline_symbols_ascii      = 0
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.branch     = ''
let g:airline_symbols.crypt      = '🔒'
let g:airline_symbols.dirty      = '⚡'
let g:airline_symbols.linenr     = ''
let g:airline_symbols.maxlinenr  = '¶'
let g:airline_symbols.notexists  = 'Ɇ'
let g:airline_symbols.paste      = 'Þ'
let g:airline_symbols.readonly   = ''
let g:airline_symbols.spell      = 'Ꞩ'
let g:airline_symbols.whitespace = 'Ξ'

" Extensions                                                        {{{1

"let g:airline_extensions                       = ['branch', 'fugitiveline', 'whitespace', 'tabline', 'taskwarrior', 'quickfix', 'wordcount', 'syntastic', 'tagbar', 'bufferline']
"let g:airline#extensions#disable_rtp_load      = 1

let g:airline#extensions#nerdtree_status       = 1

let g:airline#extensions#bufferline#enabled    = 1
let g:airline#extensions#bookmark#enabled      = 1 " vim-bookmarks

let g:airline#extensions#nrrwrgn#enabled       = 0

let g:airline#extensions#tagbar#enabled        = 0

let g:airline#extensions#whitespace#checks     = ['indent', 'trailing', 'mixed-indent-file', 'conflicts']

let g:airline#extensions#wordcount#filetypes   = ['asciidoc', 'help', 'mail', 'markdown', 'pandoc', 'vimdoc', 'org', 'rst', 'tex', 'text']

" Tabline                                                           {{{2
let g:airline#extensions#tabline#enabled           = 1
let g:airline#extensions#tabline#alt_sep           = 1 " if g:airline_right_sep
let g:airline#extensions#tabline#show_splits       = 1 " (only when tabs are open)
let g:airline#extensions#tabline#show_tab_type     = 1
let g:airline#extensions#tabline#show_tab_nr       = 1
let g:airline#extensions#tabline#show_tabs         = 1
let g:airline#extensions#tabline#show_tab_count    = 1
let g:airline#extensions#tabline#show_buffers      = 1
let g:airline#extensions#tabline#show_close_button = 0
"let g:airline#extensions#tabline#excludes         = []
let g:airline#extensions#tabline#exclude_preview   = 1
let g:airline#extensions#tabline#tab_nr_type       = 1
let g:airline#extensions#tabline#buffers_label     = 'b'
let g:airline#extensions#tabline#tabs_label        = 't'
let g:airline#extensions#tabline#overflow_marker   = '…'
let g:airline#extensions#tabline#ignore_bufadd_pat = '\c\vgundo|undotree|vimfiler|tagbar|nerd_tree'

" Syntastic                                                         {{{2
let g:airline#extensions#syntastic#enabled         = 1
let g:airline#extensions#syntastic#error_symbol    = 'E:'
let g:airline#extensions#syntastic#warning_symbol  = 'W:'
let g:airline#extensions#syntastic#stl_format_err  = '%E{[%fe(#%e)]}'
let g:airline#extensions#syntastic#stl_format_warn = '%W{[%fw(#%w)]}'
