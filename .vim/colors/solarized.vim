" File: ~/.vim/colors/solarized.vim
" Author: Samuli Thomasson
" Description: Wrapper over <url:file:~/.cache/vim/dein/repos/*/*/*/colors/solarized.vim>

let $TERM_PROGRAM          = 'rxvt'  " Cheat for screen/tmux
let g:solarized_termtrans  = 1       " Assume terminal colors are set to the solarized color map
let g:solarized_bold       = 1
let g:solarized_underline  = 0
let g:solarized_italic     = 1
let g:solarized_contrast   = "normal" " normal | high | low
let g:solarized_diffmode   = "low"    " normal | high | low
let g:solarized_visibility = 'low'    " When 'set list' have less stuff visible
let g:solarized_hitrail    = 0        " Experimental

execute 'source' fnameescape(dein#util#_get_plugins('solarized')[0].path .'/colors/solarized.vim')

if has("gui_running")
  finish
endif

" Overwrite some highlights
if &g:background ==# 'dark' && &t_Co >=# 255
  highlight CursorLineNr ctermbg=0                  ctermfg=30
  highlight LineNr       ctermbg=none               ctermfg=30
  highlight SignColumn   ctermbg=none
  highlight Folded       ctermbg=none   cterm=bold  ctermfg=11
  highlight Todo         ctermbg=none   cterm=none  ctermfg=198
  highlight Underlined   ctermbg=none   cterm=none  ctermfg=67
endif
