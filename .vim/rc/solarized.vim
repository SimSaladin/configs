" File: ~/.vim/rc/solarized.vim

" let s:filepath = resolve(get(dein#get('solarized'), 'path', '.') . '/colors/solarized.vim')

let $TERM_PROGRAM          = 'rxvt'  " Cheat for screen/tmux
let g:solarized_termtrans  = 1       " Assume terminal colors are set to the solarized color map
let g:solarized_bold       = 1       " Use bold typeface
let g:solarized_underline  = 0       " Use underline
let g:solarized_italic     = 1       " Use italic typeface
let g:solarized_contrast   = 'normal' " normal | high | low
let g:solarized_diffmode   = 'high'   " normal | high | low
let g:solarized_visibility = 'normal' " normal | high | low
let g:solarized_hitrail    = 0        " Experimental

" Overwrite some highlights
function! s:my_solarized()
  if !has('gui_running') && &background ==# 'dark' && &t_Co >=# 255
    highlight CursorLineNr ctermbg=0                  ctermfg=30
    highlight LineNr       ctermbg=none               ctermfg=30
    highlight SignColumn   ctermbg=none
    highlight Folded       ctermbg=none   cterm=bold  ctermfg=11
    highlight Todo         ctermbg=none   cterm=none  ctermfg=198
    highlight Underlined   ctermbg=none   cterm=none  ctermfg=67

    highlight MySignDiffAdd    cterm=bold ctermfg=28 ctermbg=0
    highlight MySignDiffDelete cterm=none ctermfg=160 ctermbg=0
    highlight MySignDiffChange cterm=bold ctermfg=6 ctermbg=0
  endif
endfunction

augroup my_solarized
  au!
  au ColorScheme solarized call s:my_solarized()
augroup END

