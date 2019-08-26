" File: ~/.vim/after/syntax/vimwiki.vim
" Vimwiki: The default vimwiki syntax doesn't look nice in terminal
if !exists('b:current_syntax')
  finish
endif

highlight link VimwikiList Type

highlight VimwikiMarkers cterm=none ctermfg=3
highlight VimwikiHeader1 cterm=bold ctermfg=9
highlight VimwikiHeader2 cterm=bold ctermfg=9
highlight VimwikiHeader3 cterm=bold ctermfg=9
highlight VimwikiHeader4 cterm=none ctermfg=3
highlight VimwikiHeader5 cterm=none ctermfg=3
highlight VimwikiHeader6 cterm=none ctermfg=3
highlight VimwikiCode    cterm=none ctermfg=1
highlight VimwikiPre                ctermfg=6
highlight VimwikiHR                 ctermfg=3
