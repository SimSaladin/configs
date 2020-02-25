" File: ~/.vim/rc/options_vim-bookmarks.vim
" Author: Samuli Thomasson
" Description: settings for vim-bookmarks plugin

scriptencoding utf-8

let g:bookmark_no_default_key_mappings = 1
let g:bookmark_save_per_working_dir    = 0
let g:bookmark_auto_save               = 1   " load and save whenever buffer changes
let g:bookmark_auto_save_file          = get(g:,'myvimrc#datadir',$HOME).'/bookmarks'
let g:bookmark_highlight_lines         = 0
let g:bookmark_show_warning            = 0
let g:bookmark_show_toggle_warning     = 0
let g:bookmark_center                  = 0   " Vertical line centering when jumping to bmark
let g:bookmark_auto_close              = 1   " Auto-close the bmark split when jumping to a bmark
let g:bookmark_location_list           = 1   " Use loc list for bmarks
let g:bookmark_disable_ctrlp           = 0   " Disable ctrl-p
let g:bookmark_sign                    = '♥' " Signs
let g:bookmark_annotation_sign         = '#'

highlight link BookmarkSign Todo
highlight BookmarkLine           ctermbg=194 ctermfg=NONE
highlight BookmarkAnnotationLine ctermbg=121 ctermfg=NONE

fun! s:setup()
  call unite#custom#profile('source/vim_bookmarks', 'context', {
  \ 'profile-name':'bmarks',
  \ 'direction':'dynamictop',
  \ 'auto-resize':1,
  \ 'start_insert':0,
  \ 'no_quit':0,
  \ 'keep_focus':0,
  \ 'immediately':1,
  \ 'auto-preview':1,
  \ 'auto-highlight':1,
  \ 'hide-source-names':1,
  \ })
endfun

if has('vim_starting')
  au VimEnter * call s:setup()
else
  call s:setup()
endif
