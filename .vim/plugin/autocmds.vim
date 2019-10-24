" File: ~/.vim/plugin/autocmds.vim

if get(g:,'no_vimrc_autocmds')
  finish
endif

" Perform (again) filetype detection after a write if filetype is not yet known.
augroup MyFtRedetect
  au BufWritePost * ++nested call init#redetect_filetype()
augroup END

" Create missing parent directories when writing the buffer of a new file for the first time.
" Note: should not use '<afile>' but instead '%' because the buffer name might have changed since!
augroup MyAutoMkdir
  au!
  au BufNewFile *?
        \ au BufWritePre <buffer=abuf> ++once call vimrc#mkdir_missing(expand('%:p:h'))
augroup END

" Insert a skeleton (header) when a buffer of a new file is loaded.
augroup MyAutoHeader
  au!
  au BufNewFile *?
        \ au FileType <buffer=abuf> ++once call vimrc#insert_header()
augroup END

augroup MyQFLoc
  au!
  "autocmd QuickFixCmdPost *grep* cwindow
  autocmd QuickFixCmdPost *grep* nested cwindow
  "autocmd QuickFixCmdPost *Syntastic* nested lwindow

  " Close quickfix window automatically when leaving window
  au WinEnter * if winnr('$') == 1 && &buftype == 'quickfix' | q | endif
augroup END
