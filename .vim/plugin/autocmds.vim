" File: ~/.vim/plugin/autocmds.vim

if get(g:,'no_vimrc_autocmds') || exists('g:did_vimrc_autocmds')
  finish
endif
let g:did_vimrc_autocmds = 1

" Perform (again) filetype detection after a write if filetype is not yet known.
augroup MyFtRedetect
  au!
  au BufWritePost * nested call init#redetect_filetype()
augroup END

" Create missing parent directories when writing the buffer of a new file for the first time.
" Note: should not use '<afile>' but instead '%' because the buffer name might have changed since!
augroup MyAutoMkdir
  au!
  if v:version > 800 " uses ++once
    au BufNewFile *? au BufWritePre <buffer=abuf> ++once call vimrc#mkdir_missing(expand('%:p:h'))
  endif
augroup END

" Insert a skeleton (header) when a buffer of a new file is loaded.
augroup MyAutoHeader
  au!
  if v:version > 800 " uses ++once
    au BufNewFile *? au FileType <buffer=abuf> ++once
      \ if (empty(&bt) && line('$') ==# 1 && getline(1) ==# '')
      \|  startinsert!
      \|  call vimrc#insert_header()
      \|endif
  endif
augroup END

augroup MyQFLoc
  au!
  autocmd QuickFixCmdPost *grep* nested cwindow

  " Close quickfix window automatically when leaving window
  au WinEnter * if winnr('$') == 1 && &buftype == 'quickfix' | q | endif
augroup END

augroup MySetHls
  au!
  let g:hls = &g:hls
  au FileType man set hls
  au WinEnter * if &l:ft == 'man' | set hls | endif
  au WinLeave * if get(g:,'hls') | set hls | else | set nohls | endif
augroup END

fun! s:Resize()
  " Resize when ft=man so content fits on page.
  if &l:ft ==# 'man'
    let lpos = str2float(line('.'))/line('$')
    let cpos = str2float(col('.'))/&l:columns
    keepj norm! gg0
    keepj exe "norm \<Plug>ManPreGetPage\<CR>"
    keepj exe 'norm! '.float2nr(lpos*line('$')).'G'.float2nr(cpos*&l:columns).'l'
  endif
endfun

augroup MyResize
  au!
  au VimResized * windo call s:Resize()
augroup END
