" File: ftdetect/ftdetect.vim
scriptencoding utf-8

" if exists('g:did_load_filetypes')
"   finish
" endif

" Note: :setfiletype will not change a previously detected filetype.
" Note: :set filetype=<ft> does override any previous filetype.
" Note: ftdetect/<ft>.vim files may overrule the default file type checks.
" Note: to set filetype based on file contents write a scripts.vim.
" Note: See vim-polyglot/ftdetect/polyglot.vim if using vim-polyglot.

augroup filetypedetect

  " <url:vimhelp:ft-mail-plugin>
  autocmd BufRead,BufNewFile *mutt-* setfiletype mail

  autocmd BufRead,BufNewFile */.vim/doc/*.txt setfiletype help

  autocmd BufRead,BufNewFile *.{link,network,netdev} setfiletype systemd
  autocmd BufRead,BufNewFile */systemd/*/*.d/*.conf  setfiletype systemd

  autocmd BufRead,BufNewFile .envrc setfiletype sh

  " autocmd BufRead,BufNewFile *.template ++nested execute 'doautocmd BufRead '. expand('<afile>:r')
augroup END
