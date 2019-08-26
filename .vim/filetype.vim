if exists('did_load_filetypes')
  finish
endif
augroup filetypedetect
  autocmd BufRead,BufNewFile *mutt-* setfiletype mail
  autocmd BufRead,BufNewFile */.vim/doc/*.txt setfiletype help
  autocmd BufRead,BufNewFile */systemd/network/*.{link,network,netdev} setfiletype systemd
augroup END
"
" See also: vim-polyglot/ftdetect/polyglot.vim
