" File: after/ftdetect/ftdetect.vim
scriptencoding utf-8

augroup filetypedetect

  " Overrule a "setl ft=json" from the "json" plugin
  autocmd! BufRead,BufNewFile *.template

augroup END
