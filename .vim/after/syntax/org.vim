" File: ~/.vim/after/syntax/org.vim
" Org Filetype: add syntax ranges for some code blocks.
if !exists('g:loaded_SyntaxRange')
  finish
endif

call SyntaxRange#Include('#+BEGIN_SRC sh', '#+END_SRC', 'sh')
" XXX sh: can't label-hl here or it may extend to the end of buffer?
call SyntaxRange#Include('#+BEGIN_SRC haskell',  '#+END_SRC', 'haskell', 'comment')
call SyntaxRange#Include('#+BEGIN_SRC markdown', '#+END_SRC', 'markdown', 'comment')
call SyntaxRange#Include('#+BEGIN_SRC help',     '#+END_SRC', 'help', 'comment')
