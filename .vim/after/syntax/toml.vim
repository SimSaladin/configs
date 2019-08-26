" File: ~/.vim/after/syntax/toml.vim
" TOML: Add vImscript highlighting between ''' / ''' blocks. (Useful with dein)
if !exists("b:current_syntax") || !exists('g:loaded_SyntaxRange')
  finish
endif
call SyntaxRange#IncludeEx('start=+\(#.\{-}\)\@<!''\{3}+ms=e+1 end=+''\{3}+me=s-1 keepend containedin=tomlString contained', 'vim')
