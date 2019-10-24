" ~/.vim/after/syntax/toml.vim

if !exists("b:current_syntax") || !exists('g:loaded_SyntaxRange')
  finish
endif

call SyntaxRange#IncludeEx('start=+\(#.\{-}\)\@<!''\{3}+ms=e+1 end=+''\{3}+me=s-1 keepend containedin=tomlString contained', 'vim')
