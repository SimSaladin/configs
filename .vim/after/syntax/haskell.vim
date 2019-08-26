" File: ~/.vim/after/syntax/haskell.vim
" Haskell: add CoffeeScript syntax for [coffee| ... |] blocks.
if !exists("b:current_syntax")
  finish
endif

let old_syntax = b:current_syntax
unlet b:current_syntax

syn include @coffee syntax/coffee.vim
syn region coffeeQQ matchgroup=haskellQuasiQuoted start=/\[\$\?coffee|/ end=/|\]/ keepend contains=@coffee,coffeeInterp
syn region coffeeQQInterp start="%{"  end="}" keepend contained contains=TOP

let b:current_syntax = old_syntax
