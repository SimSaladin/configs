" File: .vim/plugin/cmds.vim

command!
      \ IndentInfo
      \ call vimrc#indent_info()

command! -range=%
      \ IndentCheck
      \ exe '<line1>,<line2>g/\v^%(%(\t|\s{'.&l:sw.'})+)@>\s/echo expand("%") line(".")'

command! -range=% -nargs=+ -complete=command
      \ IndentCheckDo
      \ exe '<line1>,<line2>g/\v^%(%(\t|\s{'.&l:sw.'})+)@>\zs\s*/' . <q-args>

command!
      \ IndentHighlight
      \ highlight default link indentError Error |
      \ exe 'match indentError /\v^%(%(\t|\s{'.&l:sw.'})+)@>\zs\s+/'

command! -complete=shellcmd -nargs=+
      \ Shell call vimrc#shell(<q-args>)

command! Syn
      \ echo synIDattr(synID(line("."),col("."),1),"name")

command! DiffOrig
      \ vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

command! LogAutocmds
      \ call logautocmds#toggle()

cnoremap w!!
      \ silent w !test -f $'%' -a \\! -w $'%' && sudo dd status=none of=$'%'<NL>e!<cr>

command! -range=%
      \ Hindent call haskell#hindent()

command! DeinUpdate
      \ call dein#update() | exe 'edit' g:dein#install_log_filename

command! DeinCleanReCache
      \ call dein#clear_state() | call dein#recache_runtimepath()
