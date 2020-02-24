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
      \ silent w !test \( -f $'%' -o \\! -e $'%' \) -a \\! -w $'%' && sudo dd status=none of=$'%'<NL>e!<cr>

command! -range=%
      \ Hindent call haskell#hindent()

command! DeinUpdate
      \ call dein#update() | exe 'edit' g:dein#install_log_filename

command! DeinCleanReCache
      \ call dein#clear_state() | call dein#recache_runtimepath()

command! -nargs=+ -bang PageMan call s:PageMan(<f-args>)

function! s:PageMan(...)
  let page = ''
  if a:0 == 0
    let page = expand('<cword>')
  elseif a:0 == 1
    let page = a:1
  elseif a:0 == 2
    let page = a:2.'.'.a:1
  endif
  exe 'Page!' 'man' shellescape(page) '2>/dev/null'
  exe 'file' 'man:'.page
endfunction
