" File: ftplugin/haskell/format.vim
if exists('b:did_haskell_format')
  finish
endif
let b:did_haskell_format = 1

" TODO: use basic some syntactic extensions
" TODO: b:hindent_sort_imports easy access
" TODO: b:hindent_on_save easy access
" TODO: implement on_save
" TODO: multi-use formatprg (?)

" augroup hindent
"     autocmd!
"     autocmd BufWritePre *.hs call hindent#HindentOnSave()
" augroup END

let g:hindent_command      = 'hindent'
let b:hindent_sort_imports = 0
let b:hindent_line_length  = &l:textwidth
let b:hindent_indent_size  = &l:shiftwidth

let b:undo_haskell_format = 'setlocal formatprg<'

setlocal formatprg=stylish-haskell

if exists('s:did_load')
  finish
endif
let s:did_load = 1

function s:Hindent() range "{{{
  if !exists('g:hindent_command') || !executable(g:hindent_command)
    echomsg 'hindent not found in $PATH, did you install it?'
    return
  endif

  let winview = winsaveview()

  let cmd = g:hindent_command .
    \ ' --indent-size ' . b:hindent_indent_size .
    \ ' --line-length ' . b:hindent_line_length

  if b:hindent_sort_imports
    let cmd .= ' --sort-imports'
  endif

  silent! exe 'w !' . cmd . ' > /dev/null 2>&1'
  if v:shell_error
    echohl WarningMsg
    echo 'Hindent: Parsing error\n'
    echohl None
  else
    silent! exe 'undojoin'
    silent! exe 'keepjumps '.a:firstline.','.a:lastline.'!'.cmd
  endif

  call winrestview(winview)
endfunction "}}}

command -range=% Hindent execute "<line1>,<line2>call <SID>Hindent()"
noremap <unique> <script> <Plug>(Hindent) <SID>Hindent
noremap <SID>Hindent :call <SID>Hindent()<CR>
