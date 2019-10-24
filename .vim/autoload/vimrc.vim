" ~/.vim/autoload/vimrc.vim

scriptencoding 'utf-8'

function! vimrc#shell(script) abort                                       "{{{1
  let command = join(map(split(a:script), 'expand(v:val)'))
  let winnr = bufwinnr('^' . command . '$')
  silent! execute  winnr < 0 ? 'botright vnew ' . fnameescape(command) : winnr . 'wincmd w'
  setlocal buftype=nowrite bufhidden=wipe nobuflisted noswapfile nowrap nonumber
  echo 'Execute ' . command . '...'
  silent! execute 'silent %!'. command
  silent! redraw
  silent! execute 'au BufUnload <buffer> execute bufwinnr(' . bufnr('#') . ') . ''wincmd w'''
  silent! execute 'nnoremap <silent> <buffer> <LocalLeader>r :call vimrc#shell(''' . command . ''')<CR>'
  silent! execute 'nnoremap <silent> <buffer> q :q<CR>'
  silent! execute 'AnsiEsc'
  echo 'Shell command ' . command . ' executed.'
endfunction

function! vimrc#haskell_module_name(path) abort                           "{{{1

  " <url:file:~/.vim/UltiSnips/haskell.snippets#snip:haskell:header>

  if fnamemodify(a:path, ':t') !~# '\u'
    return 'Main'
  endif

  let module = []
  for part in reverse(split(fnamemodify(a:path, ':r'), '/'))
    if part !~# '^\u' | break | endif
    let module += [part]
  endfor

  return join(reverse(module), '.')
endfunction

function! vimrc#indent_info() abort                                       "{{{1
  echom printf('sta:%s sr:%s  <ft:%s> [tw:%d et:%s wrap:%s] (sw:%d ts:%s sts:%s)  +%s+  %s:%s %s:%s:%s:%s [indk:%s] [flp:%s]',
        \ &g:sta ? 'yes' : 'no',
        \ &g:sr  ? 'yes' : 'no',
        \ &l:ft, &l:tw,
        \ &l:et   ? 'yes' : 'no',
        \ &l:wrap ? 'yes' : 'no',
        \ &l:sw,
        \ empty(&l:vts)  ? &l:ts  : &l:vts.' ('.&l:ts.')',
        \ empty(&l:vsts) ? &l:sts : &l:vsts.' ('.&l:sts.')',
        \ &l:commentstring,
        \ &l:fo,
        \ &l:ai ? 'ai' : '',
        \ &l:ci ? 'ci' : '',
        \ &l:si ? 'si' : '',
        \ &l:indentexpr,
        \ &l:lisp ? 'lisp' : '',
        \ &l:indentkeys, &l:formatlistpat)
endfunction

function! vimrc#insert_header() abort                                     "{{{1

  " This relies on UltiSnips. When snippet with trigger 'header' is in scope
  " when a new file is opened, it is automatically inserted and expanded.

  if &l:modifiable && has_key(UltiSnips#SnippetsInCurrentScope(1), 'header')
    execute ':normal iheader=UltiSnips#ExpandSnippet()'
  endif
endfun

function! vimrc#mkdir_missing(dir) abort                                  "{{{1
  if !isdirectory(a:dir)
    silent call mkdir(a:dir, 'p')
  endif
endfunction

" Modifies all numbered fold markers (open and close) by adding (subtracting)
" a:d to the foldlevel digits. When new marker level would be zero the marker is
" deleted instead.
function! vimrc#fold_renumber(d) range abort                              "{{{1
  if ! abs(a:d) > 0
    return
  endif
  let l:save = winsaveview()
  while search('[{}]\{3}\zs\d', 'cWz', a:lastline)
    exe 'normal ' . abs(a:d) . (a:d > 0 ? '' : '')
    call setline('.', substitute(getline('.'), '\s*[{}]\{3}\%#0', '', 'eI'))
  endwhile
  call winrestview(l:save)
endfunction

function! vimrc#fold_replace(level) abort                                 "{{{1
  call origami#DeleteFoldmarker()
  call origami#InsertFoldmarker('open', 'nocomment', a:level)
endfunction

" Mark current window. Execute first argument. Swap if current window is
" marked (new). Otherwise execute second arg.
function! vimrc#windowswap_wincmd(cmd, altcmd) abort                      "{{{1
  call WindowSwap#MarkWindowSwap()
  execute a:cmd
  if WindowSwap#IsCurrentWindowMarked()
    execute a:altcmd
  else
    call WindowSwap#DoWindowSwap()
  endif
endfunction

function! vimrc#deoplete_init() abort
  call deoplete#custom#option({
    \ 'skip_chars':          [],
    \ 'num_processes':       4,
    \ 'max_list':            150,
    \ 'auto_refresh_delay':  200,
    \ 'refresh_always':      v:true,
    \ 'camel_case':          v:true,
    \ 'candidate_marks':     ['0', '1', '2', '3', '4'],
    \ })
  call deoplete#custom#var('tabnine',{
    \ 'line_limit':       1000,
    \ 'max_num_results':  7,
    \ })
  call deoplete#custom#source('ultisnips',     'rank',150) " [US]
  call deoplete#custom#source('file',          'rank',150) " [F]
  call deoplete#custom#source('vim',           'rank',120) " [vim]
  call deoplete#custom#source('LanguageClient','rank',120)
  call deoplete#custom#source('tabnine',       'rank',120) " [TN]
  call deoplete#custom#source('around',        'rank',100)
  call deoplete#custom#source('buffer',        'rank',100)
  call deoplete#enable()
endfunction
