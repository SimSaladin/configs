" File: ~/.vim/autoload/vimrc.vim
" Description: Misc. functions

scriptencoding utf-8

function! vimrc#shell(script) abort "{{{
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
endfunction "}}}

function! vimrc#haskell_module_name(path) abort    " {{{
  "  See <url:file:~/.vim/UltiSnips/haskell.snippets#snip:haskell:header>
  if fnamemodify(a:path, ':t') =~# '\u'
    let module = []
    for part in reverse(split(fnamemodify(a:path, ':r'), '/'))
      if part =~# '^\u'
        let module += [part]
      else
        break
      endif
    endfor
    return join(reverse(module), '.')
  else
    return 'Main'
  endif
endfun "}}}

function! vimrc#indent_info() abort   " {{{
  echom printf('&g:%s  [%s %d/%s%s]  %d>> %s/%s  +%s+  %s:%s %s:%s:%s:%s [indk:%s]',
        \ &g:sta ? 'smarttab' : 'nosta',
        \ &l:ft, &l:tw, &l:et ? 'et' : 'noet', &l:wrap ? '/wrap' : '',
        \ &l:sw,
        \ empty(&l:vts)  ? &l:ts  : &l:vts.' ('.&l:ts.')',
        \ empty(&l:vsts) ? &l:sts : &l:vsts.' ('.&l:sts.')',
        \ &l:cms,
        \ &l:fo,
        \ &l:ai ? 'ai' : '',
        \ &l:ci ? 'ci' : '',
        \ &l:si ? 'si' : '',
        \  &l:indentexpr,
        \ &l:lisp ? 'lisp' : '',
        \ &l:indentkeys)
endfunction   " }}}

function! vimrc#insert_header() abort " {{{
  " This relies on UltiSnips. When snippet with trigger 'header' is in scope
  " when a new file is opened, it is automatically inserted and expanded.
  if &l:modifiable && has_key(UltiSnips#SnippetsInCurrentScope(1), 'header')
    execute ':normal iheader=UltiSnips#ExpandSnippet()'
  endif
endfun "}}}

function! vimrc#mkdir_missing(dir) abort "{{{
  if !isdirectory(a:dir)
    silent call mkdir(a:dir, 'p')
  endif
endfunction "}}}
