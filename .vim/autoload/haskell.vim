" ~/.vim/autoload/haskell.vim

" Section: Hindent

let g:haskell#hindent_program = 'hindent'

function! haskell#hindent() range                                         "{{{1
  let prog = g:haskell#hindent_program
  if !executable(prog)
    echomsg 'hindent not found in $PATH, did you install it?'
    return
  endif
  let cmd = prog .
    \ ' --indent-size ' . b:hindent_indent_size .
    \ ' --line-length ' . b:hindent_line_length .
    \ b:hindent_sort_imports ? ' --sort-imports' : ''
  let winview = winsaveview()
  silent! exe 'w !' . cmd . ' >/dev/null 2>&1'
  if v:shell_error
    echohl WarningMsg
    echo 'Hindent: Parsing error\n'
    echohl None
  else
    silent! exe 'undojoin'
    silent! exe 'keepjumps '.a:firstline.','.a:lastline.'!'.cmd
  endif
  call winrestview(winview)
endfunction "}}}1

" Section: Folding

let s:haddockSec1  = '^\s*-- \*\_[^*]'
let s:haddockSec2  = '^\s*-- \*\{2}\_[^*]'
let s:haddockSec3  = '^\s*-- \*\{3}\_[^*]'
let s:pragmaOpen   = '^\s*{-#'
let s:pragmaClose  = '#-}\s*$'
let s:haddockLine  = '^\s*-- [|^$]'
let s:space        = '^\s*$'
let s:lineComment  = '^\s*---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$'
let s:blockComment = '\v^\s*(\{-.{-})?(.{-}-\})?\s*$'

function! s:is_space(lnum)                                                "{{{
  return getline(a:lnum) =~# '^\s*$'
endfunction "}}}

function! s:lineseek(lnum, n, pat)                                        "{{{
  " Find closest line that matches pat
  let lnum = a:lnum
  while lnum > 0 && lnum <= line('$')
    if getline(lnum) =~# a:pat
      return lnum
    else
      let lnum += a:n
    endif
  endwhile
  return 0
endfunction "}}}

function! s:indentseek(lnum, n)                                           "{{{
  " Find indent level of the closest line > a:lnum that isn't empty
  return indent(s:lineseek(a:lnum + a:n, a:n, '\S'))
endfunction "}}}

function! s:top_of(lnum, d) abort                                         "{{{
  let res = 0
  if a:d < 0 | let res = s:lineseek(a:lnum, -1, '^\S')
  else | let res = s:lineseek(s:lineseek(a:lnum, 1, '\S'), -1, '^\S')
  endif
  while res > 1 && getline(res - 1) =~# '^\S'
    let res -= 1
  endwhile
  return res
endfunction "}}}

fun! haskell#foldtext() abort                                             "{{{1
    " From: github.com/Twinside/vim-haskellFold/autoload/haskellFold.vim
    let i = v:foldstart
    let retVal = ''
    let began = 0

    let commentOnlyLine = '^\s*--.*$'
    let monoLineComment = '\s*--.*$'
    let nonEmptyLine    = '^\s\+\S'
    let emptyLine       = '^\s*$'
    let multilineCommentBegin = '^\s*{-'
    let multilineCommentEnd = '-}'

    let short = get(g:, 'haskellFold_ShortText', 0)
    let isMultiLine = 0

    let line = getline(i)
    while i <= v:foldend

        if isMultiLine
            if line =~ multilineCommentEnd
                let isMultiLine = 0
                let line = substitute(line, '.*-}', '', '')

                if line =~ emptyLine
                    let i = i + 1
                    let line = getline(i)
                end
            else
                let i = i + 1
                let line = getline(i)
            end
        else
            if line =~ multilineCommentBegin
                let isMultiLine = 1
                continue
            elseif began == 0 && !(line =~ commentOnlyLine)
                let retVal = substitute(line, monoLineComment, ' ','')
                let began = 1
            elseif began != 0 && line =~ nonEmptyLine && !short
                let tempVal = substitute( line, '\s\+\(.*\)$', ' \1', '' )
                let retVal = retVal . substitute(tempVal, '\s\+--.*', ' ','')
            elseif began != 0
                break
            endif

            let i = i + 1
            let line = getline(i)
        endif
    endwhile

    if retVal ==# ''
        " We didn't found any meaningfull text
        return foldtext()
    endif

    return retVal
endfunction "}}}1

fun! haskell#foldexpr(lnum)                                               "{{{1
  " An alternative fold expression (folds more aggressively and is buggy bugs).
  let l:curln = getline(a:lnum)
  if     l:curln =~# '^module\>.*where' | return 0
  elseif l:curln =~# '^module\>' | return '>1'
  elseif l:curln =~# '^import\>' | return getline(a:lnum - 1) =~# '^\(import\|\s\)' ? 1 : '>1'
  elseif l:curln =~# '^-- \*\*\*' | return '>3'
  elseif l:curln =~# '^-- \*\*' | return '>2'
  elseif l:curln =~# '^-- \*' | return '>1'
  elseif l:curln =~# s:space
    if     (getline(a:lnum + 1) =~# '^-- \*\*\*') | return '3'
    elseif (getline(a:lnum + 1) =~# '^-- \*\*') | return '2'
    elseif (getline(a:lnum + 1) =~# '^-- \*') | return '1'
    elseif (getline(a:lnum - 1) =~# '^-- \*') | return '-1'
    endif

  " current indent == next* indent == prev* indent
  elseif indent(a:lnum) == s:indentseek(a:lnum, 1)
    if indent(a:lnum) == 0
      if getline(a:lnum - 1) =~# s:space
        if getline(a:lnum + 1) =~# s:space
          return -1
        else
          return 'a1'
        endif
      elseif getline(a:lnum + 1) =~# s:space
        return 's1'
      endif
    endif

  " current indent < next* indent
  elseif indent(a:lnum) < s:indentseek(a:lnum, 1)
    let l:n = ( min([8, s:indentseek(a:lnum, 1)]) - min([8, indent(a:lnum)]))/2
    if indent(a:lnum) == 0 && getline(a:lnum - 1) !~# s:space
      let l:n -= 1
    endif
    return (l:n > 0) ? ('a'.string(l:n )) : '='

  " current indent > next* indent
  elseif indent(a:lnum) > s:indentseek(a:lnum, 1)
    let l:n = (min([8, indent(a:lnum)]) - min([8, s:indentseek(a:lnum, 1)]))/2
    if s:indentseek(a:lnum, 1) == 0 && getline(a:lnum + 1) !~# s:space
      let l:n -= 1
    endif
    return (l:n >= 1) ? ('s' . string(l:n)) : '='

  endif
  " anything else
  return '='
endfunction "}}}1
