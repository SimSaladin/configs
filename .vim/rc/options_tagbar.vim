" File: ~/.vim/rc/options_tagbar.vim
" Author: Samuli Thomasson
" Description: Options for the tagbar plugin

" Wiki: <url:https://github.com/majutsushi/tagbar/wiki>

let g:tagbar_left             = 0  " (0)
let g:tagbar_width            = 50 " (40)
let g:tagbar_show_linenumbers = 1  " (0)
let g:tagbar_foldlevel        = 2  " (99)
let g:tagbar_autoshowtag      = 1  " (0)
let g:tagbar_autopreview      = 1  " (0)
let g:tagbar_silent           = 1  " (0)

" Custom type configurations {{{1

" vimwiki:vwtags
let g:vimwiki_tagbar_vimwiki = {
      \ 'ctagstype':'vimwiki',
      \ 'ctagsbin': get(g:,'myvimrc#cachedir','.').'/dein/.cache/vimrc/.dein/extras/vwtags.py',
      \ 'ctagsargs': 'markdown',
      \ 'kinds':['h:header'],
      \ 'sro':'&&&',
      \ 'kind2scope':{'h':'header'},
      \ 'sort':0
      \ }

" haskell:hasktags
let g:haskell_tagbar_hasktags = {
      \ 'ctagstype':'haskell',
      \ 'ctagsbin':'hasktags',
      \ 'ctagsargs':'-x -c -o-',
      \ 'kinds':[
          \ 'm:modules:0:1',
          \ 'd:data: 0:1',
          \ 'd_gadt: data gadt:0:1',
          \ 't:type names:0:1',
          \ 'nt:new types:0:1',
          \ 'c:classes:0:1',
          \ 'cons:constructors:1:1',
          \ 'c_gadt:constructor gadt:1:1',
          \ 'c_a:constructor accessors:1:1',
          \ 'ft:function types:1:1',
          \ 'fi:function implementations:0:1',
          \ 'o:others:0:1'
      \ ],
      \ 'sro':'.',
      \ 'kind2scope':{'m':'module','c':'class','d':'data','t':'type'},
      \ 'scope2kind':{'module':'m','class':'c','data':'d','type':'t'}
      \ }

" haskell:fasttags
let g:haskell_tagbar_fasttags = {
      \ 'ctagsbin'  : 'fast-tags',
      \ 'ctagsargs' : '-o-',
      \ 'kinds'     : [
          \  'm:modules:0:0',
          \  'c:classes:0:1',
          \  't:types:0:1',
          \  'C:constructors:0:1',
          \  'p:patterns:0:1',
          \  'o:operators:0:1',
          \  'f:functions:0:1'
      \ ],
      \ 'sro':'.',
      \ 'kind2scope':{'m':'module','c':'class','d':'data','t':'type'},
      \ 'scope2kind':{'module':'m','class':'c','data':'d','type':'t'}
      \ }

" Extra types {{{1
let g:tagbar_type_vimwiki = g:vimwiki_tagbar_vimwiki
let g:tagbar_type_haskell = g:haskell_tagbar_hasktags
