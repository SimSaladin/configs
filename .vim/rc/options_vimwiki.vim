
let g:vimwiki_hl_headers                = 1 " Use different colors for headers
let g:vimwiki_hl_cb_checked             = 1 " highlight only first line of [X]
let g:vimwiki_html_header_numbering     = 1
let g:vimwiki_html_header_numbering_sym = '.'
let g:vimwiki_folding                   = 'expr'
let g:vimwiki_listsym_rejected          = '✗'
let g:vimwiki_table_mappings  = 0
let g:vimwiki_map_prefix      = '<leader>_ww'
" Note: this prefix is also used for couple <buffer> mappings(why?)
"       <prefix>r @<Plug>VimwikiRenameLink
"       <prefix>d @<Plug>VimwikiDeleteLink

let g:vimwiki_list = [{
      \ 'path'             : '~/Documents/notes/',
      \ 'path_html'        : '~/Documents/notes_html/',
      \ 'ext'              : '.wmd',
      \ 'syntax'           : 'markdown',
      \ 'list_margin'      : 0,
      \ 'auto_toc'         : 1,
      \ 'auto_tags'        : 1,
      \ 'auto_diary_index' : 1,
      \ 'maxhi'            : 1
      \ }]

