" ~/.vim/ftplugin/vimwiki_maps.vim

if exists('b:did_vimwiki_maps')
  finish
endif
let b:did_vimwiki_maps = 1

nmap <unique><silent><buffer> t              <Plug>VimwikiTabnewLink
nmap <unique><silent><buffer> <localleader>> <Plug>VimwikiAddHeaderLevel
nmap <unique><silent><buffer> <localleader>< <Plug>VimwikiRemoveHeaderLevel
nmap <unique><silent><buffer> <c-j>          <Plug>VimwikiNextLink
nmap <unique><silent><buffer> <c-k>          <Plug>VimwikiPrevLink
nmap <unique><silent><buffer> L              <Plug>VimwikiNormalizeLink
vmap <unique><silent><buffer> L              <Plug>VimwikiNormalizeLinkVisual
nmap <unique><silent><buffer> <localleader>d <Plug>VimwikiDeleteLink
nmap <unique><silent><buffer> <localleader>l <Plug>VimwikiRenameLink
