" File: ~/.vim/ftplugin/vimwiki_maps.vim
" Author: Samuli Thomasson

scriptencoding utf-8

if exists('b:did_local_vimwiki_mappings')
  finish
endif
let b:did_local_vimwiki_mappings = 1

nmap <silent><buffer> <localleader>> <Plug>VimwikiAddHeaderLevel
nmap <silent><buffer> <localleader>< <Plug>VimwikiRemoveHeaderLevel
nmap <silent><buffer> <c-j>          <Plug>VimwikiNextLink
nmap <silent><buffer> <c-k>          <Plug>VimwikiPrevLink
nmap <silent><buffer> L              <Plug>VimwikiNormalizeLink
vmap <silent><buffer> L              <Plug>VimwikiNormalizeLinkVisual
nmap <silent><buffer> <localleader>d <Plug>VimwikiDeleteLink
nmap <silent><buffer> <localleader>l <Plug>VimwikiRenameLink

