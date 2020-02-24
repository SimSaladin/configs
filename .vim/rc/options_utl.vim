" File: ~/.vim/rc/options_utl.vim
" Author: Samuli Thomasson
" Description: Configuration for the UTL plugin
" User Guide: <url:vim:utl_usr.txt>
" Config File: <url:config:>
" Command Utl: <url:vimscript:Utl help commands>

let g:utl_opt_verbose        = 0
let g:utl_opt_highlight_urls = 'yes' " yes|no

"function! s:setup()
"  syn region myUrl matchgroup=myUrlTag start=+\v\c\<(url|lnk):+ end=+>+ oneline containedin=ALL
"  hi def link myUrl Underlined
"endfun

" Schemas:

" ftp:    delegates to http (web browser)
" https:  delegates to http (web browser)
" mailto: delegates to mail client: g:utl_cfg_hdl_scm_mailto
" scp:    uses vim's netrw: g:utl_cfg_hdl_scm_scp
" man:
" vimhelp:
" vimscript:

" Handlers (mediatype)
let g:utl_cfg_hdl_mt_generic                 = "silent !xdg-open '%p'" " The generic handler
let g:utl_cfg_hdl_mt_text_directory          = 'VIM' " directories: text/directory
let g:utl_cfg_hdl_mt_text_directory__vim     = 'VIM' " <url:/etc> VIM = vim's netrw (or it's substitute)
let g:utl_cfg_hdl_mt_text_html               = "!qutebrowser -r default '%u'"
let g:utl_cfg_hdl_mt_application_pdf__evince = "silent !evince '%u#%f'"

" Handlers (schema)
let g:utl_cfg_hdl_scm_mailto                 = "!neomutt '%u'"

let g:utl_cfg_hdl_scm_http                   = "silent !xdg-open '%u#%f'"
let g:utl_cfg_hdl_scm_http_system            = "silent !xdg-open '%u#%f'"
let g:utl_cfg_hdl_scm_http__wget             = "call Utl_if_hdl_scm_http__wget('%u')"

let g:utl_cfg_hdl_scm_scp                    = 'silent %d %u'
