" File: ~/.vim/rc/options_yankring.vim
" Author: Samuli Thomasson
" Description: Options for YankRing plugin

scriptencoding utf-8

" Options
let g:yankring_history_dir     = g:myvimrc#cachedir

" Keys
let g:yankring_n_keys          = 'Y D x X'
let g:yankring_o_keys          = 'b B w W e E d y $ G;  iW aw aW'
let g:yankring_zap_keys        = 'f F t T / ?'
let g:yankring_ignore_operator = 'g~ gu gU != gq g? > < zf g@'
let g:yankring_v_key           = 'v'
let g:yankring_del_v_key       = 'd x'
let g:yankring_paste_n_bkey    = 'P'
let g:yankring_paste_n_akey    = 'p'
let g:yankring_paste_v_key     = 'P'

" Maps
let g:yankring_replace_n_pkey  = '<C-P>'
let g:yankring_replace_n_nkey  = '<C-N>'
