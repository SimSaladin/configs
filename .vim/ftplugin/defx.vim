" File: ~/.vim/ftplugin/defx.vim
" Author: Samuli Thomasson
" Description: Defx buffer mappings. <url:vimhelp:defx.txt>

scriptencoding utf-8
if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

nnoremap <silent> <buffer> <expr> <CR> defx#async_action('drop')
nnoremap <silent> <buffer> <expr> c defx#do_action('copy')
nnoremap <silent> <buffer> <expr> !  defx#do_action('execute_command')
nnoremap <silent> <buffer> <expr> m defx#do_action('move')
nnoremap <silent> <buffer> <expr> p defx#do_action('paste')
nnoremap <silent> <buffer> <expr> l defx#async_action('open')
nnoremap <silent> <buffer> <expr> E defx#do_action('open', 'vsplit')
nnoremap <silent> <buffer> <expr> P defx#do_action('open', 'pedit')
nnoremap <silent> <buffer> <expr> o defx#async_action('open_or_close_tree')
nnoremap <silent> <buffer> <expr> O defx#async_action('open_tree_recursive')
nnoremap <silent> <buffer> <expr> K defx#do_action('new_directory')
nnoremap <silent> <buffer> <expr> N defx#do_action('new_file')
nnoremap <silent> <buffer> <expr> M defx#do_action('new_multiple_files')
nnoremap <silent> <buffer> <expr> C defx#do_action('toggle_columns', 'mark:filename:type:size:time')
nnoremap <silent> <buffer> <expr> S defx#do_action('toggle_sort', 'Time')
nnoremap <silent> <buffer> <expr> se defx#do_action('add_session')
nnoremap <silent> <buffer> <expr> sl defx#do_action('load_session')
nnoremap <silent> <buffer> <expr> d defx#do_action('remove_trash')
nnoremap <silent> <buffer> <expr> r defx#do_action('rename')
nnoremap <silent> <buffer> <expr> x defx#do_action('execute_system')
nnoremap <silent> <buffer> <expr> > defx#do_action('toggle_ignored_files')
nnoremap <silent> <buffer> <expr> . defx#do_action('repeat')
nnoremap <silent> <buffer> <expr> yy defx#do_action('yank_path')
nnoremap <silent> <buffer> <expr> h defx#async_action('cd', ['..'])
nnoremap <silent> <buffer> <expr> ~ defx#async_action('cd')
nnoremap <silent> <buffer> <expr> \ defx#do_action('cd', getcwd())
nnoremap <silent> <buffer> <expr> q defx#do_action('quit')
nnoremap <silent> <buffer> <expr> <Space> defx#do_action('toggle_select') . 'j'
nnoremap <silent> <buffer> <expr> * defx#do_action('toggle_select_all')
nnoremap <silent> <buffer> <expr> j line('.') == line('$') ? 'gg' : 'j'
nnoremap <silent> <buffer> <expr> k line('.') == 1 ? 'G' : 'k'
nnoremap <silent> <buffer> <expr> <C-l> defx#do_action('redraw')
nnoremap <silent> <buffer> <expr> <Tab> winnr('$') != 1 ? ':<C-u>wincmd w<CR>' : ':<C-u>Defx -buffer-name=temp -split=vertical<CR>'
xnoremap <silent> <buffer> <expr> <CR>  defx#do_action('toggle_select_visual')
