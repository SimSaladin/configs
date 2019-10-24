" ~/.vim/ftplugin/defx_maps.vim
"
" Help: <url:vimhelp:b/defx>

if exists('b:did_defx_maps')
  finish
endif
let b:did_defx_maps = 1

" normal
nnoremap <unique><silent><buffer><expr> <CR>    defx#do_action('drop')
nnoremap <unique><silent><buffer><expr> <c-l>   defx#do_action('redraw')
nnoremap <unique><silent><buffer><expr> <Space> defx#do_action('toggle_select') . 'j'
nnoremap <unique><silent><buffer><expr> <Tab>   winnr('$') != 1 ? ':<C-u>wincmd w<CR>' : ':<C-u>Defx -buffer-name=temp -split=vertical<CR>'
nnoremap <unique><silent><buffer><expr> !       defx#do_action('execute_command')
nnoremap <unique><silent><buffer><expr> *       defx#do_action('toggle_select_all')
nnoremap <unique><silent><buffer><expr> .       defx#do_action('repeat')
nnoremap <unique><silent><buffer><expr> >       defx#do_action('toggle_ignored_files')
nnoremap <unique><silent><buffer><expr> C       defx#do_action('toggle_columns', 'mark:filename:type:size:time')
nnoremap <unique><silent><buffer><expr> E       defx#do_action('open', 'vsplit')
nnoremap <unique><silent><buffer><expr> K       defx#do_action('new_directory')
nnoremap <unique><silent><buffer><expr> M       defx#do_action('new_multiple_files')
nnoremap <unique><silent><buffer><expr> N       defx#do_action('new_file')
nnoremap <unique><silent><buffer><expr> O       defx#async_action('open_tree_recursive')
nnoremap <unique><silent><buffer><expr> P       defx#do_action('open', 'pedit')
nnoremap <unique><silent><buffer><expr> S       defx#do_action('toggle_sort', 'Time')
nnoremap <unique><silent><buffer><expr> \       defx#do_action('cd', getcwd())
nnoremap <unique><silent><buffer><expr> c       defx#do_action('copy')
nnoremap <unique><silent><buffer><expr> d       defx#do_action('remove_trash')
nnoremap <unique><silent><buffer><expr> h       defx#async_action('cd', ['..'])
nnoremap <unique><silent><buffer><expr> j       line('.') == line('$') ? 'gg' : 'j'
nnoremap <unique><silent><buffer><expr> k       line('.') == 1 ? 'G' : 'k'
nnoremap <unique><silent><buffer><expr> l       defx#async_action('open')
nnoremap <unique><silent><buffer><expr> m       defx#do_action('move')
nnoremap <unique><silent><buffer><expr> o       defx#async_action('open_or_close_tree')
nnoremap <unique><silent><buffer><expr> p       defx#do_action('paste')
nnoremap <unique><silent><buffer><expr> q       defx#do_action('quit')
nnoremap <unique><silent><buffer><expr> r       defx#do_action('rename')
nnoremap <unique><silent><buffer><expr> se      defx#do_action('add_session')
nnoremap <unique><silent><buffer><expr> sl      defx#do_action('load_session')
nnoremap <unique><silent><buffer><expr> x       defx#do_action('execute_system')
nnoremap <unique><silent><buffer><expr> yy      defx#do_action('yank_path')
nnoremap <unique><silent><buffer><expr> ~       defx#async_action('cd')

" visual
xnoremap <unique><silent><buffer><expr> <CR>    defx#do_action('toggle_select_visual')
