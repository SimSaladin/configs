" ~/.vim/ftplugin/denite_maps.vim

if exists('b:did_denite_maps')
  finish
endif
let b:did_denite_maps = 1

nnoremap <silent><buffer><expr> <CR>    denite#do_map('do_action')
nnoremap <silent><buffer><expr> a       denite#do_map('choose_action')
nnoremap <silent><buffer><expr> d       denite#do_map('do_action', 'delete')
nnoremap <silent><buffer><expr> p       denite#do_map('do_action', 'preview')
nnoremap <silent><buffer><expr> q       denite#do_map('quit')
nnoremap <silent><buffer><expr> '       denite#do_map('quick_move')
nnoremap <silent><buffer><expr> f       denite#do_map('open_filter_buffer')
nnoremap <silent><buffer><expr> <Space> denite#do_map('toggle_select').'j'
nnoremap <silent><buffer><expr> r       denite#do_map('do_action', 'quickfix')
nnoremap <silent><buffer><expr> <C-r>   denite#do_map('restore_sources')
nnoremap <silent><buffer><expr> <F1>    ":\<C-u>Utl openLink file:~/.vim/deinlazy.toml#denite\<CR>"
