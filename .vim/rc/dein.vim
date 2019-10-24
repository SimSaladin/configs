" File: ~/.vim/rc/dein.vim

let s:dein_base    = g:myvimrc#cachedir.'/dein'
let s:dein_install = s:dein_base.'/repos/github.com/Shougo/dein.vim'
let s:dein_src     = 'https://github.com/Shougo/dein.vim'
let s:toml         = resolve($HOME.'/.vim/dein.toml')
let s:toml_lazy    = resolve($HOME.'/.vim/lazy.dein.toml')
let s:toml_ft      = resolve($HOME.'/.vim/ft.dein.toml')

let g:dein#auto_recache           = 1
let g:dein#enable_name_conversion = 1
let g:dein#enable_notification    = 1
let g:dein#install_progress_type  = 'title'
let g:dein#notification_time      = 10
let g:dein#install_log_filename   = s:dein_base . '/install-'.fnamemodify($MYVIMRC,':t:r:r').'.log'

if !init#setup_dein(s:dein_install, s:dein_src)
  finish
endif

" Load the cache script. Returns 1 if cache script is old/invalid/not found.
if dein#load_state(s:dein_base)

  call dein#begin(s:dein_base, [$MYVIMRC, expand('<sfile>'), s:toml, s:toml_ft, s:toml_lazy])

  call dein#load_toml(s:toml, {'lazy': 0})
  call dein#load_toml(s:toml_ft, {'lazy': 0})
  call dein#load_toml(s:toml_lazy, {'lazy': 1})

  " Changes runtimepath
  call dein#end()

  " Write the cache script
  call dein#save_state()

  " Install plugins (async)
  if !has('vim_starting') && dein#check_install()
    call dein#install()
  endif
endif

"call dein#call_hook('source')

" The post_source hooks are not called automatically when vim is starting
if has('vim_starting')
  autocmd VimEnter * ++once call dein#call_hook('post_source')
endif

if !has('vim_starting') && exists('#myvimrc#VimEnter')
  doautocmd myvimrc VimEnter
  " XXX: ftplugins not applied?
endif
