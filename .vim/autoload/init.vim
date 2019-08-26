" File: ~/.vim/autoload/init.vim
" Author: Samuli Thomasson

function! init#source_rc(path) abort "{{{1
  let abspath = resolve(expand('~/.vim/rc/' . fnamemodify(a:path.'.vim', ':t')))
  if filereadable(abspath)
    execute 'source' fnameescape(abspath)
  else
    echo 'File '.abspath.' not readable!'
  endif
endfun "}}}1

function! init#setup_dein() abort "{{{1
  let s:dein_url = 'github.com/Shougo/dein.vim'
  " setup plugin manager
  let l:dir = g:deindir . '/repos/' . s:dein_url
  if &runtimepath !~ l:dir
    if !isdirectory(l:dir)
      execute '!git clone https://'.s:dein_url fnameescape(l:dir)
    endif
    execute 'set runtimepath^=' . l:dir
  endif
endfunction "}}}1

function! init#do_plugins() abort "{{{1

  call init#setup_dein()

  let g:dein#auto_recache           = 1
  let g:dein#install_progress_type  = 'title'
  let g:dein#enable_notification    = 1
  let g:dein#notification_time      = 10
  let g:dein#enable_name_conversion = 1
  let g:dein#install_log_filename   = $TMPDIR.'/dein.log'

  let s:dein_lazy   = $HOME.'/.vim/deinlazy.toml'
  let s:dein_normal = $HOME.'/.vim/dein.toml'

  call init#source_rc('plugin-vars')

  if !dein#load_state(g:deindir)
    return
  endif

  call dein#begin(g:deindir, [expand('$MYVIMRC')]) " s:dein_toml, s:dein_lazy_toml])
  call dein#load_toml(s:dein_lazy, {'lazy': 1})
  call dein#load_toml(s:dein_normal, {'lazy': 0})
  call dein#end()
  call dein#save_state()

  if !has('vim_starting') && dein#check_install()
    call dein#install()
  endif

  if !has('vim_starting')
    call dein#call_hook('source')
    call dein#call_hook('post_source')
  endif
endfunction "}}}1

function! init#do_syntax() abort "{{{1
  filetype plugin indent on
  syntax on
  colorscheme solarized
  if has('vim_starting')
    return
  endif

  " XXX Workaround when re-sourcing quickfixsigns vcsdiff
  if exists('#QuickFixSignsVscdiff#ColorScheme')
    doautocmd QuickFixSignsVscdiff ColorScheme
  endif

  " XXX Workaround when re-sourcing utl
  if exists('#utl_highl#BufWinEnter')
    doautocmd utl_highl BufWinEnter
  endif

  " XXX Workaround re-sourcing airline
  if exists(':AirlineRefresh')
    execute 'AirlineRefresh'
  endif
endfunction "}}}1
