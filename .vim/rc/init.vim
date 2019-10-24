" ~/.vim/rc/init.vim

" Load once                                                               {{{1
if exists('g:did_myvimrc_init')
  finish
endif
let g:did_myvimrc_init = 1

let g:myvimrc#initrtp = &g:rtp

language message C

" Leaders expand when a mapping is defined, so it's useful to define them early.
let g:mapleader      = ','
let g:maplocalleader = '\'

" Files and directories                                                   {{{1

if empty($XDG_CACHE_HOME)
  call setenv('XDG_CACHE_HOME',$HOME.'/.cache')
endif

if !exists('g:myvimrc#cachedir')
  let g:myvimrc#cachedir = $XDG_CACHE_HOME . '/vim'
endif
if !isdirectory(g:myvimrc#cachedir)
  call mkdir(g:myvimrc#cachedir, 'p', 0700)
endif

" Note: directory, backupdir, undodir are set in /usr/share/vim/vimfiles/archlinux.vim on ArchLinux.
let &g:directory   = g:myvimrc#cachedir . '/swap//'
let &g:backupdir   = g:myvimrc#cachedir . '/backup//'
let &g:undodir     = g:myvimrc#cachedir . '/undo//'
let &g:viewdir     = g:myvimrc#cachedir . '/view//'
let &g:viminfofile = g:myvimrc#cachedir . '/viminfo'

" Note: system() does not consider 'shelltemp', so we setup $TMPDIR if it doesn't exist yet.
if empty($TMPDIR) || $TMPDIR ==# '/tmp'
  if isdirectory($XDG_RUNTIME_DIR) && filewritable($XDG_RUNTIME_DIR)
    call setenv('TMPDIR', $XDG_RUNTIME_DIR . '/vim')
  else
    call setenv('TMPDIR', $XDG_CACHE_HOME . '/vim/tmp')
  endif
endif

if !isdirectory($TMPDIR)
  call mkdir($TMPDIR, '', 0700)
endif

let g:myvimrc#runtimedir = $TMPDIR

" Standard plugins                                                        {{{1

" Skip loading default plugins
let g:loaded_getscriptPlugin  = 1
let g:loaded_gzip             = 1
let g:loaded_logiPat          = 1
let g:loaded_matchparen       = 1
let g:loaded_netrwPlugin      = 1
let g:loaded_rrhelper         = 1
let g:loaded_spellfile_plugin = 1
let g:loaded_tarPlugin        = 1
let g:loaded_2html_plugin     = 1
let g:loaded_vimballPlugin    = 1
let g:loaded_zipPlugin        = 1

" See <url:vimhelp:ft-bsah-syntax> and syntax/sh.vim
let g:is_bash         = 1
let g:sh_fold_enabled = 7 " For foldmethod=syntax

" See <url:vimhelp:ft-man-plugin>
let g:ft_man_folding_enable = 1

" Extra                                                                   {{{1

" The cecutil plugin is included in so many other plugins
let g:no_cecutil_maps = 1

" $VIMRUNTIME/macros/less.sh
let g:less        = get(g:,'less',{})
let g:less.number = 0

" plugin/vimpager.vim - gvim ansiesc passthrough enabled ptree
let g:vimpager     = get(g:,'vimpager',{})
let g:vimpager.X11 = has('gui_running')
