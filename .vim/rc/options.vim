" ~/.vim/rc/options.vim
" See also :options window

" encoding                                                                {{{1
setg encoding=utf-8   " "global" encoding (latin1 or from "$LANG")

" cpoptions                                                               {{{1
setg cpoptions& " "global" cpoptions (aABceFs)
setg cpo+=i     " *cpo-i* Interrupting the reading of a file will leave it modified.
setg cpo+=I     " *cpo-I* When moving the cursor up or down just after inserting indent for 'autoindent', do not delete the indent.
setg cpo+=o     " *cpo-o* Line offset to search command is not remembered for next search.
setg cpo+=t     " *cpo-t* Search pattern for the tag command is remembered for "n" command.
setg cpo+=w     " *cpo-w* When using "cw" on a blank character, only change one character and not all blanks until the start of the next word.
setg cpo+=X     " *cpo-X* When using a count with "R" the replaced text is deleted only once.  Also when repeating "R" with "." and a count.
setg cpo+=$     " *cpo-$* When making a change to one line, don't redisplay the line, but put a '$' at the end of the changed text.
setg cpo-=s     " *cpo-s* Set buffer options when entering buffer for first time.

" common                                                                  {{{1
if has('filterpipe')
  setg noshelltemp       " "global" shelltemp (on) - When off, prefer pipe in shell commands.
endif
if has('python3')
  setg pyxversion=3      " "global" pyxversion ('')
endif
if !has('gui_running')
  setg background=dark   " "global" background (Vim tries to guess if not set (and easily fails on terminals))
endif
if has('ttyout')
  setg term=$TERM        " "global" term / ttytype ($TERM)
  setg nottybuiltin      " "global" ttybuiltin (on)
  setg ttyfast           " "global" ttyfast (...)
  setg ttyscroll=3       " "global" ttyscroll (999) - set small if redrawing is fast
  setg termguicolors&    " "global" termguicolors (off) - use 24bit colors
  setg mouse=            " "global" mouse ("a") - enable/disable mouse
  setg visualbell        " "global" visualbell (off) - visual bell instead of beeping
  setg t_vb=             " "global" t_vb (...) - bell character (empty for no bell)
endif
setg secure              " "global" secure (off)
setg title               " "global" title (off) (filename [+=-] (path) - VIM) TODO titlestring, titlelen
setg packpath=           " "global" packpath (...)
setg runtimepath&        " "global" runtimepath (...)
setg rtp-=/usr/share/vim/vimfiles
setg rtp-=/usr/share/vim/vimfiles/after
setg sessionoptions&     " "global" sessionoptions (....) Note: don't include 'folds' if using FastFold
setg ssop-=folds
setg ssop-=curdir
setg ssop-=options
setg ssop+=sesdir,localoptions
setg directory=$XDG_CACHE_HOME/vim/swap//
if has('mksession')
  setg viewdir=$XDG_CACHE_HOME/vim/view
endif
if has('viminfo')
  setg viminfofile=$XDG_CACHE_HOME/vim/viminfo
endif

" GUI                                                                     {{{1
setg guioptions&         " "global" guioptions ("egmrLT")
setg go+=!fc
setg go-=rL

" statusline, prompt, wildmenu                                            {{{1
setg laststatus=2        " "global" laststatus (1)
setg noshowcmd           " "global" showcmd (on)"
setg noshowmode          " "global" showmode (on)
setg confirm             " "global" confirm (off)
setg cmdheight=2         " "global" cmdheight (1)
setg shortmess&          " "global" shortmess ("filnxtToOS")
setg shm=atToOcFA
setg nowildmenu          " "global" wildmenu (off)
setg wildchar=<tab>      " "global" wildchar (<tab>)
setg wildcharm=<tab>     " "global" wildcharm (none 0)
setg wildmode&           " "global" wildmode (full)
setg wim=list:longest,full
setg tabpagemax=50      " "global" tabpagemax (10)

" edit                                                                    {{{1
setg pastetoggle=<F10>  " "global" pastetoggle ('')
setg tildeop            " "global" tildeop (off)
setg backspace-=eol     " "global" backspace ("indent,eol,start")
setg nojoinspaces       " "global" joinspaces (on)
setg virtualedit=block  " "global" virtualedit ("")
setg nostartofline      " "global" startofline (on)
setg whichwrap&         " "global" whichwrap ("b,s")
setg ww-=b,s
setg ww+=<,>,[,]
setg timeoutlen=5000    " "global" ms, timeout to complete mappings (1000)
setg ttimeoutlen=500    " "global" ms timeout to complete keycodes, if different than timeoutlen for maps (-1)
setg esckeys            " "global" (on) do <Esc> escapes in insert mode
setg digraph<           " "global" digraph (off)

" formatting                                                              {{{1
setg breakat&           " "global" breakat (^I!@*-+;:,./?)
setg smarttab&          " "global" smarttab (off)
setg shiftround&        " "global" shiftround (off)
setg tabstop&           " "local (buffer)" tabstop (8)
setg shiftwidth=0       " "local (buffer)" shiftwidth (8)
setg softtabstop=-1     " "local (buffer)" softtabstop (0)
setg textwidth&         " "local (buffer)" textwidth (0)
setg expandtab&         " "local (buffer)" expandtab (off)
setg autoindent         " "local (buffer)" autoindent (off)
setg formatoptions&     " "local (buffer)" formatoptions ("tcq")
setg fo+=ronlj
setl ts< sw< sts< tw< et< ai< fo<

" search                                                                  {{{1
setg ignorecase         " "global" ignorecase (off)
setg smartcase          " "global" smartcase (off)
setg incsearch          " "global" incsearch (off)
setg showmatch          " "global" showmatch (off)
setg matchtime=2        " "global" matchtime (5)
setg matchpairs&        " "local (buffer)" matchpairs ("(:),{:},[:]")
setg mps+=<:>,^:$
setg makeprg&           " "global-local (buffer)" makeprg (make)
setg errorformat&       " "global-local (buffer)" errorformat (...)
setg grepprg=grep\ -snPHi\ --color=always\ -m50
                        " "global-local (buffer)" grepprg ("grep -n $* /dev/null")
setg grepformat=        " "global" grepformat (%f:%l:%m,...)
setg gfm^=%f:%l:%m
setg gfm^=%\\e%*[^K]K%f%*[^:]:%*[^K]K%*[^K]K%l%*[^:]:%*[^K]K%m
setl mps< mp< gp< gfm< efm<

" Buffer                                                                  {{{1
setg hidden             " "global" hidden (off)
setg switchbuf&         " "global" switchbuf ("")
setg swb+=useopen,split
setg lazyredraw         " "global" lazyredraw (off)
setg errorbells         " "global" errorbells (off)
setg autowrite&         " "global" autowrite (off)
setg autoread           " "global-local (buffer)" autoread (off)
setl ar<

" modelines                                                               {{{1
setg modelines=2        " "global" modelines (5)
setg modeline&          " "local (buffer)" modeline (on) (off for root)
setl ml<

" window                                                                  {{{1
"set helpheight=20      " "global" helpheight (20)
"set previewheight=12   " "global" previewheight (12)
"setg equalalways       " "global" equalalways (on)
setg eadirection=ver    " "global" eadirection (both)
setg winheight=12       " "global" winheight (1)
setg winwidth=20        " "global" winwidth (20)
setg winminheight=7     " "global" winminheight (1)
setg winminwidth=20     " "global" winminwidth (1)
setg splitbelow         " "global" splitbelow (off)
setg splitright         " "global" splitright (off)
setg scrolljump=5       " "global" (0)
setg sidescroll=5       " "global" (0)
setg scrolloff=256      " "global-local (window)" scrolloff (5)
setg sidescrolloff=40   " "global-local (window)" (0)
setl so< siso<
setg nowrap             " "local (window)" wrap (on)
setg linebreak          " "local (window)" linebreak (off)
setg breakindent        " "local (window)" breakindent (off)
setg breakindentopt=    " "local (window)" breakindentopt ('')
setg nonumber           " "local (window)" number (off)
setg relativenumber     " "local (window)" relativenumber (off)
setg numberwidth=2      " "local (window)" numberwidth (4)
setg cursorline         " "local (window)" cursorline (off)
setg signcolumn=number  " "local (window)" signcolumn ("auto")
setl wrap< lbr< bri< briopt< nu< rnu< nuw< cul< scl<

" conceal                                                                 {{{1
setg conceallevel=2     " "local (window)" conceallevel (0)
setg concealcursor=nc   " "local (window)" concealcursor ("")
setl cole< cocu<

" folding                                                                 {{{1
setg foldlevelstart=1   " "global" foldlevelstart (-1)
setg foldopen-=block    " "global" foldopen ("block,hor,mark,percent,quickfix,search,tag,undo")
setg fdo-=undo
setg foldmethod=marker  " "local (window)" foldmethod ("manual")
setg foldcolumn=1       " "local (window)" foldcolumn (0)
setg foldignore=        " "local (window)" foldignore ("#")
setl fdm< fdc< fdi<

" history                                                                 {{{1
setg history=700        " "global" history (200)

" undo                                                                    {{{1
setg undoreload=15000   " "global" undoreload (10000)
setg undolevels=500     " "global-local (buffer)" undolevels (1000)
setl ul<
if has('persistent_undo')
  setg undodir=$XDG_CACHE_HOME/vim/undo//
                        " "global" undodir
  setg undofile         " "local (buffer)" undofile (off)
  setl udf<
endif

" backup                                                                  {{{1
setg backupdir=$XDG_CACHE_HOME/vim/backup//,.
setg backup             " "global" backup (off)
setg writebackup        " "global" writebackup (off)
setg backupskip&        " "global" backupskip (/tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*)
setg bsk+=/run/*
setg bsk+=/dev/*
setg bsk+=*/.ssh/id*
setg bsk+=*_EDITMSG
setg backupcopy&        " "global-local (buffer)" backupcopy (auto)
setl bkc<
