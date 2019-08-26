" File: ~/.vim/rc/options.vim
" Author: Samuli Thomasson
" Description: Standard vim options

scriptencoding utf-8

" TODO: <url:vimhelp:Terminal-Normal>
" TODO: setlocal define  include  includeexpr
" TODO: vim: set commentstring=\ \"%s
" TODO  <url:#tn=patchmode> toggle
" TODO  <url:vimhelp:keywordprg>

" Section division below is the same as in :options.

" Section:  1 important                                                   {{{1

setglobal pt=<F10>            " (global) pastetoggle

" Section:  2 moving around, searching and patterns                       {{{1

setglobal whichwrap=b,s       " (global) which commands wrap to another line
setglobal nostartofline       " (global) <C-U> and similar remain in cursor column.
setglobal noautochdir         " (global) change to directory of file in buffer
setglobal wrapscan            " (global) Searches wrap around the end of the file.
setglobal incsearch           " (global) show match for partly typed search command
setglobal ignorecase          " (global) Search patterns ignore case.
setglobal smartcase           " (global) override 'ignorecase' when pattern has upper case characters

" Section:  3 tags                                                        {{{1

" Section:  4 displaying text                                             {{{1

setglobal scrolloff=256       " (global-local to window)
setglobal nowrap              " (local to window) visually (only) wrap lines longer than window
setglobal linebreak           " (local to window) if wrap: wrap on "breakat" and don't insert <EOL>s
setglobal lazyredraw          " (global)
setglobal nonumber            " (local to window) show the line number for each line
setglobal relativenumber      " (local to window) show the relative line number for each line
setglobal numberwidth=1       " (local to window)
setglobal cole=2 cocu=nc      " (local to window) conceal settings

" Section:  5 syntax, highlighting and spelling                           {{{1

" Terminal background detection doesn't work.
let &g:background = has('gui_running') ? &g:background : 'dark'

setglobal nohls               " (global)
"set wincolor                 " (local to window) Highlight group to use instead of hl-Normal
"set termguicolors            " (global) use 24bit color in terminal
"set cursorcolumn             " (local to window) Highlight cursor column
setglobal cursorline          " (local to window) Highlight the cursor's line. XXX: non-ideal interactions with concealing in console vim.

" Section:  6 multiple windows                                            {{{1

setglobal laststatus=2        " (global) when to show a statusline
setglobal ea ead=ver          " (global) equalalways,-direction = [ver / hor / both]
setglobal wh=12 wiw=20        " (global) Min [LINES / COLS] used for the CURRENT window
setglobal wmh=7 wmw=20        " (global) Min [LINES / COLS] used for ANY window
setglobal hh=20 pvh=12        " (global) help / pvw height
"set nowfh nowfw              " (local to window) [no] fixed window [ h / w ]
"set pvw                      " (local to window) set preview window
setglobal hidden              " (global) don't unload a buffer when no longer shown in a window
setglobal swb=useopen,split   " (global) which window to use when jumping to a buffer
setglobal sb spr              " (global)  split [below?] [right?]

" Section:  7 multiple tab pages                                          {{{1

" Section:  8 terminal                                                    {{{1

" Section:  9 using the mouse                                             {{{1

setglobal mouse=              " (global) Disable mouse

" Section: 10 GUI                                                         {{{1

setglobal guioptions+=f       " (global) Gvim no auto-background it

" Section: 11 printing                                                    {{{1

" Section: 12 messages and info                                           {{{1

setglobal shortmess=atToOcFA  " (global) list of flags to make messages shorter
setglobal noshowcmd           " (global)
setglobal noshowmode          " (global)
setglobal confirm             " (global) start a dialog when a command fails
setglobal errorbells          " (global) Bell for error messages
setglobal visualbell          " (global) visual bell instead of beep
setglobal t_vb=               " (global) terminal bell character (empty disables)

" Section: 13 selecting text                                              {{{1

" Section: 14 editing text                                                {{{1

setglobal undolevels=500      " (global-local to buffer) local is -123456 when global is used
setglobal undoreload=2000     " (global) save whole buffer on reload when at most this many lines
setglobal tw=80               " (local to buffer) line length above which to break a line
setglobal bs=indent,start     " (global) How backspace acts in insert mode default:indent,eol,start
setglobal formatoptions=cronlj
                              " (global-local to buffer)
" formatoptions                                                           {{{2
" |fo-table|:
"   c - auto-wrap comments using textwidth inserting comment leader
"   r - auto-insert comment leader after <Enter> in Insert mode
"   o - auto-insert comment leader after 'o' or 'O' in Normal mode
"   q - allow formatting of comments with "gq"
"   a - reformat paragraphs
"   n - recognize numbered lists
"   l - don't break already long lines in insert
"   j - join comment leaders where it makes sense
"}}}2

setglobal tildeop             " (global) Make tilde an operator. ~{motion} instead of g~{motion}
"setglobal opfunc=…           " (global) function called for the g@ operator
setglobal showmatch           " (global) flash matching pair on insert.
setglobal matchtime=2         " (global) flash duration (in 1/10s)
setglobal matchpairs+=<:>,^:$ " (local to buffer) characters that form pairs; (:),{:},[:]
setglobal nojoinspaces        " (global)

" Section: 15 tabs and indenting                                          {{{1

setglobal smarttab shiftround " (global)
setglobal sts=-1 sw=2 et ai   " (local to buffer)
" Defaults                                                                {{{2
" VIM: ts=8 vts= sts=0  vsts= sw=8 noet nosta
"      nosr noai nosi nobri briopt=
"      nocin cink=[] cino=[] cinw=[] nolisp lw=[]
"      nopi noci inde=
" Explanations                                                            {{{2
" tabstop                                                                 {{{
" How many spaces to print per one tab. Generally we can just leave this at 8
" no matter which ascii character you want to prefer, since most tend to view
" tabs as having the length equivalent to 8 spaces.
" }}}
" shiftwidth                                                              {{{
" This sets the amount of indentation that's applied by e.g. 'cindent' >> <<
" etc. If 0 then value of 'tabstop' is used (since vim 7.3.629). In
" tab-free contexts it's best to set shiftwidth so that the most common
" indent amounts are multiples of shiftwidth value.
" }}}
" expandtab                                                               {{{
" If this is set vim won't insert any tab characters your typed
" as tabs, or does it convert a string of spaces you type into a tab
" character. Unless the verbatim "paste" mode is active. }}}
" softtabstop                                                             {{{
" Soft tab stop is an optional setting, which makes it
" possible to have a an additional indentation schema in parallel to the
" other. Double or half of the other, typically. Something like sw=2 sts=5
" works as you'd expect, irrespective of if expandtab is in effect or not
" (though if it is, indentation wider than tabstop will be turned into tabs.
" Disable (set negative value) to have one effective indentation parameter.
" See also "vartabstop" and "varsofttabstop".
" }}}
" smarttab                                                                {{{
" Setting smarttab adds a small final twist. Set si to be able to
" use tabs for indenting before first non-blank (^) on a line by shiftwidth
" /and/ still be inserting pure tabs (or softtabstop) elsewhere!
" }}}
" shiftround                                                              {{{
" Whether indenting should calculate and round nearest multiple of shiftwidth
" from the beginning of line. If off always increases current by shiftwidth.
" <url:vimhelp:shiftround> default: off
" }}}
" autoindent                                                              {{{
" (buffer) To inherit indentation from adjacent lines i files of even arbitrary
" filetypes.
" }}}
" smartindent                                                             {{{
" }}}
" cindent                                                                 {{{
" Automatic indentation suited for C-like languages.  set
"   "cinkeys=..."
"   "cinoptions=..."
"   "cinwords=..."
" }}}
" lisp: Lispy auto-indent                                                 {{{
" "lispwords"
" }}}
" preserveindent                                                          {{{
" When changing indent of current line preserve as much of indent as possible
" }}}
" copyindent                                                              {{{
" Copy the indent structure over from current line to next
" }}}

" Section: 16 folding (local to window)                                   {{{1

setglobal foldlevelstart=0    " (global)
setglobal foldcolumn=1        " (local to window)
setglobal foldmethod=marker   " (local to window)
setglobal foldignore=         " (local to window)
setglobal foldopen=hor,mark,percent,quickfix,search,tag " (global)

" Defaults                                                                {{{2
"
" VIM: fcl= fdc=0 fen fde=0 fdi=# fdl=0 fdls=-1 fmr={{{,}}} fdm=manual fml=1 fdn=20
"      fdo=block,hor,mark,percent,quickfix,search,tag,undo fdt=foldtext()

" Section: 17 diff mode                                                   {{{1

" Section: 18 mapping                                                     {{{1

" Section: 19 reading and writing files                                   {{{1

setglobal modeline            " (local to buffer)
setglobal modelines=2         " (global)
setglobal writebackup         " (global)
setglobal backup              " (global)
setglobal backupskip=/tmp/*,/run/*,/dev/shm/*,*/.ssh/id*,*_EDITMSG
                              " (global)
setglobal backupcopy=auto     " (global-local to buffer)
setglobal autoread            " (global-local to buffer)

" Section: 20 the swap file                                               {{{1

" Section: 21 command line editing                                        {{{1

setglobal history=700         " (global) size of history of ":" commands
"setglobal wildchar=<tab>     " (global)
setglobal wildcharm=<tab>     " (global) like 'wildchar' but can be used in a mapping
setglobal wildmode=list:longest,list:full
                              " (global) command completion behavior
setglobal nowildmenu          " (global) command-line completion shows a list of matches

" Section: 22 executing external commands                                 {{{1

"set noshelltemp              " (global) Note: gets set in init.rc.vim

" Section: 23 running make and jumping to errors                          {{{1

setglobal grepprg=grep\ -nHPs " (global-local to buffer)

" Section: 24 language specific                                           {{{1

" Section: 25 multi-byte characters                                       {{{1

" Section: 26 various                                                     {{{1

setglobal virtualedit=block   " (global) when to use virtual editing: "block", "insert" and/or "all"
setglobal secure              " (global) Reasonable paranoia of autocmds/maps in rc files of current directory Session
setglobal sessionoptions-=folds
                              " (global) XXX: do -=folds if using FastFold+Obsession
setglobal pyxversion=3        " (global) calls python3

" Set local options for current buffer/window if vim_starting             {{{1
if has('vim_starting')
  " 1..5
  setlocal so< wrap< lbr< nu< rnu< nuw< cole< cocu< cul< cuc<
  " 14..16
  setlocal ul< tw< fo< mps< sts< sw< et< ai< fdi< fdm< fdc<
  " 19
  setlocal ar< bkc< ml<
  " 23
  setlocal gp<
endif
