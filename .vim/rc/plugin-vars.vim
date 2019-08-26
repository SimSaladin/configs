" File: ~/.vim/rc/plugin-vars.vim
" Author: Samuli Thomasson

scriptencoding utf-8

if exists('g:loaded_plugin_vars')
  finish
endif
let g:loaded_plugin_vars = 1

" FastFold                                                                {{{1
let g:fastfold_force = 0
    " To intercepts also [indent, diff, marker] folds. Default is to do [expr,
    " syntax]. Kinda forces fdm = manual if on.
let g:fastfold_minlines = 1000

nmap <SID>(DisableFastFoldUpdate) <Plug>(FastFoldUpdate)
    " Note: Disables FastFold FastFold default mapping to "zuz"

" Origami (folding)                                                       {{{1
" |origami.txt|

" NOTE: All of these have buffer-local variants too!
let g:OrigamiPadding          = 0 " Specify extra padding to be added.
let g:OrigamiSeparateLevels   = 0 " Align different fold-levels independently.
let g:OrigamiFoldAtCol        = 75
    " Force the markers to a column. Relative values "+-N" relative to textwidth.
    " NOTE: mutually exclusive with SeparateLevels!
let g:OrigamiIncAllLines      = 0
    " If consider all lines or just those with a
    " foldmarker when aligning at longest line.
let g:OrigamiStaggeredSpacing = 0
    " If different foldlevels should be staggered.
    " If expandtab, this specifies the number of spaces to stagger.
    " If noexpandtab, specifies the number of tabs.
" let g:OrigamiMap = {
"       \ 'Leader'           : "Z",
"       \ 'Align'            : "a",
"       \ 'AlignAll'         : "A",
"       \ 'CommentedOpen'    : "F",
"       \ 'UncommentedOpen'  : "f",
"       \ 'CommentedClose'   : "C",
"       \ 'UncommentedClose' : "c",
"       \ 'Delete'           : "D",
"       \ }

function! s:fold_replace(count) "{{{
  return ":call origami#DeleteFoldmarker() | "
        \ . 'call origami#InsertFoldmarker("open", "nocomment", '.a:count.')'
endfunction
noremap <unique><script><expr> <Plug>(fold-replace) <SID>fold_replace(v:count1)
"}}}

function! s:fold_renumber(d) range
  if a:d != 0
    while search('\%({\{3}\|}\{3}\)\zs\d', 'cWz', a:lastline)
      execute 'normal' a:d > 0 ? a:d.'' : (-a:d).''
      s/[{}]\{3}\%#0//e
    endwhile
  endif
endfunction

nmap <unique> <Plug>(fold-decrease) V<Plug>(textobj-fold-i):call <SID>fold_renumber(-1)<CR>
nmap <unique> <Plug>(fold-increase) V<Plug>(textobj-fold-i):call <SID>fold_renumber(1)<CR>

" Replace fold (if any) with open fold (of count or 1)
nmap <unique> Zr <Plug>(fold-replace)
" Increase or decrease foldlvl in the whole current fold
nmap <unique> Z> <Plug>(fold-increase)
nmap <unique> Z< <Plug>(fold-decrease)

" fold-cycle                                                              {{{1
let g:fold_cycle_default_mapping = 0

nmap <Tab>    <Plug>(fold-cycle-open)
nmap <S-Tab>  <Plug>(fold-cycle-close)

" Context FileTypes                                                       {{{1
let g:context_filetype#filetypes = {}
"let g:context_filetype#filetypes.perl6 = [{'filetype' : 'pir', 'start' : 'Q:PIR\s*{', 'end' : '}'}]

" Quickfixsigns                                                           {{{1
let g:quickfixsigns_classes    = ['qfl', 'loc', 'marks', 'vcsdiff', 'vcsmerge']
    " ['qfl', 'loc', 'marks', 'vcsdiff', 'vcsmerge', 'longlines']
let g:quickfixsigns_echo_map   = '<leader>qq'
    " echoes text for signs at current line

sign define QFS_LONGLINES text=😶 texthl=Identifier

" Polyglot                                                                {{{1
let g:polyglot_disabled               = ['markdown', 'csv', 'org', 'elm']

" Haskell syntax                                                          {{{1
let g:haskell_enable_quantification   = 1 " to enable highlighting of `forall`
let g:haskell_enable_recursivedo      = 1 " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax      = 1 " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles        = 1 " to enable highlighting of type roles
let g:haskell_enable_static_pointers  = 1 " to enable highlighting of `static`
let g:haskell_backpack                = 1 " to enable highlighting of backpack keywords

" Colorizer                                                               {{{1
let g:colorizer_startup = 0

nmap <leader>tc <Plug>Colorizer

" better-whitespace                                                       {{{1
let g:strip_whitespace_on_save           = 1 " asks first
let g:strip_only_modified_lines          = 1
let g:strip_max_file_size                = 20000 " default 1k
let g:show_spaces_that_precede_tabs      = 1
let g:strip_whitelines_at_eof            = 1
let g:better_whitespace_ctermcolor       = '9'
let g:better_whitespace_operator         = '' " default <leader>s

" Syntastic                                                               {{{1
let g:syntastic_aggregate_errors         = 1
let g:syntastic_vim_checkers             = ['vint'] " XXX: needs pip install vim-vint (pacman -S vint)
let g:syntastic_haskell_checkers         = ['hlint']
let g:syntastic_yaml_checkers            = ['yamllint']
let g:syntastic_help_checkers            = ['vimhelplint']
let g:syntastic_yaml_yamllint_args       = "-c ~/.yamllint.yaml" " XXX: should be more intelligent
let g:syntastic_extra_filetypes          = []
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_wq              = 0
let g:syntastic_aggregate_errors         = 1
"let g:syntastic_id_checkers             = 1
let g:syntastic_error_symbol             = '😭' " https://github.com/ryanoasis/powerline-extra-symbols
let g:syntastic_style_error_symbol       = '😱'
let g:syntastic_warning_symbol           = '😯'
let g:syntastic_style_warning_symbol     = '😴'

" CamelCaseMotions (nox)                                                  {{{1

for s:w in ['w', 'b', 'e']
  execute 'map' s:w '<Plug>CamelCaseMotion_'.s:w
  execute 'xmap i'.s:w '<Plug>CamelCaseMotion_i'.s:w
  execute 'omap i'.s:w '<Plug>CamelCaseMotion_i'.s:w
endfor

" CAW (nox)                                                               {{{1

let g:caw_operator_keymappings = 1 " omaps not enabled by default

nmap <Leader>c <Plug>(caw:prefix)
xmap <Leader>c <Plug>(caw:prefix)
    " Default prefix is 'gc' |caw-keymappings-prefix|

" operator-replace (o)                                                    {{{1

map <unique> <space>r <Plug>(operator-replace)

" operator-surround (nox)                                                 {{{1

" TODO if you use vim-textobj-multiblock
" TODO if you use vim-textobj-anyblock
" TODO if you use vim-textobj-between
" nmap <silent>sdb <Plug>(operator-surround-delete)<Plug>(textobj-between-a)
" nmap <silent>srb <Plug>(operator-surround-replace)<Plug>(textobj-between-a)

let s:tilde_bl    = {'block':["~~~\n","\n~~~"],'motionwise':['line', 'block'],'keys':['~']}
let s:backtick_bl = {'block':["```\n","\n```"],'motionwise':['line', 'block'],'keys':['`']}

let s:snip_blocks = [
      \ {'block':['`!v','`'],'motionwise':['char','line','block'],'keys':['v']}
      \ ]

let g:operator#surround#blocks          = {}
let g:operator#surround#blocks.markdown = [s:tilde_bl, s:backtick_bl]
let g:operator#surround#blocks.pandoc   = [s:tilde_bl, s:backtick_bl]
let g:operator#surround#blocks.vimwiki  = [s:tilde_bl, s:backtick_bl]
let g:operator#surround#blocks.snippets = []

" Note: cl is the same as default mapping of s
" Note: cc is the same as default mapping of S

map <unique><silent> sa <Plug>(operator-surround-append)
map <unique><silent> sd <Plug>(operator-surround-delete)
map <unique><silent> sr <Plug>(operator-surround-replace)
map <unique><silent> S  <Plug>(operator-surround-replace)

" WindowSwap (commands, mappings)                                         {{{1

function! s:windowswapSwapWithCmd(cmd, altcmd)
  " Mark current window. Execute first argument. Swap if current window is
  " marked (new). Otherwise execute second arg.
  call WindowSwap#MarkWindowSwap()
  execute a:cmd
  if WindowSwap#IsCurrentWindowMarked()
    execute a:altcmd
  else
    WindowSwap#DoWindowSwap()
  endif
endfunction

let g:windowswap_map_keys = 0 " map default keys in plugin?

nnoremap <silent> <leader>wy :call WindowSwap#MarkWindowSwap()<cr>
nnoremap <silent> <leader>wp :call WindowSwap#DoWindowSwap()<cr>
nnoremap <silent> <leader>ws :call WindowSwap#EasyWindowSwap()<cr>

" Overwrite vim defaults for CTRL-W H …J …K …L
nnoremap <silent> <c-w>H :<c-u>call <SID>windowswapSwapWithCmd(v:count1.'wincmd h', 'wincmd H')<cr>
nnoremap <silent> <c-w>J :<c-u>call <SID>windowswapSwapWithCmd(v:count1.'wincmd j', 'wincmd J')<cr>
nnoremap <silent> <c-w>K :<c-u>call <SID>windowswapSwapWithCmd(v:count1.'wincmd k', 'wincmd K')<cr>
nnoremap <silent> <c-w>L :<c-u>call <SID>windowswapSwapWithCmd(v:count1.'wincmd l', 'wincmd L')<cr>

" YankRing                                                                {{{1
let g:yankring_history_dir     = g:datadir
let g:yankring_n_keys          = 'Y D x X'
let g:yankring_o_keys          = 'b B w W e E d y $ G;  iW aw aW'
let g:yankring_zap_keys        = 'f F t T / ?'
let g:yankring_ignore_operator = 'g~ gu gU != gq g? > < zf g@'
let g:yankring_v_key           = 'v'
let g:yankring_del_v_key       = 'd x'
let g:yankring_paste_n_bkey    = 'P'
let g:yankring_paste_n_akey    = 'p'
let g:yankring_paste_v_key     = 'P'
let g:yankring_replace_n_pkey  = '<C-P>'
let g:yankring_replace_n_nkey  = '<C-N>'

nnoremap <silent> <leader>ty :<C-U>YRToggle<CR>

function! YRRunAfterMaps()
  return
  " YankRing calls this after doing its mappings (noremap).
endfunction

" Utl                                                                     {{{1
let g:utl_opt_verbose                    = 0
let g:utl_opt_highlight_urls             = 'yes'
let g:utl_cfg_hdl_mt_generic             = "silent !xdg-open '%p'"
let g:utl_cfg_hdl_mt_text_html           = 'VIM'
let g:utl_cfg_hdl_mt_text_directory__vim = 'VIM'
let g:utl_cfg_hdl_mt_text_directory      = g:utl_cfg_hdl_mt_text_directory__vim
let g:utl_cfg_hdl_scm_http__wget         = "call Utl_if_hdl_scm_http__wget('%u')"
let g:utl_cfg_hdl_scm_http_system        = "silent !xdg-open '%u#%f'"
let g:utl_cfg_hdl_scm_http               = g:utl_cfg_hdl_scm_http_system
let g:utl_cfg_hdl_scm_mailto             = "!neomutt '%u'"

nnoremap <leader>o :Utl<CR>

" Speeddating                                                             {{{1
let g:speeddating_no_mappings = 1

nmap <leader>dn   <Plug>SpeedDatingUp
nmap <leader>dp   <Plug>SpeedDatingDown
nmap <leader>ds   <Plug>SpeedDatingNowLocal
nmap <leader>dS   <Plug>SpeedDatingNowUTC

" Tagbar                                                                  {{{1
nnoremap <leader>tb :TagbarToggle<CR>

" UltiSnips                                                               {{{1
let g:snips_author                = exists('g:author_name') ? g:author_name : '(empty)'
let g:UltiSnipsEditSplit          = 'context'
let g:UltiSnipsSnippetsDir        = $HOME.'/.vim/UltiSnips'
let g:UltiSnipsSnippetDirectories = [g:UltiSnipsSnippetsDir]
let g:UltiSnipsEnableSnipMate     = 0
" NOTE: UltiSnips (un)maps trigger keys when they have no valid action.
let g:UltiSnipsExpandTrigger      = '<TAB>'
let g:UltiSnipsJumpForwardTrigger = '<TAB>'  " Note: can be same as ExpandTrigger.
let g:UltiSnipsJumpBackwardTrigger= '<S-TAB>'
let g:UltiSnipsListSnippets       = '<c-x>?' " <c-tab> not liked by terminal, don't map to that.

" Airline                                                                 {{{1
let g:airline_theme              = 'solarized'
let g:airline_powerline_fonts    = 1
let g:airline_extensions         = ['branch', 'tabline', 'whitespace', 'obsession']
let g:airline_highlighting_cache = 1
let g:airline_inactive_collapse  = 1 " default 1
let g:airline_inactive_alt_sep   = 0 " default 1
let g:airline_focuslost_inactive = 1
let g:airline_mode_map           = {
    \ '__'     : '?',
    \ 'c'      : '*',
    \ 'i'      : 'I',
    \ 'ic'     : 'I*',
    \ 'ix'     : 'X',
    \ 'multi'  : 'M',
    \ 'n'      : 'N',
    \ 'ni'     : 'iN',
    \ 'no'     : 'O',
    \ 'R'      : 'R',
    \ 'Rv'     : 'vR',
    \ 's'      : 'S',
    \ 'S'      : 'Ss',
    \ ''     : 'Sc',
    \ 't'      : 'T',
    \ 'v'      : 'V',
    \ 'V'      : 'L',
    \ ''     : 'B',
    \ }
    " <url:vimhelp:vim-modes>
let g:airline#extensions#whitespace#checks        =
      \ [ 'indent', 'trailing', 'mixed-indent-file', 'conflicts' ]
let g:airline#extensions#wordcount#filetypes      =
      \ ['asciidoc', 'help', 'mail', 'markdown', 'pandoc', 'vimdoc',
      \ 'org', 'rst', 'tex', 'text']
let g:airline#extensions#tabline#tab_nr_type      = 2
let g:airline#extensions#tabline#show_tab_type    = 1
let g:airline#extensions#tabline#show_buffers     = 1     " tabline
let g:airline#extensions#obsession#indicator_text = '$$'  " obsession

" LanguageClient                                                          {{{1
let g:LanguageClient_autoStart      = 0
let g:LanguageClient_serverCommands = { 'haskell': ['hie-wrapper'] }
let g:LanguageClient_rootMarkers    = ['stack.yaml']
let g:LanguageClient_loggingFile    = expand('~/.local/share/LanguageClient.log')

" Goyo                                                                    {{{1
let g:goyo_width                    = 90
let g:goyo_height                   = 100

" Denite                                                                  {{{1
let g:fruzzy#sortonempty            = 0

nmap ;; :Denite<space><tab>

nnoremap <silent> <Leader>gf :<c-u>Denite file<CR>
nnoremap <silent> <Leader>gr :<c-u>Denite file_mru<CR>
nnoremap <silent> <Leader>gR :<c-u>Denite file/old<CR>
nnoremap <silent> <Leader>gb :<c-u>Denite buffer<CR>
nnoremap <silent> <Leader>gg :<c-u>Denite -buffer-name=search -no-empty grep<CR>
nnoremap <silent> <Leader>gn :<c-u>Denite dein<CR>
nnoremap <silent> <leader>gt :<C-u>Denite junkfile:new junkfile<CR>

nnoremap <silent> <c-h> :<c-u>DeniteCursorWord help<cr>

nnoremap <expr> <leader>/ line('$') > 10000 ? '/' : ":\<C-u>Denite -buffer-name=search -start-filter line\<CR>"
nnoremap <expr> <leader>n line('$') > 10000 ? 'n' : ":\<C-u>Denite -buffer-name=search -resume -refresh -no-start-filter\<CR>"
nnoremap <expr> <leader>* line('$') > 10000 ? '*' : ":\<C-u>DeniteCursorWord -buffer-name=search line\<CR>"

" NERDTree                                                                {{{1
let g:NERDTreeIgnore =
      \ ['\~$', '.aes$', '^dist$', '.dvi$', '.aux$', '.toc$', '.1$']

" Defx                                                                    {{{1
nnoremap <silent> <Space>f
  \ :<C-u>Defx -listed -resume -buffer-name=tab`tabpagenr()`<CR>

" CSV                                                                     {{{1
let g:csv_autocmd_arrange = 1

" VimWiki                                                                 {{{1
let notes =
      \ {
      \ 'path':'~/notes/','syntax':'markdown','list_margin':0,
      \ 'auto_toc':1,'auto_tags':1,'auto_diary_index':1,
      \ 'ext':'.wmd'
      \ }
let g:vimwiki_list                      = [notes]
let g:vimwiki_hl_headers                = 1 " Use different colors for headers
let g:vimwiki_hl_cb_checked             = 1 " highlight only first line of [X]
let g:vimwiki_html_header_numbering     = 1
let g:vimwiki_html_header_numbering_sym = '.'
let g:vimwiki_folding                   = 'expr'
let g:vimwiki_listsym_rejected          = '✗'
let g:vimwiki_auto_tags                 = 1
let g:vimwiki_auto_diary_index          = 1
let g:vimwiki_map_prefix                = '<unique> <leader>'
    " <url:vimhelp:vimwiki-global-mappings>
    " Also used for couple <buffer> mappings (why?)
    " <prefix>r @<Plug>VimwikiRenameLink
    " <prefix>d @<Plug>VimwikiDeleteLink
let g:vimwiki_table_mappings            = 0 " Map <CR> and <Tab> in insert mode?
    " <url:vimhelp:vimwiki-table-mappings>

nmap <unique><silent> <leader>wi  <Plug>VimwikiIndex
nmap <unique><silent> <leader>wti <Plug>VimwikiTabIndex
nmap <unique><silent> <leader>wg  <Plug>VimwikiUISelect
nmap <unique><silent> <leader>wdi <Plug>VimwikiDiaryIndex
nmap <unique><silent> <leader>wnd <Plug>VimwikiMakeDiaryNote
nmap <unique><silent> <leader>wnt <Plug>VimwikiTabMakeDiaryNote
nmap <unique><silent> <leader>wno <Plug>VimwikiMakeYesterdayDiaryNote
nmap <unique><silent> <leader>wnf <Plug>VimwikiMakeTomorrowDiaryNote
nmap <unique><silent> <leader>wh  <Plug>Vimwiki2HTML
nmap <unique><silent> <leader>whh <Plug>Vimwiki2HTMLBrowse
"nmap <unique>         <NOP>       <Plug>VimwikiDiaryGenerateLinks



" Not mapped: <Plug>VimwikiDiaryGenerateLinks

" TaskWiki                                                                {{{1
let g:taskwiki_maplocalleader = g:maplocalleader

" Pandoc                                                                  {{{1
let g:pandoc#filetypes#handled              = ["pandoc", "markdown"]
let g:pandoc#modules#enabled                =
      \ [
      \ 'yaml', 'bibliographies', 'completion', 'command', 'folding',
      \ 'formatting', 'indent', 'menu', 'metadata', 'keyboard', 'toc', 'spell'
      \ ]
let g:pandoc#formatting#mode                = 'ha' " Formatting: h/a/A/s
let g:pandoc#spell#default_langs            = ["en"]
let g:pandoc#folding#fold_yaml              = 1
let g:pandoc#folding#fold_div_classes       = []
let g:pandoc#folding#fastfolds              = 1
let g:pandoc#folding#fold_fenced_codeblocks = 1 " pandoc-folding-module
let g:pandoc#syntax#conceal#urls            = 1
let g:pandoc#syntax#codeblocks#embeds#langs =
      \ [
      \ "sh", "bash=sh", "yaml", "ruby", "haskell", "sshconfig", "help", "systemd"
      \ ]
    " pandoc-syntax
let g:pandoc#after#modules#enabled          =
      \ ["deoplete", "fastfold", "nrrwrgn", "ultisnips"]
    " pandoc-after
    " Note: "goyo" is always integrated. Others: fastfold nrrwrgn deoplete
    " vimcompletesme ultisnips neosnippets snipmate supertab tablemode unite

" Vim-orgmode                                                             {{{1
let g:org_indent                   = 0 " Default: let it be.
                                       " Codeblocks are less painful to embed
let g:org_aggressive_conceal       = 1
let g:org_agenda_files             = []
let g:org_heading_highlight_colors =
      \ ['Special', 'PreProc', 'Statement', 'Type', 'Identifier', 'Constant', 'Title']
let g:org_todo_keywords            =
      \ ['TODO', 'WAITING', '|', 'DONE'] " see <lnk:vimhelp:attr-list>
let g:org_todo_keyword_faces       =
      \ [
      \ ['TODO',"magenta"], ['WAITING',[':foreground magenta',':slant italic']]
      \ ]

" Emmet                                                                   {{{1
let g:user_emmet_mode = 'iv'
