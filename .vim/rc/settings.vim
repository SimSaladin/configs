" File: ~/.vim/plugin/settings.vim
scriptencoding utf-8

if get(g:,'no_vimrc_plugin_settings')
  finish
endif

" airline                                                                 {{{1

let g:airline_theme              = 'solarized'
let g:airline_powerline_fonts    = 1
let g:airline_highlighting_cache = 1
let g:airline_inactive_collapse  = 1 " default 1
let g:airline_inactive_alt_sep   = 0 " default 1
let g:airline_focuslost_inactive = 1
let g:airline_mode_map = {
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
"let g:airline#extensions#disable_rtp_load = 1
"let g:airline_extensions =
"      \ ['branch', 'tabline', 'whitespace', 'obsession']
let g:airline#extensions#nrrwrgn#enabled = 0
let g:airline#extensions#whitespace#checks =
      \ ['indent', 'trailing', 'mixed-indent-file', 'conflicts']
let g:airline#extensions#wordcount#filetypes =
      \ ['asciidoc', 'help', 'mail', 'markdown', 'pandoc', 'vimdoc', 'org', 'rst', 'tex', 'text']
let g:airline#extensions#tabline#tab_nr_type   = 2
let g:airline#extensions#tabline#show_tab_type = 1
let g:airline#extensions#tabline#show_buffers  = 1
let g:airline#extensions#obsession#indicator_text = '$$'

" Goyo                                                                    {{{1

let g:goyo_width  = 100
let g:goyo_height = '100%'

" yankring                                                                {{{1

let g:yankring_history_dir     = g:myvimrc#cachedir
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

" operator-surround                                                       {{{1

if !exists('g:operator#surround#blocks')
  let g:operator#surround#blocks = {}
endif
let s:tilde_bl    = {'block':["~~~\n","\n~~~"],'motionwise':['line', 'block'],'keys':['~']}
let s:backtick_bl = {'block':["```\n","\n```"],'motionwise':['line', 'block'],'keys':['`']}
let g:operator#surround#blocks.markdown = [s:tilde_bl, s:backtick_bl]
let g:operator#surround#blocks.pandoc   = [s:tilde_bl, s:backtick_bl]
let g:operator#surround#blocks.vimwiki  = [s:tilde_bl, s:backtick_bl]
let g:operator#surround#blocks.snippets = []

" fold / fastfold                                                         {{{1

let g:fastfold_force                  = 0
let g:fastfold_minlines               = 2000
let g:fastfold_skip_filetypes         = [ 'taglist' ]
let g:fastfold_fold_command_suffixes  = []
let g:fastfold_fold_movement_commands = []

" fold / origami                                                          {{{1

" NOTE: All settings have buffer-local variants too.
let g:OrigamiPadding          = 0
let g:OrigamiFoldAtCol        = 75
let g:OrigamiSeparateLevels   = 0
let g:OrigamiIncAllLines      = 0
let g:OrigamiStaggeredSpacing = 0

" utl                                                                     {{{1

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

" nerdtree                                                                {{{1

let g:NERDTreeIgnore = ['\~$', '.aes$', '^dist$', '.dvi$', '.aux$', '.toc$', '.1$']

" ultisnips                                                               {{{1

let g:snips_author    = get(g:,'author_name','(empty)')
let g:default_license = get(g:,'default_license','WTFPL')

let g:UltiSnipsEditSplit          = 'context'
let g:UltiSnipsSnippetsDir        = $HOME.'/.vim/UltiSnips'
let g:UltiSnipsSnippetDirectories = [g:UltiSnipsSnippetsDir]
let g:UltiSnipsEnableSnipMate     = 0
" NOTE: UltiSnips (un)maps trigger keys when they have no valid action.
let g:UltiSnipsExpandTrigger      = '<TAB>'
let g:UltiSnipsJumpForwardTrigger = '<TAB>'  " Note: can be same as ExpandTrigger.
let g:UltiSnipsJumpBackwardTrigger= '<S-TAB>'
let g:UltiSnipsListSnippets       = '<c-x>?' " <c-tab> not liked by terminal, don't map to that.

" polyglot                                                                {{{1

let g:polyglot_disabled               = ['markdown', 'csv', 'org', 'tmux']
let g:haskell_enable_quantification   = 1 " to enable highlighting of `forall`
let g:haskell_enable_recursivedo      = 1 " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax      = 1 " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles        = 1 " to enable highlighting of type roles
let g:haskell_enable_static_pointers  = 1 " to enable highlighting of `static`
let g:haskell_backpack                = 1 " to enable highlighting of backpack keywords

" syntastic                                                               {{{1

let g:syntastic_aggregate_errors         = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_jump                = 1
let g:syntastic_auto_loc_list            = 1
let g:syntastic_check_on_wq              = 0
let g:syntastic_error_symbol             = '║E' " 😭 https://github.com/ryanoasis/powerline-extra-symbols
let g:syntastic_warning_symbol           = '║W' " 😯
let g:syntastic_style_error_symbol       = '│E' " 😱
let g:syntastic_style_warning_symbol     = '│W' " 😴
let g:syntastic_extra_filetypes          = []
"let g:syntastic_id_checkers             = 1
let g:syntastic_python_checkers          = ['python', 'pylint']
let g:syntastic_vim_checkers             = ['vint'] " XXX: needs pip install vim-vint (pacman -S vint)
let g:syntastic_haskell_checkers         = ['hlint']
let g:syntastic_yaml_checkers            = ['yamllint']
let g:syntastic_help_checkers            = ['vimhelplint']
let g:syntastic_yaml_yamllint_args       = '-c ~/.yamllint.yaml' " XXX: should be more intelligent

" qfsigns                                                                 {{{1

let g:quickfixsigns_classes  = ['qfl', 'loc', 'marks', 'vcsdiff', 'vcsmerge']
let g:quickfixsigns_echo_map = '<leader>qq'
let g:quickfixsigns_echo_balloon = 1

" XXX: some after/syntax instead?
sign define QFS_QFL        text=│► texthl=WarningMsg
sign define QFS_LOC        text=│◊ texthl=Identifier
sign define QFS_QFL_E      text=║E texthl=WarningMsg
sign define QFS_LOC_E      text=│E texthl=WarningMsg
sign define QFS_QFL_W      text=║W texthl=WarningMsg
sign define QFS_LOC_W      text=│W texthl=WarningMsg
sign define QFS_LONGLINES  text=│$ texthl=Identifier
sign define QFS_VCS_ADD    text=++ texthl=MySignDiffAdd
sign define QFS_VCS_CHANGE text=== texthl=MySignDiffChange
sign define QFS_VCS_DEL    text=-  texthl=MySignDiffDelete
sign define QFS_VCS_DELM   text=-- texthl=MySignDiffDelete
sign define QFS_VCS_DEL1   text=-1 texthl=MySignDiffDelete
sign define QFS_VCS_DEL2   text=-2 texthl=MySignDiffDelete
sign define QFS_VCS_DEL3   text=-3 texthl=MySignDiffDelete
sign define QFS_VCS_DEL4   text=-4 texthl=MySignDiffDelete
sign define QFS_VCS_DEL5   text=-5 texthl=MySignDiffDelete
sign define QFS_VCS_DEL7   text=-7 texthl=MySignDiffDelete
sign define QFS_VCS_DEL8   text=-8 texthl=MySignDiffDelete
sign define QFS_VCS_DEL9   text=-9 texthl=MySignDiffDelete

" better-whitespace                                                       {{{1

let g:strip_whitespace_on_save      = 1 " asks first
let g:strip_only_modified_lines     = 1
let g:strip_max_file_size           = 20000 " default 1k
let g:show_spaces_that_precede_tabs = 1
let g:strip_whitelines_at_eof       = 1
let g:better_whitespace_ctermcolor  = '9'
let g:better_whitespace_operator    = '' " default <leader>s

" autoformatter                                                           {{{1

let g:autoformat_autoindent             = 0
let g:autoformat_retab                  = 0
let g:autoformat_remove_trailing_spaces = 0

" tagbar                                                                  {{{1

let g:tagbar_left             = 1  " (0)
let g:tagbar_width            = 50 " (40)
let g:tagbar_show_linenumbers = 1  " (0)
let g:tagbar_type_vimwiki     = {
      \   'ctagstype':'vimwiki'
      \ , 'kinds':['h:header']
      \ , 'sro':'&&&'
      \ , 'kind2scope':{'h':'header'}
      \ , 'sort':0
      \ , 'ctagsbin': get(g:,'myvimrc#cachedir','.').'/dein/.cache/vimrc/.dein/extras/vwtags.py'
      \ , 'ctagsargs': 'markdown'
      \ }

" vimwiki/taskwiki                                                        {{{1

let g:taskwiki_markup_syntax  = 'markdown'
let g:taskwiki_maplocalleader = g:maplocalleader
let g:taskwiki_sort_orders    = {'E':'end-','M':'modified-'}

" Note: this prefix is also used for couple <buffer> mappings(why?) <prefix>r @<Plug>VimwikiRenameLink, and <prefix>d @<Plug>VimwikiDeleteLink
let g:vimwiki_map_prefix                = '<leader>_ww'
let g:vimwiki_table_mappings            = 0
let g:vimwiki_hl_headers                = 1 " Use different colors for headers
let g:vimwiki_hl_cb_checked             = 1 " highlight only first line of [X]
let g:vimwiki_html_header_numbering     = 1
let g:vimwiki_html_header_numbering_sym = '.'
let g:vimwiki_folding                   = 'expr'
let g:vimwiki_listsym_rejected          = '✗'
let g:vimwiki_list                      = [{
      \ 'path'             : '~/notes/',
      \ 'path_html'        : '~/notes_html/',
      \ 'ext'              : '.wmd',
      \ 'syntax'           : 'markdown',
      \ 'list_margin'      : 0,
      \ 'auto_toc'         : 1,
      \ 'auto_tags'        : 1,
      \ 'auto_diary_index' : 1,
      \ 'maxhi'            : 1
      \ }]

" pandoc                                                                  {{{1

let g:pandoc#syntax#conceal#use                = 1
let g:pandoc#syntax#conceal#blacklist          = []
let g:pandoc#syntax#conceal#cchar_overrides    = {}
let g:pandoc#syntax#conceal#urls               = 1
let g:pandoc#syntax#codeblocks#ignore          = ['delimited'] " ['definition', 'delimited']
let g:pandoc#syntax#codeblocks#embeds#use      = 1
let g:pandoc#syntax#codeblocks#embeds#langs    = [ 'yaml', 'ruby', 'haskell', 'sshconfig', 'help', 'systemd',  'bash = sh' ]
let g:pandoc#syntax#style#emphases             = 1
let g:pandoc#syntax#style#underline_special    = 1
let g:pandoc#syntax#style#use_definition_lists = 1

let g:pandoc#filetypes#handled         = ['pandoc', 'markdown']
let g:pandoc#filetypes#pandoc_markdown = 1

"let g:pandoc#modules#enabled                = ['yaml', 'bibliographies', 'completion', 'command', 'folding', 'formatting', 'indent', 'menu', 'metadata', 'keyboard', 'toc', 'spell']

let g:pandoc#formatting#mode                = 'ha' " Formatting: h/a/A/s

let g:pandoc#spell#default_langs            = ['en']

let g:pandoc#folding#mode                   = 'stacked'
"let g:pandoc#folding#fastfolds              = 1
"let g:pandoc#folding#fold_div_classes       = []
let g:pandoc#folding#fold_fenced_codeblocks = 1 " pandoc-folding-module
let g:pandoc#folding#fold_yaml              = 1

" Note: will always integrate with Goyo. Default enabled modules is empty.
let g:pandoc#after#modules#enabled = [ 'deoplete', 'fastfold', 'nrrwrgn', 'ultisnips' ]

" editorconfig                                                            {{{1

let g:EditorConfig_exclude_patterns       = ['fugitive://.\*']
let g:EditorConfig_disable_rules          = ['trim_trailing_whitespace']
let g:EditorConfig_max_line_indicator     = 'line'  " 'none'
let g:EditorConfig_preserve_formatoptions = 1
" let g:EditorConfig_verbose              = 1

" context_filetypes                                                       {{{1

let g:context_filetype#filetypes = get(g:,'context_filetype#filetypes',{})

" let g:context_filetype#filetypes.toml =
" [{'filetype':'vim', 'start' : "=\\s*'\\{3}\\zs", 'end' : "'\\{3}"}]

" LanguageClient                                                          {{{1

let g:LanguageClient_autoStart      = 0
let g:LanguageClient_loggingFile    = g:myvimrc#runtimedir.'/'.getpid().'-LanguageClient.log'
let g:LanguageClient_serverCommands = { 'haskell': ['hie-wrapper'] }
let g:LanguageClient_rootMarkers    = ['stack.yaml']

" hi link ALEError Error
" hi      Warning term=underline cterm=underline ctermfg=Yellow gui=undercurl guisp=Gold
" hi link ALEWarning Warning
" hi link ALEInfo SpellCap

" vim-ghcid-quickfix                                                      {{{1

let g:ghcid_quickfix_showing = 'quickfix_on_start'  " 'quickfix_on_error'

" vim-orgmode                                                             {{{1

let g:org_indent                   = 0
let g:org_agenda_files             = []
let g:org_aggressive_conceal       = 1
let g:org_todo_keywords            = ['TODO', 'WAITING', '|', 'DONE']
let g:org_todo_keyword_faces       = [['TODO', 'magenta'], ['WAITING',[':foreground magenta',':slant italic']] ]
let g:org_heading_highlight_colors = [ 'Special', 'PreProc', 'Statement', 'Type', 'Identifier', 'Constant', 'Title' ]

