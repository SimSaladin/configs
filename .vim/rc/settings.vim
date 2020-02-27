" File: ~/.vim/plugin/settings.vim
scriptencoding utf-8

if get(g:,'no_vimrc_plugin_settings')
  finish
endif

" See <url:vimhelp:ft-man-plugin> {{{1
let g:no_man_maps             = 1
let g:ft_man_folding_enable   = 0
let g:ft_man_no_sect_fallback = 1

" operator-surround                                                  {{{1
let g:operator#surround#blocks = get(g:,'operator#surround#blocks',{})

let s:tilde_bl    = {'block':["~~~\n","\n~~~"],'motionwise':['line', 'block'],'keys':['~']}
let s:backtick_bl = {'block':["```\n","\n```"],'motionwise':['line', 'block'],'keys':['`']}

call extend(g:operator#surround#blocks, {
      \ 'markdown':[s:tilde_bl, s:backtick_bl],
      \ 'pandoc'  :[s:tilde_bl, s:backtick_bl],
      \ 'vimwiki' :[s:tilde_bl, s:backtick_bl],
      \ 'snippets':[]
      \ })

" fastfold                                                    {{{1
let g:fastfold_force                  = 0
let g:fastfold_minlines               = 2000
let g:fastfold_skip_filetypes         = ['taglist']
let g:fastfold_fold_command_suffixes  = []
let g:fastfold_fold_movement_commands = []

" origami (fold)                                                     {{{1
let g:OrigamiPadding          = 0
let g:OrigamiFoldAtCol        = 70
let g:OrigamiSeparateLevels   = 0
let g:OrigamiIncAllLines      = 0
let g:OrigamiStaggeredSpacing = 0
    " NOTE: All settings have buffer-local variants too.

" UltiSnips                                                          {{{1
let g:UltiSnipsEditSplit                = 'context'
let g:UltiSnipsSnippetDirectories       = [$HOME.'/.vim/UltiSnips']
let g:UltiSnipsEnableSnipMate           = 1
let g:UltiSnipsRemoveSelectModeMappings = 1
let g:UltiSnipsExpandTrigger            = '<TAB>'  " NOTE: UltiSnips (un)maps trigger keys when they have no valid action.
let g:UltiSnipsListSnippets             = '<c-x>?' " <c-tab> not liked by terminal, don't map to that.
let g:UltiSnipsJumpForwardTrigger       = '<TAB>'  " Note: can be same as ExpandTrigger.
let g:UltiSnipsJumpBackwardTrigger      = '<S-TAB>'

" Snippets fields {{{2
let g:snips_author    = get(g:,'author_name','(empty)')
let g:default_license = get(g:,'default_license','WTFPL')

" polyglot                                                           {{{1
let g:polyglot_disabled = ['markdown', 'csv', 'org', 'tmux']

" polyglot:haskell
let g:haskell_enable_quantification   = 1 " to enable highlighting of `forall`
let g:haskell_enable_recursivedo      = 1 " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax      = 1 " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles        = 1 " to enable highlighting of type roles
let g:haskell_enable_static_pointers  = 1 " to enable highlighting of `static`
let g:haskell_backpack                = 1 " to enable highlighting of backpack keywords

" better-whitespace                                                  {{{1
let g:better_whitespace_ctermcolor  = '9'
let g:better_whitespace_operator    = '' " default <leader>s
let g:show_spaces_that_precede_tabs = 1
let g:strip_max_file_size           = 20000 " default 1k
let g:strip_only_modified_lines     = 1
let g:strip_whitelines_at_eof       = 1
let g:strip_whitespace_on_save      = 1 " asks first

" autoformatter                                                      {{{1
let g:autoformat_autoindent             = 0
let g:autoformat_retab                  = 0
let g:autoformat_remove_trailing_spaces = 0

" vimwiki/taskwiki                                                   {{{1

let g:taskwiki_markup_syntax  = 'markdown'
let g:taskwiki_sort_orders    = {'E':'end-','M':'modified-'}
let g:taskwiki_maplocalleader = g:maplocalleader

" pandoc                                                             {{{1
" pandoc#filetypes
let g:pandoc#filetypes#handled                 = ['pandoc', 'markdown']
let g:pandoc#filetypes#pandoc_markdown         = 1
"  pandoc#modules
"let g:pandoc#modules#enabled = ['yaml', 'bibliographies', 'completion', 'command', 'folding', 'formatting', 'indent', 'menu', 'metadata', 'keyboard', 'toc', 'spell']
" pandoc#syntax
let g:pandoc#syntax#conceal#use                = 1
let g:pandoc#syntax#conceal#blacklist          = []
let g:pandoc#syntax#conceal#cchar_overrides    = {}
let g:pandoc#syntax#conceal#urls               = 1
let g:pandoc#syntax#codeblocks#ignore          = ['delimited'] " ['definition', 'delimited']
let g:pandoc#syntax#codeblocks#embeds#use      = 1
let g:pandoc#syntax#codeblocks#embeds#langs    = ['yaml', 'ruby', 'haskell', 'sshconfig', 'help', 'systemd', 'bash = sh']
let g:pandoc#syntax#style#emphases             = 1
let g:pandoc#syntax#style#underline_special    = 1
let g:pandoc#syntax#style#use_definition_lists = 1
" pandoc#formatting
let g:pandoc#formatting#mode                   = 'ha' " Formatting: h/a/A/s
" pandoc#spell
let g:pandoc#spell#default_langs               = ['en']
" pandoc#folding
let g:pandoc#folding#mode                      = 'stacked'
let g:pandoc#folding#fastfolds                 = 1
let g:pandoc#folding#fold_fenced_codeblocks    = 1 " pandoc-folding-module
let g:pandoc#folding#fold_yaml                 = 1
"let g:pandoc#folding#fold_div_classes         = []
" pandoc#after
" Note: will always integrate with Goyo. Default enabled modules is empty.
let g:pandoc#after#modules#enabled = ['deoplete', 'fastfold', 'nrrwrgn', 'ultisnips']

" EditorConfig                                                       {{{1
let g:EditorConfig_exclude_patterns       = ['fugitive://.\*']
let g:EditorConfig_disable_rules          = ['trim_trailing_whitespace']
let g:EditorConfig_max_line_indicator     = 'line'  " 'none'
let g:EditorConfig_preserve_formatoptions = 1
let g:EditorConfig_verbose                = 0

" context_filetypes                                                  {{{1
let g:context_filetype#filetypes = get(g:,'context_filetype#filetypes',{})

" let g:context_filetype#filetypes.toml =
" [{'filetype':'vim', 'start' : "=\\s*'\\{3}\\zs", 'end' : "'\\{3}"}]

" vim-ghcid-quickfix                                                 {{{1
let g:ghcid_quickfix_showing = 'quickfix_on_start'  " 'quickfix_on_error'

" vim-orgmode                                                        {{{1
let g:org_indent                   = 0
let g:org_agenda_files             = []
let g:org_aggressive_conceal       = 1
let g:org_todo_keywords            = ['TODO', 'WAITING', '|', 'DONE']
let g:org_todo_keyword_faces       = [['TODO', 'magenta'], ['WAITING',[':foreground magenta',':slant italic']] ]
let g:org_heading_highlight_colors = [ 'Special', 'PreProc', 'Statement', 'Type', 'Identifier', 'Constant', 'Title' ]

" caw                                                                {{{1
let g:caw_zeropos_sp_blank  = ' '
let g:caw_dollarpos_sp_left = ' '

" bufferline                                                         {{{1
let g:bufferline_echo                = 1
let g:bufferline_active_buffer_left  = '['
let g:bufferline_active_buffer_right = ']'
let g:bufferline_modified            = '+'
let g:bufferline_show_bufnr          = 1
let g:bufferline_rotate              = 0 " 0, 1 or 2
let g:bufferline_fixed_index         = 1 " 0, 1 or -1
let g:bufferline_fname_mod           = ':t'
let g:bufferline_inactive_highlight  = 'StatusLineNC'
let g:bufferline_active_highlight    = 'StatusLine'
let g:bufferline_solo_highlight      = 0
let g:bufferline_excludes            = ['\[vimfiler\]']
let g:bufferline_pathshorten         = 1 " pathshorten() paths in buffer names?
let g:bufferline_separator           = ' '
