" File: ~/.vim/plugin/maps.vim
scriptencoding utf-8

if get(g:,'no_vimrc_maps')
  finish
endif

" also see plugin/manpager.vim
if !exists(':Man')
  runtime ftplugin/man.vim
endif

" TODO vim-textobj-multiblock
" TODO vim-textobj-anyblock
" TODO vim-textobj-between

" Non-plugin mappings                                                      {{{1
nmap _ <Leader>
nmap - <LocalLeader>
nnoremap ; :
nnoremap Q q
nnoremap <C-@> <C-^>

" highlight current line
nnoremap <silent> <leader>hl ml:execute 'match Search /\%'.line('.').'l/'<cr>
" highlight indent errors
nnoremap <silent> <leader>hi :<c-u>IndentHighlight<cr>

" Folding                                                                  {{{1
noremap <Plug>(fold-replace)      :<c-u>call vimrc#fold_replace(v:count1)<cr>
noremap <Plug>(fold-renumber-inc) :call vimrc#fold_renumber(1)<cr>
noremap <Plug>(fold-renumber-dec) :call vimrc#fold_renumber(-1)<cr>
nmap    <Plug>(fold-increase)     V<Plug>(textobj-fold-i)<Plug>(fold-renumber-inc)
nmap    <Plug>(fold-decrease)     V<Plug>(textobj-fold-i)<Plug>(fold-renumber-dec)

nmap <s-tab> <Plug>(fold-cycle-open)
nmap zU <Plug>(FastFoldUpdate)
nmap z> <Plug>(fold-increase)
xmap z> <Plug>(fold-renumber-inc)
nmap Z< <Plug>(fold-decrease)
xmap z< <Plug>(fold-renumber-dec)
map  zf <Plug>(fold-replace)
"map  zF <Plug>(fold-replace) TODO: add comment character

" Misc.                                                                    {{{1
nnoremap <silent> <leader>o   :<c-u>Utl<cr>
nmap              <leader>sc  <Plug>Colorizer
nnoremap <silent> <leader>sn  :<c-u>NERDTreeToggle<cr>
nnoremap <silent> <leader>st  :<c-u>TagbarToggle<cr>
nnoremap <silent> <leader>sy  :<c-u>YRToggle<cr>

" Defx                                                                     {{{1
nnoremap <silent> <leader>sd
      \ :<c-u>Defx -toggle -split=vertical -winwidth=50 -direction=topleft<cr>
nnoremap <silent> <leader>f
      \ :<c-u>Defx -listed -resume -buffer-name=tab`tabpagenr()`<cr>

" speeddating <leader>d                                                    {{{1
nmap <leader>dn   <Plug>SpeedDatingUp
nmap <leader>dp   <Plug>SpeedDatingDown
nmap <leader>ds   <Plug>SpeedDatingNowLocal
nmap <leader>dS   <Plug>SpeedDatingNowUTC


" WindowSwap <leader>w <c-w>                                               {{{1
nnoremap <silent> <leader>wy :call WindowSwap#MarkWindowSwap()<cr>
nnoremap <silent> <leader>wp :call WindowSwap#DoWindowSwap()<cr>
nnoremap <silent> <leader>ws :call WindowSwap#EasyWindowSwap()<cr>
" Note: Overwrites vim defaults for CTRL-W H …J …K …L
nnoremap <silent> <c-w>H :<c-u>call vimrc#windowswap_wincmd(v:count1.'wincmd h', 'wincmd H')<cr>
nnoremap <silent> <c-w>J :<c-u>call vimrc#windowswap_wincmd(v:count1.'wincmd j', 'wincmd J')<cr>
nnoremap <silent> <c-w>K :<c-u>call vimrc#windowswap_wincmd(v:count1.'wincmd k', 'wincmd K')<cr>
nnoremap <silent> <c-w>L :<c-u>call vimrc#windowswap_wincmd(v:count1.'wincmd l', 'wincmd L')<cr>


" Vimwiki <leader>w                                                        {{{1
nmap <silent> <leader>wi  <Plug>VimwikiIndex
nmap <silent> <leader>wti <Plug>VimwikiTabIndex
nmap <silent> <leader>wg  <Plug>VimwikiUISelect
nmap <silent> <leader>wdi <Plug>VimwikiDiaryIndex
nmap <silent> <leader>wnd <Plug>VimwikiMakeDiaryNote
nmap <silent> <leader>wnt <Plug>VimwikiTabMakeDiaryNote
nmap <silent> <leader>wno <Plug>VimwikiMakeYesterdayDiaryNote
nmap <silent> <leader>wnf <Plug>VimwikiMakeTomorrowDiaryNote
nmap <silent> <leader>wh  <Plug>Vimwiki2HTML
nmap <silent> <leader>whh <Plug>Vimwiki2HTMLBrowse
"nmap <NOP>               <Plug>VimwikiDiaryGenerateLinks


" caw (comments) <leader>c                                                 {{{1
nmap <Leader>c <Plug>(caw:prefix)
xmap <Leader>c <Plug>(caw:prefix)

" op-replace <leader>r                                                     {{{1
map <silent> <leader>r <Plug>(operator-replace)

" op-surround <leader>s S                                                  {{{1
map <silent> sa <Plug>(operator-surround-append)
map <silent> sd <Plug>(operator-surround-delete)
map <silent> sr <Plug>(operator-surround-replace)
" Overwrites vim default mapping S
" Note: cl is the same as default mapping of s
" Note: cc is the same as default mapping of S
map <silent> S  <Plug>(operator-surround-replace)

" nmap <silent>sdb <Plug>(operator-surround-delete)<Plug>(textobj-between-a)
" nmap <silent>srb <Plug>(operator-surround-replace)<Plug>(textobj-between-a)

" CamelCase w b e                                                          {{{1
for s:w in ['w', 'b', 'e']
  execute 'vmap' s:w '<Plug>CamelCaseMotion_'.s:w
  execute 'omap' s:w '<Plug>CamelCaseMotion_'.s:w
  execute 'nmap' s:w '<Plug>CamelCaseMotion_'.s:w
  execute 'vmap i'.s:w '<Plug>CamelCaseMotion_i'.s:w
  execute 'omap i'.s:w '<Plug>CamelCaseMotion_i'.s:w
endfor

" Denite <leader>g                                                         {{{1
nnoremap <silent> <Leader>gf :<c-u>Denite file<cr>
nnoremap <silent> <Leader>gr :<c-u>Denite file_mru<cr>
nnoremap <silent> <Leader>gR :<c-u>Denite file/old<cr>
nnoremap <silent> <Leader>gb :<c-u>Denite buffer<cr>
nnoremap <silent> <Leader>gg :<c-u>Denite -buffer-name=search -no-empty grep<cr>
nnoremap <silent> <Leader>gn :<c-u>Denite dein<cr>
nnoremap <silent> <leader>gt :<C-u>Denite junkfile:new junkfile<cr>
"nnoremap <silent> <c-h>      :<c-u>DeniteCursorWord help<cr>
"nnoremap <expr>   <leader>/ line('$') > 10000 ? '/' : ":\<C-u>Denite -buffer-name=search -start-filter line\<cr>"
"nnoremap <expr>   <leader>n line('$') > 10000 ? 'n' : ":\<C-u>Denite -buffer-name=search -resume -refresh -no-start-filter\<cr>"
"nnoremap <expr>   <leader>* line('$') > 10000 ? '*' : ":\<C-u>DeniteCursorWord -buffer-name=search line\<cr>"

" Deoplete <C-g>                                                           {{{1
inoremap <expr> <C-g>0 pumvisible() ? deoplete#insert_candidate(0) : '0'
inoremap <expr> <C-g>1 pumvisible() ? deoplete#insert_candidate(1) : '1'
inoremap <expr> <C-g>2 pumvisible() ? deoplete#insert_candidate(2) : '2'
inoremap <expr> <C-g>3 pumvisible() ? deoplete#insert_candidate(3) : '3'
inoremap <expr> <C-g>4 pumvisible() ? deoplete#insert_candidate(4) : '4'

" LanguageClient <leader>l                                                 {{{1
nnoremap <leader>lc :call LanguageClient_contextMenu()<cr>
nnoremap <leader>lk :call LanguageClient#textDocument_hover()<cr>
nnoremap <leader>lg :call LanguageClient#textDocument_definition()<cr>
nnoremap <leader>lr :call LanguageClient#textDocument_rename()<cr>
nnoremap <leader>lf :call LanguageClient#textDocument_formatting()<cr>
nnoremap <leader>lb :call LanguageClient#textDocument_references()<cr>
nnoremap <leader>la :call LanguageClient#textDocument_codeAction()<cr>
nnoremap <leader>ls :call LanguageClient#textDocument_documentSymbol()<cr>
