" File: ~/.vim/plugin/maps.vim
scriptencoding utf-8

if get(g:,'no_vimrc_maps')
  finish
endif

" TODO vim-textobj-multiblock
" TODO vim-textobj-anyblock
" TODO vim-textobj-between

" mapleader & maplocalleader {{{1
nmap _ <Leader>
nmap - <LocalLeader>
  " NOTE: Leader      ',' or '_'
  " NOTE: LocalLeader '\' or '-'

" ; is easier to type on programmer's dvorak {{{1
nnoremap ; :

" CTRL-@ is pretty useless but easy to type on programmers dvorak,  {{{1
" so remap it to more useful CTRL-^. Remember also CTRL-W_CTRL-^.
nnoremap <C-@> <C-^>

" Q: Disable entering into Ex mode with Q. (prefer gQ)              {{{1
nnoremap Q :

" Remap , to ,, to make single , a nice map leader character        {{{1
nnoremap ,, ,
xnoremap ,, ,
onoremap ,, ,

" rxvt-unicode mapped modifiers                                     {{{1

" Syntax: [27;<MODS>;<KEYCODE>

map  [27;2;8~   <C-BS>
imap [27;2;8~   <C-BS>

map  [27;2;9~   <C-Tab>
imap [27;2;9~   <C-Tab>

map  [27;2;127~ <C-Delete>
imap [27;2;127~ <C-Delete>

" CamelCase: re-define w b e mappings and iw, ib, ie motions        {{{1
for s:w in ['w','b','e']
  execute 'map'      s:w '<Plug>CamelCaseMotion_'  . s:w
  execute 'sunmap'   s:w
  execute 'omap i' . s:w '<Plug>CamelCaseMotion_i' . s:w
  execute 'xmap i' . s:w '<Plug>CamelCaseMotion_i' . s:w
endfor

" Highlight things: <Leader>h{li}                 {{{1
" highlight current line
nnoremap <silent> <Leader>hl ml:execute 'match Search /\%'.line('.').'l/'<CR>
" highlight indent errors
nnoremap <silent> <Leader>hi :<C-u>IndentHighlight<CR>
" Enable colorizer plugin
nmap     <silent> <Leader>hc <Plug>Colorizer

" Fold stuff                                                        {{{1
nmap     <silent> <s-tab> <Plug>(fold-cycle-open)
nmap     <silent> zU      <Plug>(FastFoldUpdate)
xmap     <silent> z>      <Plug>(fold-renumber-inc)
xmap     <silent> z<      <Plug>(fold-renumber-dec)
nmap     <silent> z>      <Plug>(fold-increase)
nmap     <silent> Z<      <Plug>(fold-decrease)
map      <silent> zf      <Plug>(fold-replace)
"map     <silent> zF      <Plug>(fold-replace)    |" TODO: add comment character

noremap  <silent> <Plug>(fold-renumber-inc) :call vimrc#fold_renumber(1)<CR>
noremap  <silent> <Plug>(fold-renumber-dec) :call vimrc#fold_renumber(-1)<CR>
noremap  <silent> <Plug>(fold-replace)      :<C-u>call vimrc#fold_replace(v:count1)<CR>
nmap     <silent> <Plug>(fold-increase)     V<Plug>(textobj-fold-i)<Plug>(fold-renumber-inc)
nmap     <silent> <Plug>(fold-decrease)     V<Plug>(textobj-fold-i)<Plug>(fold-renumber-dec)

" Utl                                                               {{{1
nnoremap <silent> <leader>o   :<C-u>Utl openLink<CR>
nnoremap <silent> <leader>O   :<C-u>Utl openLink currentFile<CR>
nnoremap <silent> <leader>yl  :<C-u>Utl copyLink<CR>
nnoremap <silent> <leader>yf  :<C-u>Utl copyFileName currentFile<CR>

" Tagbar                                                            {{{1
nnoremap <silent> <leader>st  :<C-u>TagbarToggle<CR>

" YankRing                                                          {{{1
nnoremap <silent> <leader>yy  :<C-u>YRShow<CR>

" NERDTree                                                          {{{1
nnoremap <silent> <leader>n   :<C-u>NERDTreeToggle<CR>
nnoremap <silent> <leader>N   :<C-u>NERDTree %<CR>

" Defx                                                              {{{1
nnoremap <silent> <Leader>sd  :<C-u>Defx -toggle -split=vertical -winwidth=50 -direction=topleft<CR>
nnoremap <silent> <Leader>f   :<C-u>Defx -listed -resume -buffer-name=tab`tabpagenr()`<CR>

" SpeedDating <leader>d {n,p,s,S}                                   {{{1
nmap     <silent> <Leader>dn  <Plug>SpeedDatingUp
nmap     <silent> <Leader>dp  <Plug>SpeedDatingDown
nmap     <silent> <Leader>ds  <Plug>SpeedDatingNowLocal
nmap     <silent> <Leader>dS  <Plug>SpeedDatingNowUTC

" WindowSwap <Leader>w [yps], <C-w>[HJKL]                           {{{1
nnoremap <silent> <Leader>wy  :<C-u>call WindowSwap#MarkWindowSwap()<CR>
nnoremap <silent> <Leader>wp  :<C-u>call WindowSwap#DoWindowSwap()<CR>
nnoremap <silent> <Leader>ws  :<C-u>call WindowSwap#EasyWindowSwap()<CR>
nnoremap <silent> <C-w>H      :<C-u>call vimrc#windowswap_wincmd(v:count1.'wincmd h','wincmd H')<CR>
nnoremap <silent> <C-w>J      :<C-u>call vimrc#windowswap_wincmd(v:count1.'wincmd j','wincmd J')<CR>
nnoremap <silent> <C-w>K      :<C-u>call vimrc#windowswap_wincmd(v:count1.'wincmd k','wincmd K')<CR>
nnoremap <silent> <C-w>L      :<C-u>call vimrc#windowswap_wincmd(v:count1.'wincmd l','wincmd L')<CR>
                " Note: Overwrites vim defaults for CTRL-W H …J …K …L

" Vimwiki <leader>w {i,ti,g,di,nd,nt,no,nf,h,hh}                    {{{1
nmap     <silent> <Leader>wi   <Plug>VimwikiIndex
nmap     <silent> <Leader>wti  <Plug>VimwikiTabIndex
nmap     <silent> <Leader>wg   <Plug>VimwikiUISelect
nmap     <silent> <Leader>wdi  <Plug>VimwikiDiaryIndex
nmap     <silent> <Leader>wnd  <Plug>VimwikiMakeDiaryNote
nmap     <silent> <Leader>wnt  <Plug>VimwikiTabMakeDiaryNote
nmap     <silent> <Leader>wno  <Plug>VimwikiMakeYesterdayDiaryNote
nmap     <silent> <Leader>wnf  <Plug>VimwikiMakeTomorrowDiaryNote
nmap     <silent> <Leader>wh   <Plug>Vimwiki2HTML
nmap     <silent> <Leader>whh  <Plug>Vimwiki2HTMLBrowse
"nmap             <NOP>        <Plug>VimwikiDiaryGenerateLinks

" caw (comments) <Leader>c                                          {{{1
nmap     <silent> <Leader>c           <Plug>(caw:prefix)
xmap     <silent> <Leader>c           <Plug>(caw:prefix)
nmap     <silent> <Plug>(caw:prefix)A <Plug>(caw:dollarpos:comment)
nmap     <silent> <Plug>(caw:prefix)I <Plug>(caw:zeropos:comment)

" op-replace <leader>r                                              {{{1
map      <silent> <Leader>r    <Plug>(operator-replace)

" op-surround <Leader>s [adrS]                                      {{{1
map      <silent> sa           <Plug>(operator-surround-append)
map      <silent> sd           <Plug>(operator-surround-delete)
map      <silent> sr           <Plug>(operator-surround-replace)
map      <silent> S            <Plug>(operator-surround-replace)
	  " Note: Overwrites vim default mapping S:
	  "       cl is the same as default mapping of s
	  "       cc is the same as default mapping of S

" nmap <silent>sdb <Plug>(operator-surround-delete)<Plug>(textobj-between-a)
" nmap <silent>srb <Plug>(operator-surround-replace)<Plug>(textobj-between-a)

" Denite <leader>g...                                               {{{1
"
" Buffer Mappings: <url:../ftplugin/denite_maps.vim>
"
" Args: <url:vimhelp:denite-options>

nnoremap <silent> <Leader>g/   :<C-u>Denite file -buffer-name=file -start-filter<CR>
nnoremap <silent> <Leader>gf   :<C-u>Denite file_mru -buffer-name=recent-file<CR>
nnoremap <silent> <Leader>g<C-R> :<C-u>Denite file/old<CR>
nnoremap <silent> <Leader>gb   :<C-u>Denite buffer -buffer-name=buf -immediately-1<CR>
nnoremap <silent> <Leader>gr   :<C-u>Denite register -buffer-name=reg<CR>
nnoremap <silent> <Leader>gg   :<C-u>Denite grep -buffer-name=search -no-empty<CR>
nnoremap <silent> <Leader>gn   :<C-u>Denite dein<CR>
nnoremap <silent> <leader>gt   :<C-u>Denite tag -buffer-name=tag<CR>
nnoremap <silent> <leader>gl   :<C-u>Denite line -buffer-name=line<CR>
nnoremap <silent> <leader>g!   :<C-u>Denite command_history -buffer-name=command -reversed -winheight=8<CR>
nnoremap <silent> <leader>gj   :<C-u>Denite jump -buffer-name=jump<CR>
nnoremap <silent> <leader>g#   :<C-u>Denite junkfile:new junkfile<CR>

" Deol {{{1
nnoremap <silent> <leader>g$ :<C-U>exe 'Deol -split' '-cwd='.fnamemodify(expand('%'),':h')<CR>
tnoremap <Esc> <C-\><C-n>

" Deoplete <C-g> {0..4}                                             {{{1
inoremap <expr> <C-g>0 pumvisible() ? deoplete#insert_candidate(0) : '0'
inoremap <expr> <C-g>1 pumvisible() ? deoplete#insert_candidate(1) : '1'
inoremap <expr> <C-g>2 pumvisible() ? deoplete#insert_candidate(2) : '2'
inoremap <expr> <C-g>3 pumvisible() ? deoplete#insert_candidate(3) : '3'
inoremap <expr> <C-g>4 pumvisible() ? deoplete#insert_candidate(4) : '4'

" LanguageClient <leader>l [ckgrfbas]                               {{{1
nnoremap <silent> <leader>lc :call LanguageClient_contextMenu()<CR>
nnoremap <silent> <leader>lk :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> <leader>lg :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <leader>lr :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent> <leader>lf :call LanguageClient#textDocument_formatting()<CR>
nnoremap <silent> <leader>lb :call LanguageClient#textDocument_references()<CR>
nnoremap <silent> <leader>la :call LanguageClient#textDocument_codeAction()<CR>
nnoremap <silent> <leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

" vim-bookmarks                                                     {{{1
nmap     <silent> <Leader>m     <Plug>BookmarkToggle
nmap     <silent> <Leader>i     <Plug>BookmarkAnnotate   |" <TEXT>
nmap     <silent> <Leader>a     <Plug>BookmarkShowAll
nmap     <silent> <Leader>j     <Plug>BookmarkNext
nmap     <silent> <Leader>k     <Plug>BookmarkPrev
nmap     <silent> <Leader>x     <Plug>BookmarkClear
nmap     <silent> <Leader>X     <Plug>BookmarkClearAll
nmap     <silent> <Leader>kk    <Plug>BookmarkMoveUp     |" [<COUNT>]
nmap     <silent> <Leader>jj    <Plug>BookmarkMoveDown   |" [<COUNT>]
nmap     <silent> <Leader>G     <Plug>BookmarkMoveToLine |" <LINE>
nmap     <silent> <Leader>S     <Plug>BookmarkSave       |" <FILE_PATH>
nmap     <silent> <Leader>L     <Plug>BookmarkLoad       |" <FILE_PATH>
