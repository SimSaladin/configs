" File: ~/.vim/rc/options_nerdtree.vim
" Author: Samuli Thomasson
" Description: Settings for the NERDTree plugin
" Documentation: <url:vimhelp:NERDTree.txt>
" Plugins: <url:~/.vim/nerdtree_plugin/>

scriptencoding utf-8

let g:NERDTreeQuitOnOpen = 0
let g:NERDTreeAutoDeleteBuffer = 1

" Customized Mappings:

let g:NERDTreeMapOpenInTab       = 'T'      " (t) open in tab
let g:NERDTreeMapOpenInTabSilent = 't'      " (T) as above but keep focus on the tree
let g:NERDTreeMapOpenSplit       = 'i'      " (i) open selected file in split window
let g:NERDTreeMapPreviewSplit    = '<C-I>'  " (gi) as above but leave cursor on the tree
let g:NERDTreeMapOpenVSplit      = 's'      " (s) open selected file in vsplit window
let g:NERDTreeMapPreviewVSplit   = '<C-S>'  " (gs) as above but leave cursor on the tree

" o........Open files, directories and bookmarks......................|NERDTree-o|
" go.......Open selected file, but leave cursor in the NERDTree......|NERDTree-go|
"          Open selected bookmark dir in current NERDTree
"
" <CR>.....User-definable custom open action.......................|NERDTree-<CR>|
" O........Recursively open the selected directory....................|NERDTree-O|
" x........Close the current nodes parent.............................|NERDTree-x|
" X........Recursively close all children of the current node.........|NERDTree-X|
" e........Edit the current dir.......................................|NERDTree-e|
"
" D........Delete the current bookmark ...............................|NERDTree-D|
"
" P........Jump to the root node......................................|NERDTree-P|
" p........Jump to current nodes parent...............................|NERDTree-p|
" K........Jump up inside directories at the current tree depth.......|NERDTree-K|
" J........Jump down inside directories at the current tree depth.....|NERDTree-J|
" <C-J>....Jump down to next sibling of the current directory.......|NERDTree-C-J|
" <C-K>....Jump up to previous sibling of the current directory.....|NERDTree-C-K|
"
" C........Change the tree root to the selected dir...................|NERDTree-C|
" u........Move the tree root up one directory........................|NERDTree-u|
" U........Same as 'u' except the old root node is left open..........|NERDTree-U|
" r........Recursively refresh the current directory..................|NERDTree-r|
" R........Recursively refresh the current root.......................|NERDTree-R|
" m........Display the NERDTree menu..................................|NERDTree-m|
" cd.......Change the CWD to the dir of the selected node............|NERDTree-cd|
" CD.......Change tree root to the CWD...............................|NERDTree-CD|
"
" I........Toggle whether hidden files displayed......................|NERDTree-I|
" f........Toggle whether the file filters are used...................|NERDTree-f|
" F........Toggle whether files are displayed.........................|NERDTree-F|
" B........Toggle whether the bookmark table is displayed.............|NERDTree-B|
"
" q........Close the NERDTree window..................................|NERDTree-q|
" A........Zoom (maximize/minimize) the NERDTree window...............|NERDTree-A|
" ?........Toggle the display of the quick help.......................|NERDTree-?|

" Statusline                                                        {{{1
let g:NERDTreeStatusline = "%{exists('b:NERDTree')?substitute(b:NERDTree.root.path.str(),'^'.$HOME.'/','~/',''):''}"

" Setting CWD                                                       {{{1
let g:NERDTreeUseTCD    = 1
let g:NERDTreeChDirMode = 1

" OpenArgs (for NERDTree-<CR>)                                      {{{1
" where    := h | v | t | p
" reuse    := all | currenttab | ""
" keepopen := 0|1  (close the tree window?)
" stay     := 0|1  (remain in tree window after opening?)
let g:NERDTreeCustomOpenArgs = {
  \ 'file':{'reuse':'all','where':'p','keepopen':1,'stay':1},
  \ 'dir':{}}

" Bookmarks                                                         {{{1
let g:NERDTreeShowBookmarks = 1
let g:NERDTreeBookmarksFile = get(g:,'myvimrc#datadir',$HOME).'/NERDTreeBookmarks'

" UI, icons and symbols                                             {{{1
let g:NERDTreeMinimalUI           = 1
let g:NERDTreeWinSize             = 46
let g:NERDTreeAutoCenter          = 0
let g:NERDTreeDirArrowExpandable  = '▸' " >
let g:NERDTreeDirArrowCollapsible = '▾' " v

" Menu                                                              {{{1
let g:NERDTreeMinimalMenu = 1

" Sorting                                                           {{{1
let g:NERDTreeNaturalSort     = 1
let g:NERDTreeSortHiddenFirst = 1
let g:NERDTreeSortOrder       = ['\/$','\.md$','\.l?hs$','*','\.swp$','\.bak$','\~$']

" Ignored                                                           {{{1
let g:NERDTreeRespectWildIgnore = 1

let g:NERDTreeIgnore = ['\~$','.aes$','^dist$','.dvi$','.aux$','.toc$','.1$','.lock$']
