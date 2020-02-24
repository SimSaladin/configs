" File: ~/.vim/rc/options_quickfixsigns.vim
" Author: Samuli Thomasson
" Description: Options for quickfixsigns plugin

scriptencoding utf-8

let g:quickfixsigns_classes      = ['qfl', 'loc', 'marks', 'vcsdiff', 'vcsmerge']
let g:quickfixsigns_echo_map     = '<leader>qq'
let g:quickfixsigns_echo_balloon = 1

" Signs

" XXX: some after/syntax instead?

sign define QFS_LOC        text=│◊ texthl=Identifier
sign define QFS_LOC_E      text=│E texthl=WarningMsg
sign define QFS_LOC_W      text=│W texthl=WarningMsg
sign define QFS_LONGLINES  text=│$ texthl=Identifier
sign define QFS_QFL        text=│► texthl=WarningMsg
sign define QFS_QFL_E      text=║E texthl=WarningMsg
sign define QFS_QFL_W      text=║W texthl=WarningMsg
sign define QFS_VCS_ADD    text=++ texthl=MySignDiffAdd
sign define QFS_VCS_CHANGE text=== texthl=MySignDiffChange
sign define QFS_VCS_DEL    text=-  texthl=MySignDiffDelete
sign define QFS_VCS_DEL1   text=-1 texthl=MySignDiffDelete
sign define QFS_VCS_DEL2   text=-2 texthl=MySignDiffDelete
sign define QFS_VCS_DEL3   text=-3 texthl=MySignDiffDelete
sign define QFS_VCS_DEL4   text=-4 texthl=MySignDiffDelete
sign define QFS_VCS_DEL5   text=-5 texthl=MySignDiffDelete
sign define QFS_VCS_DEL7   text=-7 texthl=MySignDiffDelete
sign define QFS_VCS_DEL8   text=-8 texthl=MySignDiffDelete
sign define QFS_VCS_DEL9   text=-9 texthl=MySignDiffDelete
sign define QFS_VCS_DELM   text=-- texthl=MySignDiffDelete
