" Vim compiler file
" Compiler:         Pulp
" Maintainer:       Alex Sturtz <alex@parsesoftware.com>
" Latest Revision:  2019-06-01

if exists("current_compiler")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

CompilerSet errorformat=%E%trror\ found:%.%#
CompilerSet errorformat+=%E%trror\ %n%.%#
CompilerSet errorformat+=%W%tarning\ %n%.%#
CompilerSet errorformat+=%-C%.%#in\ module\ %o%.%#
CompilerSet errorformat+=%-C%.%#at\ %f:%l:%c%.%#
CompilerSet errorformat+=%-C
CompilerSet errorformat+=%C%*\\s%m
CompilerSet errorformat+=%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save
