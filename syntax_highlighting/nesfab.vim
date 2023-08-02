" Vim syntax file
" Language: NESFab
" Maintainer: Pubby

set expandtab

if exists("b:current_syntax")
  finish
endif

syn keyword nesfabKeyword if else for while do break continue return fn 
    \ ct nmi mode goto label file struct vars data omni ready fence irq
    \ default switch case asm charmap chrrom true false audio system stows 
    \ employs preserves state read write sizeof len push pop

syntax match nesfabId "_\{0,1}\l\k*"
syntax match nesfabType "_\{0,1}\u\k*"

syntax match nesfabGroup "/\k\+"

" Integer with - + or nothing in front
syn match nesfabNumber '\d\+\(\.\d*\|\)'
syn match nesfabNumberHex '\$\x\+\(\.\x*\|\)'
syn match nesfabNumberBin '%[01]\+\(\.[01]*\|\)'

" Comment
syn match nesfabCommentL "//.*$"
syntax region nesfabComment start=/\/\*/ end=/\*\//

" String
syn region nesfabString start="\"" skip=+\\\\\|\\"+ end="\""

let b:current_syntax = "nesfab"

hi def link nesfabNumber     Constant
hi def link nesfabNumberHex  Constant
hi def link nesfabNumberBin  Constant
hi def link nesfabCommentL   Comment
hi def link nesfabComment    Comment
hi def link nesfabString     String
hi def link nesfabKeyword    Statement
hi def link nesfabGroup      Identifier
hi def link nesfabType       Type
