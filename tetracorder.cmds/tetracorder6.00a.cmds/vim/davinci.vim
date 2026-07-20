" vim: ts=8

" Vim syntax file
" Language:	generic configure file from conf.vim and c.vim added davinci stuff
" Maintainer:	Roger Clark, psi.edu
" Last Change:	2017

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:ft = matchstr(&ft, '^\([^.]\)\+')

syn keyword	confTodo	contained TODO FIXME XXX
" Avoid matching "text#text", used in /etc/disktab and /etc/gettytab
syn match	confComment	"^#.*" contains=confTodo
syn match	confComment	"\s#.*"ms=s+1 contains=confTodo
syn region	confString	start=+"+ skip=+\\\\\|\\"+ end=+"+ oneline
syn region	confString	start=+'+ skip=+\\\\\|\\'+ end=+'+ oneline

syn keyword     cStatement      return continue filename type title
syn keyword     cLabel          case default
syn keyword     cConditional    if else
syn keyword     cRepeat         while for

" String and Character constants
" Highlight special characters (those which have a backslash) differently
"syn match       cSpecial        display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
"if !exists("c_no_utf")
"  syn match     cSpecial        display contained "\\\(u\x\{4}\|U\x\{8}\)"
"endif
"syn match       cSpecialCharacter display "L\='\\\o\{1,3}'"
"syn match       cSpecialCharacter display "'\\x\x\{1,2}'"
"syn match       cSpecialCharacter display "L'\\x\x\+'"

syn match parens       /[(){}]/
syn match doubleequal  /==/
syn match doubleor     /||/
syn match doubleand    /&&/
syn match lesseq       /<=/
syn match greatereq    />=/

"syn keyword basicLanguageKeywords for while if else printf write read scale verbose 
syn keyword basicLanguageKeywords write read scale verbose exit

syn keyword basicLanguageFunctions ascii atof atoi avg bil bip bsq dim double display fexists filetype float format fprintf fsize histogram int load min max org moment plot read_lines source sprintf stddev system xplot cat byte basename argv argc printf

"  *cterm-colors*
"  
"  NR-16   NR-8    COLOR NAME 
"  0       0       Black
"  1       4       DarkBlue
"  2       2       DarkGreen
"  3       6       DarkCyan
"  4       1       DarkRed
"  5       5       DarkMagenta
"  6       3       Brown, DarkYellow
"  7       7       LightGray, LightGrey, Gray, Grey
"  8       0*      DarkGray, DarkGrey
"  9       4*      Blue, LightBlue
"  10      2*      Green, LightGreen
"  11      6*      Cyan, LightCyan
"  12      1*      Red, LightRed
"  13      5*      Magenta, LightMagenta
"  14      3*      Yellow, LightYellow
"  15      7*      White

highlight pSpecial     term=NONE cterm=NONE ctermfg=LightRed    ctermbg=NONE gui=NONE guifg=NONE guibg=NONE
highlight doubleequal  term=NONE cterm=NONE ctermfg=LightBlue   ctermbg=NONE gui=NONE guifg=NONE guibg=NONE
highlight doubleor     term=NONE cterm=NONE ctermfg=LightBlue   ctermbg=NONE gui=NONE guifg=NONE guibg=NONE
highlight doubleand    term=NONE cterm=NONE ctermfg=LightBlue   ctermbg=NONE gui=NONE guifg=NONE guibg=NONE
highlight lesseq       term=NONE cterm=NONE ctermfg=LightBlue   ctermbg=NONE gui=NONE guifg=NONE guibg=NONE
highlight greatereq    term=NONE cterm=NONE ctermfg=LightBlue   ctermbg=NONE gui=NONE guifg=NONE guibg=NONE

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
" options (I think): Comment Keyword StorageClass String Number Operator
"
hi def link confComment             Comment
hi def link confTodo                Todo
hi def link confString              String
hi def link basicLanguageKeywords   Keyword
hi def link basicLanguageFunctions  StorageClass
hi def link cStatement              Statement
hi def link cRepeat                 Repeat
hi def link cConditional            Conditional
hi def link cLabel                  Label
hi def link cSpecial                cSpecial
hi def link cSpecialCharacter       cSpecial
hi def link parens                  pSpecial

let b:current_syntax = "davinci"

" vim: ts=8 sw=2
