"add syntax to all code files"
syntax on

"turn on 256 colors in vim"
set t_Co=256

"add new color sheme"
color jellybeans

"transfer a very long lines"
set wrap

"turn on autoindents in code files"
set ai

"turn on autoindents in C style"
set cin

"turn on line numbers"
set number

"set forward and backgorund colors"
"highlight Normal ctermfg=red ctermbg=darkgrey"

"?????"
"filetype indent plugin on"

set modeline

"it doesnt work!!!"
""""""""""""""""""""""""""
"set background = dark
"hi clear"
"""""""""""""""""""""""""

map <Esc><Esc> :w<CR>
nmap <c-s> :w<CR>
vmap <c-s> <Esc><c-s>gv
imap <c-s> <Esc><c-s>

"enter make a new line in normal mode"
nmap <S-Enter> O<Esc>
nmap <CR> o<Esc>

"Add pathogen for installing plugins into any folders"
"set nocp"

"execute pathogen#infect()"
"execute pathogen#helptags()"
"execute pathogen#runtime_append_all_bundles()"

"filetype plugin indent on"

"max opened insets in one window"
set tabpagemax=20

"turn on always show of tabs line"
set showtabline=2

