""" Basic settings {{{
set nocompatible                                    "vim automatically enable if see ~/.vimrc

set number                                          "enable line numbers 

set cursorline                                      "highlight current line

set showcmd                                         "show a command in status line

set tabstop=4                                       "number of spaces that a <Tab> in the file counts for
set shiftwidth=4                                    "number of spaces to use for each step of (auto)indent
set smarttab
set et                                              "enable autocorrect by default

set wrap                                            "wrap long lines
set autoindent                                      "enable auto-indent for new lines
set smartindent                                     "enable auto-indent in C style

set showmatch
set hlsearch
set incsearch
set ignorecase
set smartcase

set textwidth=130                                   "max width of text that is being inserted,               
                                                    "longer line will be broken
                                                    
set ruler                                           "show the line and column number of the cursor position,
                                                    "separated by a coma

" The order of using encodings and files formats
set ffs=unix,dos,mac
set fencs=utf-8,cp1251,koi8-r,ucs-2,cp-866

set title                                           "enable setting title                           
set titlestring=VIM:\ %-25.55F\ %a%r%m titlelen=70  "configure title to look like: Vim /path/to/file

"set mouse=a                                        "enable mouse using

set wildmenu                                        "enables a menu at the bottom of the vim/gvim window
set wildmode=longest:list,full

filetype plugin indent on
syntax on
"""" }}}


" Navigation {{{
"nnoremap H ^                                        "start of line
"nnoremap L $                                        "end of line

nnoremap <C-c>n :cnext<CR>
nnoremap <C-c>p :cprevious<CR>

nnoremap <Enter> o<Esc>
nnoremap <silent> <C-S> :<C-u>Update<CR>

inoremap <c-s> <c-o>:Update<CR>

nnoremap <Tab> >>
nnoremap <S-Tab> <<
inoremap <S-Tab> <C-D>
vnoremap > >gv
vnoremap < <gv
vnoremap <Tab> >
vnoremap <S-Tab> <
" }}} 


" ColorScheme {{{
if &t_Co < 256
    set t_Co=256
    set background=dark
    color atom-dark-256
else
    set background=dark
    color atom-dark-256
endif
" }}}   
