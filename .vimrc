""" Basic settings {{{
set nocompatible

set number

set cursorline

set showcmd

set tabstop=4
set shiftwidth=4
set smarttab
set et

set wrap
set autoindent
set smartindent

set showmatch
set hlsearch
set incsearch
set ignorecase
set smartcase

set textwidth=130
                                                    
set ruler

" The order of using encodings and files formats
set ffs=unix,dos,mac
set fencs=utf-8,cp1251,koi8-r,ucs-2,cp-866

set title                                           "enable setting title                           
set titlestring=VIM:\ %-25.55F\ %a%r%m titlelen=70  "configure title to look like: Vim /path/to/file

"set mouse=a

set wildmenu
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
