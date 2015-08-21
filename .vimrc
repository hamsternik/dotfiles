""" Basic settings {{{
    set nocompatible

    set number

    " Tabulation 
    set tabstop=4
    set softtabstop=4
    set shiftwidth=4
    set expandtab!

    " Indentation
    set nowrap
    set autoindent
    set smartindent

    set showmatch

    " Searching {
        set hlsearch
        set incsearch

        set ignorecase
        set smartcase
    "}

    set cursorline

    set showcmd
    set ruler
    set rulerformat=%-13.(%l,%c%V%)\ %P

    set title
    set titlestring=VIM:\ %-25.55F\ %a%r%m titlelen=70

    set wildmenu
    set wildmode=longest:list,full

    set textwidth=120

    " The order of using encodings and files formats
    set ffs=unix,dos,mac
    set fencs=utf-8,cp1251,koi8-r,ucs-2,cp-866

    if has('mouse')
        set mouse=a
    endif

    filetype plugin indent on
    syntax on
""" }}}



""" Navigation & Mapping {{{
    let mapleader = ","

    nnoremap <silent> <leader>ev :e $MYVIMRC<CR>
    nnoremap <silent> <leader>sv :e $MYVIMRC<CR>

    nnoremap <C-c>n :cnext<CR>
    nnoremap <C-c>p :cprevious<CR>

    nnoremap <Enter> o<Esc>
    nnoremap <silent> <C-S> :<C-u>Update<CR>

    nnoremap <F2> :set hlsearch!<CR>

    inoremap <c-s> <c-o>:Update<CR>
    inoremap <S-Tab> <C-D>

    nnoremap <Tab> >>
    nnoremap <S-Tab> <<
    vnoremap > >gv
    vnoremap < <gv
    vnoremap <Tab> >
    vnoremap <S-Tab> <
""" }}} 


""" ColorScheme {{{
    if &t_Co < 256
        set t_Co=256
        set background=dark
        color atom-dark-256
    else
        set background=dark
        color atom-dark-256
    endif
""" }}}   
