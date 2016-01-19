" Put this at the top of your .vimrc to use Vundle. (c)
""" Vundle {{{

    set nocompatible
    filetype off
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()

        " let Vundle manage itself
        Plugin 'VundleVim/Vundle.vim'
    
        "other plugins
        Plugin 'Valloric/YouCompleteMe'
        Plugin 'scrooloose/nerdtree'
        Plugin 'bling/vim-airline'
        Plugin 'klen/python-mode'


    call vundle#end()

    filetype plugin on
    filetype indent plugin on
""" }}}


""" Plugin setting {{{

    " vim-airline
    set noshowmode
    set laststatus=2
    let g:airline#extensions#tabline#enabled = 1
""" }}}


""" Basic settings {{{

    set nocompatible

    set number

    set exrc
    set secure

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

    set showcmd
    set ruler
    set rulerformat=%-13.(%l,%c%V%)\ %P

    set wildmenu
    set wildmode=longest:list,full

    set textwidth=120

    " The order of using encodings and files formats
    set ffs=unix,dos,mac
    set fencs=utf-8,cp1251,koi8-r,ucs-2,cp-866

    if has('mouse')
        set mouse=a
    endif

    " if need
    if has("mac") || has("macunix")
        set ambiwidth=double
    endif

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

    inoremap <c-s> <c-o>:Update<CR>
    inoremap <S-Tab> <C-D>

    nnoremap <Tab> >>
    nnoremap <S-Tab> <<
    vnoremap > >gv
    vnoremap < <gv
    vnoremap <Tab> >
    vnoremap <S-Tab> <

    nnoremap gr :grep <cword> *<CR>
    nnoremap Gr :grep <cword> %:p:h/*<CR>
    nnoremap gR :grep '\b<cword>\b' *<CR>
    nnoremap GR :grep '\b<cword>\b' %:p:h/*<CR>

    " select ALL
    map <C-A> ggVG 

    " cancel searching highlight
    nnoremap ; :nohlsearch<CR>
""" }}} 


""" ColorScheme {{{

    if &t_Co < 256
        set t_Co=256
        
        let g:molokai_original = 1
        let g:rehash256 = 1

        set cursorline 

        colorscheme molokai
    else
        
        let g:molokai_original = 1
        let g:rehash256 = 1

        set cursorline 

        colorscheme molokai
    endif
""" }}}   

