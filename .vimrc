" Use space as leader key
nnoremap <space> <nop>
let mapleader = "\<space>"

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

        Plugin 'godlygeek/tabular'
        Plugin 'plasticboy/vim-markdown'
        let g:vim_markdown_folding_disable = 1
        " let g:vim_markdown_folding_style_pythonic = 1
        " let g:vim_markdown_folding_level = 3
        let g:vim_markdown_frontmatter = 1 "YAML Front Matter
        set nofoldenable "Enable/Disable Folding 

        Plugin 'lervag/vimtex'
		let g:vimtex_quickfix_open_on_warning = 0
		let g:vimtex_index_split_pos = 'below'
		let g:vimtex_fold_enabled = 1
		let g:vimtex_view_method = 'okular'
		let g:vimtex_complete_img_use_tail = 1
		let g:vimtex_imaps_leader = ','

		let g:tex_stylish = 1
		let g:tex_conceal = ''
		let g:tex_flavor = 'latex'
		let g:tex_isk='48-57,a-z,A-Z,192-255,:'


		Plugin 'luochen1990/rainbow'
		let g:rainbow_active = 1
		let g:rainbow_conf = {
    \		'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
    \		'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
    \		'operators': '_,_',
    \		'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
    \		'separately': {
    \			'*': {},
    \			'tex': {
    \				'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
    \			},
    \			'vim': {
    \				'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
    \			},
    \		}
	\	}

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

	nnoremap <leader>s :split<cr>
	nnoremap <leader>v :vsplit<cr>
	
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

