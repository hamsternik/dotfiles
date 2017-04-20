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
    
        Plugin 'klen/python-mode'

        "" R-plugin {{
        Plugin 'jcfaria/Vim-R-plugin'
        "" }}

        "" NERDTree {{
        Plugin 'scrooloose/nerdtree'
        silent! nnoremap <F2> :NERDTreeToggle<CR>
        silent! nnoremap <F3> :NERDTreeFind<CR>

        "let NERDTreeShowHidden=1
        autocmd VimEnter * NERDTree
        autocmd VimEnter * wincmd p

        autocmd StdinReadPre * let s:std_in=1
        autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

        autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
        "" }}

        "" Markdown {{
        Plugin 'godlygeek/tabular'
        Plugin 'plasticboy/vim-markdown'
        let g:vim_markdown_folding_disable = 1
        " let g:vim_markdown_folding_style_pythonic = 1
        " let g:vim_markdown_folding_level = 3
        let g:vim_markdown_frontmatter = 1 "YAML Front Matter
        set nofoldenable "Enable/Disable Folding 
        "" }}

        "" Markdown Preview {{
        Plugin 'JamshedVesuna/vim-markdown-preview'
        let vim_markdown_preview_github=1 " GitHub flavoured markdown
        let vim_markdown_preview_toggle=1
        let vim_markdown_preview_hotkey='<C-p>' " Default: <C-p>
        let vim_markdown_preview_browser='Google Chrome'
        "" }}

        "" LaTeX {{
        Plugin 'lervag/vimtex'
        let g:vimtex_latexmk_enable=1
		" let g:vimtex_imaps_leader = ','

		let g:tex_flavor = 'latex'
        let g:vimtex_latexmk_options='-pdfdvi'
		" let g:vimtex_view_method = "okular"
        " let g:vimtex_view_general_viewer="okular --unique"
        let g:vimtex_latexmk_continuous = 1
        let g:vimtex_latexmk_background = 1
        let g:vimtex_latexmk_callback = 0
        "" }}

        "" Rainbow {{
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
        "" }}
        
        "" Airline bar {{
        Plugin 'bling/vim-airline'
        set noshowmode
        set laststatus=2
        let g:airline#extensions#tabline#enabled = 1
        "" }}

        "" Fugitive -- git plugin {{
        Plugin 'tpope/vim-fugitive'
        "" }} 

        "" vim-velocity {{
        Plugin 'lepture/vim-velocity'
        "au BufNewFile,BufRead *.vm,*.html,*.htm,*.shtml,*.stm set ft=velocity
        "" }}

		"" nerdcommenter {{
		Plugin 'scrooloose/nerdcommenter'
		" Add spaces after comment delimiters by default
		let g:NERDSpaceDelims = 1
		
		" Use compact syntax for prettified multi-line comments
		let g:NERDCompactSexyComs = 1
		
		" Align line-wise comment delimiters flush left instead of following code indentation
		let g:NERDDefaultAlign = 'left'
		
		" Set a language to use its alternate delimiters by default
		let g:NERDAltDelims_java = 1
		
		" Add your own custom formats or override the defaults
		let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }
		
		" Allow commenting and inverting empty lines (useful when commenting a region)
		let g:NERDCommentEmptyLines = 1
		
		" Enable trimming of trailing whitespace when uncommenting
		let g:NERDTrimTrailingWhitespace = 1
		"" }}

    Bundle 'vim-ruby/vim-ruby'

    call vundle#end()

    filetype plugin on
    filetype indent plugin on
""" }}}


""" Basic settings {{{

    set nocompatible

    set clipboard=unnamed
    set backspace=indent,eol,start

    set number

    set exrc
    set secure

    set autoread

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
    
    "" map nn :next<CR> map nb :previous<CR>
    "" map nb :previous<CR>


    "" Commenting blocks of code
    autocmd FileType c,cpp,m,java,scala,swift   let b:comment_leader = '// '
    autocmd FileType sh,ruby,python             let b:comment_leader = '# '
    autocmd FileType conf,fstab                 let b:comment_leader = '# '
    autocmd FileType tex                        let b:comment_leader = '% '
    autocmd FileType vim                        let b:comment_leader = '" '

    "noremap <silent> <leader>/ :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
    "noremap <silent> <leader>\ :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>

""" }}} 


""" ColorScheme {{{

    colorscheme molokai
    set cursorline 

    if &t_Co < 256
        set t_Co=256
        " let g:molokai_original = 1  " Original monokai bg scheme
        let g:rehash256 = 1       " Alternative 256 colors scheme
    endif
""" }}}   

