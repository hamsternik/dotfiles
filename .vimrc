"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                preamble                                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set nocompatible

" Necessary for Vundle, should be turned on after plugins setup
filetype off

" Use space as leader key
nnoremap <space> <nop>
let mapleader = "\<space>"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                          Vundle configuration                           "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage itself
Plugin 'VundleVim/Vundle.vim'

Plugin 'vim-ruby/vim-ruby'
Plugin 'klen/python-mode'
Plugin 'jcfaria/Vim-R-plugin'
Plugin 'keith/swift.vim'
Plugin 'dart-lang/dart-vim-plugin'
Plugin 'neovimhaskell/haskell-vim'

Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-fugitive'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'JamshedVesuna/vim-markdown-preview'
Plugin 'lervag/vimtex'
Plugin 'luochen1990/rainbow'
Plugin 'lepture/vim-velocity'
Plugin 'danro/rename.vim'
 
call vundle#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                        turn on filetype plugins                         "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

filetype plugin on
filetype indent plugin on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           vimrc augroup                                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" TODO: This custom group should use more precisely
augroup vimrc
    autocmd!
augroup END

autocmd! vimrc bufwritepost .vimrc source %

"" Commenting blocks of code
autocmd FileType c,cpp,m,java,scala,swift   let b:comment_leader = '// '
autocmd FileType sh,ruby,python             let b:comment_leader = '# '
autocmd FileType conf,fstab                 let b:comment_leader = '# '
autocmd FileType tex                        let b:comment_leader = '% '
autocmd FileType vim                        let b:comment_leader = '" '

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                            General settings                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set number

set clipboard=unnamed
set backspace=indent,eol,start

set exrc
set secure

set autoread

" Tabulation 
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab

" Indentation
set nowrap
set autoindent
set smartindent
set colorcolumn=120
highlight ColorColumn ctermbg=222

set showmatch

" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

set showcmd
set ruler
set rulerformat=%-13.(%l,%c%V%)\ %P

set textwidth=120

set wildmenu
set wildmode=longest:list,full
set listchars=tab:▸\ ,eol:¬

" The order of using encodings and files formats
set ffs=unix,dos,mac
set fencs=utf-8,cp1251,koi8-r,ucs-2,cp-866

" Set up mouse in all vim modes
if has('mouse')
    set mouse=a
endif

" macOS only
if has("mac") || has("macunix")
    set ambiwidth=double
endif

if &t_Co > 2 || has("gui_running")
    syntax on
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               Color Scheme                            "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

colorscheme molokai
set cursorline 

if &t_Co < 256
    set t_Co=256
    " let g:molokai_original = 1  " Original monokai bg scheme
    let g:rehash256 = 1       " Alternative 256 colors scheme
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               Keybindings                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Fast saving
nnoremap <leader>w :w!<cr>

nnoremap <leader>s :split<cr>
nnoremap <leader>v :vsplit<cr>

" easier moving between tabs, include tabs creating
nnoremap <silent> <leader>t :tabnew<cr>
nnoremap <silent> m :tabnext<cr>
nnoremap <silent> M :tabprevious<cr>

map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

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

" manual formatting of paragraphs
vmap Q gq
nmap Q gqap

" select ALL
map <silent> <C-A> ggVG 

" cancel searching highlight
nnoremap ; :nohlsearch<CR>

noremap <silent> <leader>/ :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> <leader>\ :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>

" <leader>V reloads it and makes all changes active (file has to be saved first)
noremap <silent> <leader>V :source $MYVIMRC<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           *** PLUGINS SETUP ***                       "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               python-mode                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let ropevim_enable_shortcuts = 1
"let g:pymode_python = 'python3'
let g:pymode_syntax = 1
let g:pymode_syntax_builtin_objs = 0
let g:pymode_syntax_builtin_funcs = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               vim-markdown                            "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:vim_markdown_folding_disable = 1
" let g:vim_markdown_folding_style_pythonic = 1
" let g:vim_markdown_folding_level = 3
let g:vim_markdown_frontmatter = 1 "YAML Front Matter
set nofoldenable "Enable/Disable Folding 

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           vim-markdown-preview                        "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let vim_markdown_preview_github=1 " GitHub flavoured markdown
let vim_markdown_preview_toggle=1
let vim_markdown_preview_hotkey='<C-p>' " Default: <C-p>
let vim_markdown_preview_browser='Google Chrome'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   vimtex                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:vimtex_latexmk_enable=1
" let g:vimtex_imaps_leader = ','

let g:tex_flavor = 'latex'
let g:vimtex_latexmk_options='-pdfdvi'
" let g:vimtex_view_method = "okular"
" let g:vimtex_view_general_viewer="okular --unique"
let g:vimtex_latexmk_continuous = 1
let g:vimtex_latexmk_background = 1
let g:vimtex_latexmk_callback = 0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   rainbow                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   vim-airline                         "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set noshowmode
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'tomorrow'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   nerd-tree                           "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

silent! nnoremap <F2> :NERDTreeToggle<CR>
silent! nnoremap <F3> :NERDTreeFind<CR>

"let NERDTreeShowHidden=1
autocmd VimEnter * NERDTree
autocmd VimEnter * wincmd p

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               nerd-commenter                          "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              haskell-vim                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:haskell_classic_highlighting = 1
let g:haskell_indent_if = 3
let g:haskell_indent_case = 2
let g:haskell_indent_let = 4
let g:haskell_indent_where = 6
let g:haskell_indent_before_where = 2
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_do = 3
let g:haskell_indent_in = 1
let g:haskell_indent_guard = 2
let g:haskell_indent_case_alternative = 1
let g:cabal_indent_section = 2

