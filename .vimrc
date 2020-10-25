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

Plugin 'jph00/swift-apple'
Plugin 'vim-ruby/vim-ruby'
Plugin 'klen/python-mode'
Plugin 'jcfaria/Vim-R-plugin'
Plugin 'dart-lang/dart-vim-plugin'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'xu-cheng/brew.vim'

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
Plugin 'kana/vim-submode'
" BClose -- deleting a buffer without closing the window (for NeoVim). Need for LF below.
Plugin 'rbgrouleff/bclose.vim'
Plugin 'ptzz/lf.vim'
Plugin 'editorconfig/editorconfig-vim'

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
    " Automatically delete trailing DOS-returns and whitespace on file open and write.
    autocmd BufRead,BufWritePre,FileWritePre * silent! %s/[\r \t]\+$//
augroup END

" autocmd! vimrc bufwritepost .vimrc source %

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

if has('unnamedplus')
        " By default, Vim will not use the system clipboard when yanking/pasting to
    " the default register. This option makes Vim use the system default
    " clipboard.
    " Note that on X11, there are _two_ system clipboards: the "standard" one, and
    " the selection/mouse-middle-click one. Vim sees the standard one as register
    " '+' (and this option makes Vim use it by default) and the selection one as
    " '*'.
    " See :h 'clipboard' for details.
    set clipboard=unnamedplus,unnamed
else
    " Vim now also uses the selection system clipboard for default yank/paste.
    set clipboard+=unnamed
endif

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

set t_Co=256
" set cursorline

colorscheme molokai
let g:molokai_original = 1  " Original monokai bg scheme
let g:rehash256 = 1       " Alternative 256 colors scheme
set background=dark

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               Keybindings                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enter (make new line and get into `insert` mode)
nnoremap <Enter> o<Esc>

"
" Ctrl+s (fast saving)
nnoremap <silent> <C-s> <esc>:w<CR>
inoremap <silent> <C-s> <esc>:w<CR>
"

" leader+s/+v (Horizontal/Vertical window split)
nnoremap <leader>s :split<cr>
nnoremap <leader>v :vsplit<cr>

" Ctrl+t/+[/+] (easier moving between tabs, include tabs creating)
nnoremap <silent> <C-t> :tabnew<cr>
nnoremap <silent> <C-]> :tabnext<cr>
nnoremap <silent> <C-[> :tabprevious<cr>

" Jump to bottom/top/left/rigth split
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

nnoremap <silent> <C-u> :<C-u>Update<CR>
" inoremap <c-s> <c-o>:Update<CR>

" Tab/Shift-Tab mapping for each Vim mode
nnoremap <Tab> >>
nnoremap <S-Tab> <<
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv
inoremap <S-Tab> <C-D>

" basic Vim grep
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
noremap <silent> <leader>R :source $MYVIMRC<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>


" Map +/- keys with leader to change vertical window size
" `=` symbol is easier to type rather that `+`
map <leader>=  :vertical res +1<CR>
map <leader>- :vertical res -1<CR>

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
" let vim_markdown_preview_browser='Google Chrome'


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
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_theme = 'tomorrow'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   nerd-tree                           "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" silent! nnoremap <F2> :NERDTreeToggle<CR>
" silent! nnoremap <F3> :NERDTreeFind<CR>
silent! map <leader>0 :NERDTreeToggle<CR>

let NERDTreeShowHidden=1
let g:NERDTreeIgnore = ['\.swp$', '\~$']
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

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              yaml plugin                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Source: https://www.vim.org/scripts/script.php?script_id=739
au BufNewFile,BufRead *.yaml,*.yml so ~/.vim/yaml.vim

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               vim submode                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"disable submode timeouts:
let g:submode_timeout = 0

" don't consume submode-leaving key
let g:submode_keep_leaving_key = 1

let g:submode_leave_with_key = 1

" map <leader>=  :vertical res +1<CR>
call submode#enter_with('grow/shrink', 'n', '', '<leader>=', ':vertical res +1<CR>')
call submode#map('grow/shrink', 'n', '', '=', ':vertical res +1<CR>')

" map <leader>- :vertical res -1<CR>
call submode#enter_with('grow/shrink', 'n', '', '<leader>-', ':vertical res -1<CR>')
call submode#map('grow/shrink', 'n', '', '-', ':vertical res -1<CR>')

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           LF -- terminal file manager                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:lf_map_keys = 0
map <leader>l :LfNewTab<CR>

" Opening `lf` instead of netrw when you open a directory
let g:NERDTreeHijackNetrw = 0 "" add this line if you use NERDTree
let g:lf_replace_netrw = 1 "" open lf when vim open a directory
