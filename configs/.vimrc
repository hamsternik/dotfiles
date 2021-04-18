
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

" A dark Vim/Neovim color scheme inspired by Atoms's syntax theme.
Plugin 'joshdick/onedark.vim'

" A light and configurable statusline/tabline plugin for Vim
Plugin 'itchyny/lightline.vim'

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
Plugin 'tpope/vim-fugitive'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'JamshedVesuna/vim-markdown-preview'
Plugin 'lervag/vimtex'
Plugin 'luochen1990/rainbow'
Plugin 'lepture/vim-velocity'
Plugin 'danro/rename.vim'
Plugin 'rbgrouleff/bclose.vim'
Plugin 'ptzz/lf.vim'
Plugin 'editorconfig/editorconfig-vim'

call vundle#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               Color Scheme                            "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

syntax enable
set t_Co=256

" Source: https://github.com/joshdick/dotfiles/blob/main/vim.symlink/config/settings/colorscheme.vim#L11
"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
if (has("termguicolors"))
  if !empty($TMUX)
    "Set Vim-specific sequences for RGB colors; only seems to be needed for Vim 8 running inside tmux with $TERM=tmux
    "Found at < https://github.com/vim/vim/issues/993#issuecomment-255651605 >
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  endif
  set termguicolors
endif

"onedark setup {{{
"source: https://github.com/joshdick/dotfiles/blob/main/vim.symlink/config/settings/colorscheme.vim#L23
"onedark.vim override: Don't set a background color when running in a terminal;
"just use the terminal's background color
if (has("autocmd") && !has("gui_running"))
  augroup colors
    autocmd!
    let s:white = { "gui": "#ABB2BF", "cterm": "145", "cterm16": "7"}
    autocmd ColorScheme * call onedark#set_highlight("Normal", { "fg": s:white }) "No `bg` setting
  augroup END
endif
colorscheme onedark
"}}}

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

set cursorline

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                               Keybindings                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enter (make new line and get into `insert` mode)
nnoremap <Enter> o<Esc>

" Ctrl+s (fast saving)
nnoremap <silent> <C-s> <esc>:w<CR>
inoremap <silent> <C-s> <esc>:w<CR>

" leader+s/+v (Horizontal/Vertical window split)
nnoremap <leader>s :split<CR>
nnoremap <leader>v :vsplit<CR>

" Create / Navigate through Tabs
nnoremap <C-t> :tabnew<CR>
nnoremap <C-]> :tabnext<CR>
"nnoremap <C-[> :tabprevious<CR>

inoremap <C-t> <Esc>:tabnew<CR>
inoremap <C-]> <Esc>:tabnext<CR>i
"inoremap <C-[> <Esc>:tabprevious<CR>i

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
"                               lightline                               "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set laststatus=2
set noshowmode

let g:lightline = {
    \ 'colorscheme': 'onedark',
    \ 'separator': { 'left': '/', 'right': '\' },
    \ 'subseparator': { 'left': '', 'right': '' },
    \ 'tabline': { 'left': [ [ 'buffers'] ], 'right': [ [ 'tabs' ] ] },
    \ 'component_expand': { 'buffers': 'lightline#bufferline#buffers' },
    \ 'component_type': { 'buffers': 'tabsel' },
    \ 'component_raw': { 'buffers': 1 }
    \ }
let g:lightline#bufferline#clickable = 1

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
"                                   nerd-tree                           "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" silent! nnoremap <F2> :NERDTreeToggle<CR>
" silent! nnoremap <F3> :NERDTreeFind<CR>
silent! map <leader>0 :NERDTreeToggle<CR>

let NERDTreeShowHidden=1
let g:NERDTreeIgnore = ['\.swp$', '\~$']

" Start NERDTree automatically
" autocmd VimEnter * NERDTree

" Jump to the main window
" autocmd VimEnter * wincmd p

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
" call submode#enter_with('grow/shrink', 'n', '', '<leader>=', ':vertical res +1<CR>')
" call submode#map('grow/shrink', 'n', '', '=', ':vertical res +1<CR>')

" map <leader>- :vertical res -1<CR>
" call submode#enter_with('grow/shrink', 'n', '', '<leader>-', ':vertical res -1<CR>')
" call submode#map('grow/shrink', 'n', '', '-', ':vertical res -1<CR>')

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           LF -- terminal file manager                 "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:lf_map_keys = 0
map <leader>l :LfNewTab<CR>

" Opening `lf` instead of netrw when you open a directory
let g:NERDTreeHijackNetrw = 0 "" add this line if you use NERDTree
let g:lf_replace_netrw = 1 "" open lf when vim open a directory
