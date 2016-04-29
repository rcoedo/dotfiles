" Use Fish as shell
if &shell =~# 'fish$'
    set shell=sh
endif

" Space is my leader
let mapleader="\<Space>"

set nocompatible
filetype off

call plug#begin('~/.config/nvim/plugged')

" Plugin configuration
Plug 'dag/vim-fish'
Plug 'maxbrunsfeld/vim-emacs-bindings'
Plug 'airblade/vim-gitgutter'
Plug 'terryma/vim-expand-region'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-ragtag'
Plug 'morhetz/gruvbox'
Plug 'pangloss/vim-javascript'
Plug 'tpope/vim-speeddating'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'mxw/vim-jsx'
Plug 'elixir-lang/vim-elixir'

Plug 'tpope/vim-fugitive'
Plug 'itchyny/lightline.vim'
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified' ] ],
      \   'right': [ [ 'lineinfo', 'percent' ],
      \             [ 'filetype' ] ]
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"⭤":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'component_visible_condition': {
      \   'readonly': '(&filetype!="help"&& &readonly)',
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
      \ }
      \ }

call plug#end()

" General configuration
filetype plugin indent on
set history=1000
set autoread
set number
set ruler
set cursorline
set so=7
set wildmenu
set wildmode=list:longest
set wildignore=*.o,*~,*.pyc
set wildignore+=*/node_modules/**
set wildignore+=*/bower_components/**
set backspace=eol,start,indent
set ignorecase
set smartcase
set hlsearch
set incsearch
set lazyredraw
set magic
set showmatch
set mat=2
set noerrorbells
set novisualbell
set t_vb=
set tm=500
set list
set listchars=tab:⋅›,trail:⋅,nbsp:⋅
set ffs=unix,dos,mac
set nobackup
set nowb
set noswapfile
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set lbr
set tw=120
set ai
set si
set wrap
set laststatus=2
set ttyfast
set mouse=a
set clipboard=unnamed
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

" Colors
colorscheme gruvbox
set background=dark
syntax enable
set t_Co=256
highlight Comment cterm=italic

" Key mappings
nnoremap <leader>ev :tabedit $MYVIMRC<cr>
nnoremap <leader>vc :source $MYVIMRC<cr>

noremap <leader>bd :bd<cr>
noremap <leader>ft :NERDTreeToggle<cr>
noremap <leader>fi :NERDTreeFind<cr>

nmap <leader>sh :noh<cr>
nmap <tab> <c-w><c-w>

nmap <c-p> :Files<cr>
nmap <c-b> :Buffers<cr>

" File config
au FileType gitcommit set tw=72
au FileType javascript setlocal ts=2 sw=2 expandtab
au FileType go setlocal ts=4 sw=4 expandtab
au FileType lua setlocal ts=2 sw=2 expandtab
autocmd FileType html :setlocal sw=2 ts=2 sts=2
autocmd BufNewFile,BufReadPost *.coffee setl shiftwidth=2 expandtab
autocmd BufWrite *.lua :call DeleteTrailing()
autocmd BufWrite *.py :call DeleteTrailing()
autocmd BufWrite *.coffee :call DeleteTrailing()
autocmd BufWrite *.java :call DeleteTrailing()

" Delete trailing whitespaces
func! DeleteTrailing()
   exe "normal mz"
   %s/\s\+$//ge
   exe "normal `z"
endfunc

