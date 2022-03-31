let g:seoul256_background = 234
colorscheme seoul256

set number
set relativenumber

set cursorline
set cursorcolumn

let g:airline_theme = 'zenburn'

set tabstop=2
set softtabstop=0
set expandtab
set shiftwidth=2
set smarttab
set linebreak
set smartindent
set virtualedit=all

set ignorecase
set smartcase

set notimeout
set ttimeout

nnoremap Q <nop>
nnoremap q <nop>

nnoremap : ;
nnoremap ; :
vnoremap ; :
vnoremap : ;

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'

let mapleader = " "

nnoremap <leader>e :Files<CR>
nnoremap <leader>b :Buffers<CR>

set hidden

set wildmenu
set wildmode=list,full

let $FZF_DEFAULT_OPTS = '--reverse'
