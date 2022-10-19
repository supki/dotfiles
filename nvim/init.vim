let g:seoul256_background = 234
colorscheme seoul256
highlight Pmenu ctermfg=252 ctermbg=235
highlight PmenuSel cterm=bold ctermfg=252 ctermbg=237
highlight StatusLine cterm=none ctermfg=252 ctermbg=235
highlight WildMenu cterm=bold ctermfg=252 ctermbg=237
highlight Comment ctermfg=246

set number
set relativenumber

set cursorline
set cursorcolumn

let g:airline_theme = 'zenburn'
let g:airline#extensions#tabline#enabled = 0
let g:rainbow_active = 1

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
nnoremap <tab> %

nnoremap : ;
nnoremap ; :
vnoremap ; :
vnoremap : ;

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'

let mapleader = " "

nnoremap <leader>e :GitFiles<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <silent> <leader>h :nohlsearch<CR>

set hidden

set wildmenu
set wildmode=list:full

let $FZF_DEFAULT_OPTS = '--reverse'

match DiffDelete /\s\+$/
