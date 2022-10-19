vim.g.seoul256_background = 234
vim.cmd([[
colorscheme seoul256
highlight Pmenu ctermfg=252 ctermbg=235
highlight PmenuSel cterm=bold ctermfg=252 ctermbg=237
highlight StatusLine cterm=none ctermfg=252 ctermbg=235
highlight WildMenu cterm=bold ctermfg=252 ctermbg=237
highlight Comment ctermfg=246
]])

vim.opt.number = true
vim.opt.numberwidth = 3
vim.opt.relativenumber = true

vim.opt.cursorline = true
vim.opt.cursorcolumn = true

vim.g.airline_theme = 'zenburn'
vim.cmd([[
let g:airline#extensions#tabline#enabled = 0
]])
vim.g.rainbow_active = 1

vim.opt.tabstop = 2
vim.opt.softtabstop = 0
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.smarttab = true
vim.opt.linebreak = true
vim.opt.smartindent = true
vim.opt.virtualedit = 'all'

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.timeout = false
vim.opt.ttimeout = true

vim.g.mapleader = " "

vim.cmd([[
nnoremap Q <nop>
nnoremap q <nop>
nnoremap <tab> %

nnoremap : ;
nnoremap ; :
vnoremap ; :
vnoremap : ;

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'

nnoremap <leader>e :GitFiles<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <silent> <leader>h :nohlsearch<CR>
]])

vim.opt.hidden = true

vim.opt.wildmenu = true
vim.opt.wildmode = 'list:full'

vim.cmd("let $FZF_DEFAULT_OPTS = '--reverse'")

vim.cmd('match DiffDelete /\\s\\+$/')

require('nvim-treesitter.configs').setup {
  auto_install = false,
  highlight = {
    enable = { "nix", "haskell" },
    additional_vim_regex_highlighting = false,
  },
}
