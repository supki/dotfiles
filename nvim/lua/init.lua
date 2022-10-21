vim.g.seoul256_background = 234
vim.cmd([[
colorscheme seoul256
highlight Pmenu ctermfg=252 ctermbg=235
highlight PmenuSel cterm=bold ctermfg=252 ctermbg=237
highlight StatusLine cterm=none ctermfg=252 ctermbg=235
highlight WildMenu cterm=bold ctermfg=252 ctermbg=237
highlight Comment ctermfg=246
highlight GitSignsAddNr ctermfg=40 cterm=bold
highlight GitSignsChangeNr ctermfg=45 cterm=bold
highlight GitSignsDeleteNr ctermfg=211 cterm=bold
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

vim.keymap.set('n', 'Q', "<nop>", {remap = false})
vim.keymap.set('n', 'q', "<nop>", {remap = false})

vim.keymap.set('n', '<tab>', "%", {remap = false})

vim.keymap.set('n', ';', ":", {remap = false})
vim.keymap.set('v', ':', ";", {remap = false})
vim.keymap.set('v', ';', ":", {remap = false})
vim.keymap.set('v', ':', ";", {remap = false})

vim.keymap.set(
  'n',
  'j',
  function() return (vim.v.count > 0) and 'j' or 'gj' end,
  {expr = true, remap = false}
)
vim.keymap.set(
  'n',
  'k',
  function() return (vim.v.count > 0) and 'k' or 'gk' end,
  {expr = true, remap = false}
)

vim.keymap.set('n', '<leader>e', ":GitFiles<CR>", {remap = false})
vim.keymap.set('n', '<leader>b', ":Buffers<CR>", {remap = false})
vim.keymap.set('n', '<leader>h', ":nohlsearch<CR>", {remap = false, silent = true})

vim.opt.hidden = true

vim.opt.wildmenu = true
vim.opt.wildmode = 'list:full'

vim.cmd('match DiffDelete /\\s\\+$/')

require('nvim-treesitter.configs').setup {
  auto_install = false,
  highlight = {
    enable = {
      "haskell",
      "lua",
      "nix",
    },
    additional_vim_regex_highlighting = false,
  },
}

require('gitsigns').setup {
  signcolumn = false,
  numhl = true,
  on_attach = function(bufnr)
    local gitsigns = package.loaded.gitsigns
    vim.keymap.set('n', '<leader>gr', gitsigns.reset_hunk, {buffer = bufnr})
    vim.keymap.set('n', '<leader>gb', gitsigns.blame_line, {buffer = bufnr})
  end,
}
