vim.cmd([[
colorscheme nordfox
highlight Normal guibg=#1c1c1c
highlight NormalNC guibg=#1c1c1c
highlight CursorLine guibg=#2c2c27
highlight String guifg=#bea38c
highlight GitSignsAddNr guifg=#00d700 gui=bold
highlight GitSignsChangeNr guifg=#00d7ff gui=bold
highlight GitSignsDeleteNr guifg=#ff87af gui=bold
]])

vim.opt.number = true
vim.opt.numberwidth = 3
vim.opt.relativenumber = true

vim.opt.cursorline = true
vim.opt.cursorcolumn = true

vim.g.airline_theme = 'zenburn'
vim.g['airline#extensions#tabline#enabled'] = 0
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

function autosave()
  local current_buffer = vim.api.nvim_get_current_buf()
  local buffer_name = vim.api.nvim_buf_get_name(current_buffer)
  local buftype = vim.api.nvim_buf_get_option(current_buffer, 'buftype')

  -- We only try to save those buffers that:
  --   - are file buffers (i.e. have an associated file)
  --   - do not have &buftype set
  -- Otherwise, vim becomes annoying and complains a lot.
  if buffer_name ~= '' and buftype == '' then
    vim.cmd('w')
  end
end

vim.cmd([[
  autocmd FocusLost,BufLeave,BufWinLeave * :lua autosave()
]])

require('nvim-treesitter.configs').setup {
  auto_install = false,
  highlight = {
    enable = {
      "haskell",
      "json",
      "lua",
      "nix",
      "ruby",
      "yaml",
    },
    additional_vim_regex_highlighting = false,
  },
  rainbow = {
    enable = true,
    extended_mode = false,
  },
}

require('gitsigns').setup {
  signcolumn = false,
  numhl = true,
  on_attach = function(bufnr)
    local gitsigns = package.loaded.gitsigns
    vim.keymap.set('n', '<leader>gr', gitsigns.reset_hunk, {buffer = bufnr})
    vim.keymap.set('v', '<leader>gr', function() gitsigns.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end, {buffer = bufnr})
    vim.keymap.set('n', '<leader>gs', gitsigns.stage_hunk, {buffer = bufnr})
    vim.keymap.set('v', '<leader>gs', function() gitsigns.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, {buffer = bufnr})
    vim.keymap.set('n', '<leader>gb', gitsigns.blame_line, {buffer = bufnr})
  end,
}

local lsp_status = require('lsp-status')
lsp_status.register_progress()

require('lualine').setup {
  options = {
    section_separators = '',
    component_separators = '|',
  },
  sections = {
    lualine_c = {
      "require('lsp-status').status()"
    },
    lualine_x = {
      'encoding',
      'filetype',
    },
    lualine_y = {
      'filename'
    }
  }
}

vim.g.haskell_tools = {
  hls = {
    on_attach = lsp_status.on_attach,
    settings = {
      haskell = {
        plugin = {
          eval = {
            globalOn = false,
          },
          hlint = {
            globalOn = false,
          },
        },
      },
    },
  },
}
