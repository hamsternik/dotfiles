-- TODO: check out mitchell' nvim dotfiles
-- https://github.com/mitchellh/dotfiles/blob/main/nvim/init.vim

-- require("core.keymaps")
-- require("core.plugins")
-- require("plugin_config")

-- Set <space> as the leader key.
-- See `:help mapleader`. NOTE: Must happen before plugins are loaded (otherwise wrong leader will be used).
vim.g.mapleader = " "
vim.g.maplocallleader = " "

-- [[ local map = vim.keymap.set ]]

-- Make line number default.
vim.opt.number = true
-- Add relative line numbers to help with jumping.
vim.opt.relativenumber = true

-- Enable mouse mode, can be useful eg. for resizing splits.
vim.opt.mouse = 'a'

-- Sync clipboard between OS and Neovim.
-- NOTE: Remove this option if you want your OS clipboad to remain independent.
-- vim.opt.clipboard = 'unnamedplus'

-- false: Do not show the mode, since it's already in the status line.
vim.opt.showmode = true

-- Enable break indent
-- vim.opt.breakindent = true

-- Save undo history
vim.opt.undofile = true

-- Case-insinsitive searching UNLESS \C or one or more capital letters in the search term.
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- The option controls the display of the sign column in NeoVim.
-- The sign column is the dedicated column on the left side of the editor, where various plugins
-- like Git signs, diagnostic markers, breakpoints, etc. display their icons or symbols.
-- Setting it to "yes" ensures that the sign column is always shown.
vim.opt.signcolumn = 'yes'

-- Enables live preview of substitutions in a split window.
-- Alternatives are: `nosplit` (enables live preview w/o opening a split).
vim.opt.inccommand = 'split'

-- Show which line your cursor is on:
vim.opt.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 10

vim.opt.showcmd = true
vim.opt.backspace = '2'
vim.opt.laststatus = 2
vim.opt.autowrite = true
vim.opt.cursorline = true
vim.opt.autoread = true

-- use spaces for tabs and whatnot
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.shiftround = true
vim.opt.expandtab = true

-- [[ Basic Keymaps ]]
-- See `:help vim.keymap.set()`

vim.keymap.set('n', '<ESC>', '<cmd>nohlsearch<CR>')

-- Ensure Neovim is Clipboard-Enabled. One-Time use: `"+y` or `"*y`
vim.keymap.set('x', '<leader>y', '"+y')

vim.api.nvim_set_keymap('n', '<Leader>s', ':w<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>e', ':Rex<CR>', { noremap = true, silent = true })

-- See `:help wincmd` for a list of all window commands.
vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>}', { desc = 'Move focus to the bottom window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the top window' })

vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, {})

-- [[ Install `lazy.nvim` plugin manager ]]
-- See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    error('Error cloning https://github.com/folke/lazy.nvim\n' .. out)
    os.exit(1)
  end
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

-- [[ Configure and install plugins ]]
--
-- To check the current status of your plugins, run
--    :Lazy
--
-- You can press `?` in this menu for help. Use `:q` to close the window.
--
-- To update plugins you can run
--    :Lazy update
--
-- NOTE: Here is where you install plugins.
require("lazy").setup({
  -- add plugins here, inside the `spec` table
  spec = {},

  -- NOTE: Plugins can be added with a link or for a github repo: 'owner/repo' link.
  -- Note: Plugins can also be added by using a table,
  -- with the first arg being the link and the following
  -- keys can be used to configure plugin behavior/loading/etc.
  --
  -- Use `opts = {}` to force a plugin to be loaded.
  --
  -- Configure any settings here. See the documnetation for more details.
  -- colorscheme that will be used when installing plugins.
  -- install = { colorscheme = { "habamax" } },
  -- automatically check for plugin updates
    checker = { enabled = true },
})
