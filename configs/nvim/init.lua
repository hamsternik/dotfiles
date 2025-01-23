-- TODO: check out mitchell' nvim dotfiles
-- https://github.com/mitchellh/dotfiles/blob/main/nvim/init.vim

-- require("core.keymaps")
-- require("core.plugins")
-- require("plugin_config")

vim.g.mapleader = " "
vim.g.maplocallleader = " "

-- local map = vim.keymap.set

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

vim.api.nvim_set_keymap('n', '<Leader>s', ':w<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>e', ':Rex<CR>', { noremap = true, silent = true })

vim.keymap.set('n', '<leader>h', ':nohlsearch<CR>')

-- Ensure Neovim is Clipboard-Enabled
-- One-Time Use: "+y or "*y
-- Make It Automatic: set clipboard=unnamedplus
-- Using pbcopy Directly (Alternative)
vim.keymap.set('x', '<leader>y', '"+y')
