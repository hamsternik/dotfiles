local o = vim.opt

-- Line numbers
o.number = true
--o.relativenumber = true

-- tabs
o.shiftwidth = 4
o.tabstop = 4

-- clipoboard
vim.schedule(function()
  o.clipboard = "unnamedplus"
  o.undofile = true
end)

-- status column 
-- Set sign column to be 4.
-- So, if I have a new diff, It'll not feel weird, by the signcolumn expanding

--o.signcolumn = "yes:1"

-- startup line
o.cmdheight = 0

-- folds
o.foldcolumn = "1"
o.foldlevel = 99
o.foldlevelstart = 99
o.foldenable = true

-- scoll off 
--o.scrolloff = 15

-- search
o.ignorecase = true
o.smartcase = true
