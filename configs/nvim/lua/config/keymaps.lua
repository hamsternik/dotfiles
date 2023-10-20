local map = vim.keymap.set

-- better up/down
map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

if not vim.g.vscode then
  map("n", "<leader>tt", function()
    require("utils.float_term")(nil, { ctrl_hjkl = false })
  end, { desc = "Open the terminal" })
  map("n", "<leader>gg", function()
    require("utils.float_term")("lazygit", { ctrl_hjkl = false })
  end, { desc = "Open LazyGit" })
end

map("n", "dd", function()
  if vim.fn.getline(".") == "" then
    return '"_dd'
  end

  return "dd"
end, { expr = true })
