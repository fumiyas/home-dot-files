if vim.fn.exists("g:Migemo") then
  vim.keymap.set("", "//", ":<C-u>Migemo<CR>")
end
