if vim.fn.exists("g:Migemo") then
  vim.keymap.set("", "//", ":<C-u>Migemo<CR>")
end

local ok, plugin = pcall(require, "nvim-treesitter.configs")
if ok then
  plugin.setup({
    highlight = {
      enable = true,
      disable = {
      },
    },
    indent = {
      enable = true,
    },
  })
end
