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

if vim.fn.exists(":Migemo") > 0 then
  vim.keymap.set("", "//", ":<C-u>Migemo<CR>")
end
