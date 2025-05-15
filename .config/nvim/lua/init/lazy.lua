local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local init_plugins = require "init.plugins"

require("lazy").setup {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    opts = init_plugins.treesitter_options,
  },
  {
    "nvim-lualine/lualine.nvim",
    opts = init_plugins.lualine_options,
  },
  {
    "nvim-telescope/telescope.nvim",
--[[
    dependencies = {
      "nvim-lua/plenary.nvim",
      lazy = true,
    },
--]]
    cmd = { "Telescope" },
    opts = init_plugins.telescope_options,
    keys = init_plugins.telescope_keys,
    config = function()
      require("telescope").load_extension("file_browser")
    end
  },
  {
    "nvim-lua/plenary.nvim",
    lazy = true,
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    lazy = true,
  },
  {
    "haya14busa/vim-migemo",
    cmd = { "Migemo" },
    keys = init_plugins.migemo_keys,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    opts = init_plugins.ibl_options,
  },
  {
    "mechatroner/rainbow_csv",
    ft = { "csv", "tsv" },
  },
}
