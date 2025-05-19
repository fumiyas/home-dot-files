local mod = {}

mod.treesitter_options = function()
  return {
    highlight = {
      enable = true,
      disable = {
      },
    },
    indent = {
      enable = true,
    },
  }
end

mod.lualine_options = function()
  return {
    options = {
      icons_enabled = false,
      component_separators = { left = '|', right = '|'},
      section_separators = { left = '|', right = '|'},
    }
  }
end

mod.telescope_options = function()
  return {
    extensions = {
      file_brower = {},
    }
  }
end
mod.telescope_keys = function()
  return {
    { "<Space>ff", ":Telescope find_files<CR>" },
    { "<Space>fg", ":Telescope live_grep<CR>" },
    { "<space>fb", ":Telescope file_browser path=%:p:h select_buffer=true<CR>" },
  }
end

--[[
      vim.keymap.set('n', '<C-f><C-f>', builtin.find_files, { desc = '[F]ind [F]iles' })
      vim.keymap.set('n', '<C-f><C-g>', builtin.live_grep, { desc = '[S]earch by [G]rep' })
      vim.keymap.set('n', '<C-f><C-b>', builtin.buffers, { desc = '[ ] Find existing buffers' })
--]]

--autocmd BufNewFile,BufRead *.csv   set filetype=csv
--[[
vim.api.nvim_create_autocmd({ "BufNew", "BufRead", "BufEnter" }, {
  group = "init",
  pattern = { "*.csv" },
  command = "setlocal FIXME",
})
--]]

mod.ibl_options = function()
  local highlight = {
    "CursorColumn",
    "Whitespace",
  }
  return {
    indent = {
      highlight = highlight,
      char = "",
    },
    whitespace = {
      highlight = highlight,
      remove_blankline_trail = false,
    },
  }
end

mod.migemo_keys = function()
  return {
    { "//", ":<C-u>Migemo<CR>" },
  }
end

return mod
