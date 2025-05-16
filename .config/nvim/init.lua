--vim.opt.directory:prepend("~/.local//vim//")
--vim.opt.undodir = "~/var/vim/undodir"
vim.opt.undofile = true
vim.opt.tags = {"./tags", "./TAGS", "tags", "TAGS;/"}

vim.opt.encoding = "UTF-8"
vim.opt.fileencodings = {"UCS-BOM", "UTF-8"}
vim.opt.ambiwidth = "double"
vim.opt.history = 10000
vim.opt.shortmess:append({ I = true })

vim.opt.wildmenu = true
vim.opt.wildmode = "longest:full"
vim.opt.wildignore = {"*.o", "*.a", "build", "__pycache__", "*.pyc"}
vim.opt.wildignorecase = true

vim.opt.list = false
vim.opt.listchars:append("tab:>-")
vim.opt.listchars:append("eol:$")
vim.opt.listchars:append("trail:-")
vim.opt.listchars:append("extends:~")
vim.opt.listchars:append("precedes:~")

vim.opt.completeopt = {"menuone", "preview"}

vim.opt.title = true
vim.opt.modeline = true
vim.opt.ruler = true
vim.opt.cursorline = true
vim.opt.cursorcolumn = true
vim.opt.colorcolumn = "+1"
vim.opt.showmode = true
vim.opt.showmatch = true
vim.opt.showcmd = true
vim.opt.scrolloff = 5
vim.opt.sidescroll = 1
vim.opt.sidescrolloff = 8

vim.opt.incsearch = true
vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.wrapscan = false

vim.opt.autoindent = true
vim.opt.cinoptions = ":0"
vim.opt.comments = ""
vim.opt.formatoptions = ""
vim.opt.backspace = {"indent", "eol", "start"}
vim.opt.nrformats:remove("octal")

vim.opt.textwidth = 80
vim.opt.expandtab = true
vim.opt.tabstop = 8
vim.opt.smarttab = true
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.shiftround = true

vim.opt.backup = false
vim.opt.endofline = false

require("init/lazy")

-- ======================================================================

vim.cmd.colorscheme(
  "wildcharm"
  --"elflord"
  --"koehler"
  --"pablo"
  --"torte"
  --"vim"
)

vim.api.nvim_set_hl(0, "CursorLine", { bold=true, bg="#000060" })
vim.api.nvim_set_hl(0, "CursorColumn", { bold=true, bg="#000060" })

-- ======================================================================

-- Copy to / Paste from clipboard (vim-fakeclip clone)
vim.keymap.set("", "sy", [["+y]], { desc="Copy to clipboard" })
vim.keymap.set("", "sd", [["+d]], { desc="Delete to clipboard" })
vim.keymap.set("n", "syy", [["+yy]], { desc="Copy line to clipboard" })
vim.keymap.set("n", "sdd", [["+dd]], { desc="Delete line to clipboard" })
vim.keymap.set("n", "sp", [["+p]], { desc="Paste from clipboard after the cursor" })
vim.keymap.set("n", "sP", [["+P]], { desc="Copy to clipboard before the cursor" })

vim.keymap.set("n", "sgf",
  [[<Cmd>let @+=expand('%')<CR>:echo 'Clipboard << ' . @+<CR>]],
  { desc="Copy file path to clipboard" }
)
vim.keymap.set("n", "sgF",
  [[<Cmd>let @+=expand('%:t:r')<CR>:echo 'Clipboard << ' . @+<CR>]],
  { desc="Copy file basename to clipboard" }
)

-- ======================================================================

vim.api.nvim_create_augroup('init', {})

-- Remove bogus history in the command-line window
vim.api.nvim_create_autocmd({ "CmdwinEnter" }, {
  group = "init",
  callback = function()
    vim.cmd([[: g/^qa\?!\?$/d]])
    vim.cmd([[: g/^wq\?a\?!\?$/d]])
    vim.cmd([[: g/^\(n\|rew\|sp\)$/d]])
  end
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = "init",
  pattern = { "po" },
  callback = function()
    vim.opt_local.path = "./../..,./../../..,,"
  end
})
vim.api.nvim_create_autocmd({ "FileType" }, {
  group = "init",
  pattern = { "spec" },
  callback = function()
    vim.opt_local.path = ".,./../SOURCES,,"
  end
})
vim.api.nvim_create_autocmd({ "FileType" }, {
  group = "init",
  pattern = { "go" },
  callback = function()
    vim.opt_local.expandtab = false
    vim.opt_local.shiftwidth = 8
  end
})

vim.api.nvim_create_autocmd({ "BufNewFile" }, {
  group = "init",
  pattern = { "*.bash" },
  command = [[0r ~/.vim/template/template.bash]],
})
vim.api.nvim_create_autocmd({ "BufNewFile" }, {
  group = "init",
  pattern = { "*.py" },
  command = [[0r ~/.vim/template/template.py]],
})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = "init",
  callback = function()
    vim.highlight.on_yank({ timeout = 300 })
  end,
})
