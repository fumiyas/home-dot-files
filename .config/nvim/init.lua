vim.opt.tags = {"./tags", "./TAGS", "tags", "TAGS;/"}
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
vim.opt.wildignore = {".o", "*.a", "build", "__pycache__", "*.pyc"}
vim.opt.wildignorecase = true

vim.opt.list = false
vim.opt.listchars:append("tab:>-")
vim.opt.listchars:append("eol:$")
vim.opt.listchars:append("trail:-")
vim.opt.listchars:append("extends:~")
vim.opt.listchars:append("precedes:~")

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
vim.opt.clipboard = "unnamedplus"

vim.opt.textwidth = 0
vim.opt.expandtab = true
vim.opt.tabstop = 8
vim.opt.smarttab = true
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.shiftround = true

vim.opt.backup = false
vim.opt.endofline = false

require("init/lazy")
require("init/plugins")
