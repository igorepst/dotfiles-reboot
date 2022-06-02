local o = vim.opt
local g = vim.g

o.title = true
o.background = 'light'
o.mouse = 'a'
o.termguicolors = true
o.completeopt = { 'menu', 'noselect' }
o.smarttab = true
o.showmode = false
o.ruler = false
o.clipboard = 'unnamedplus'
o.ignorecase = true
o.smartcase = true
o.wildignorecase = true
o.wildignore = { '*.o', '*.obj', '*.hi' }
o.hidden = true
o.autoread = true
o.splitbelow = true
o.splitright = true
o.timeoutlen = 500
o.updatetime = 250
o.scrolloff = 5
o.sidescroll = 1
o.sidescrolloff = 5
o.linebreak = true
o.expandtab = true
o.tabstop = 4
o.softtabstop = 4
o.shiftwidth = 4
o.number = true
o.pumheight = 10
o.wildmode = { 'longest:full', 'full' }
o.listchars = { tab = '> ', trail = '-', extends = '>', precedes = '<', nbsp = '+', eol = '$', space = '.' }
o.shortmess = 'atToOIcFs'
o.whichwrap = 'b,s,<,>,[,]'
o.inccommand = 'nosplit'
o.foldlevel=99
o.path = '.,,'
o.signcolumn = 'yes'
o.showtabline = 2
o.autoindent = true
o.smartindent = true
o.swapfile = false
o.writebackup = false
o.undofile = true
o.cursorline = true
o.iskeyword:append('-')

g.netrw_sort_options = 'i'
g.netrw_keepdir = 0
g.netrw_hide = 0
g.netrw_banner = 0
g.netrw_browse_split = 4
g.netrw_liststyle = 3
g.netrw_sizestyle = 'H'

g.loaded_tutor_mode_plugin = 1
