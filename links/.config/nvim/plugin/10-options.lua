local opt = vim.opt
local g = vim.g

opt.title = true
opt.background = 'light'
opt.mouse = 'a'
opt.termguicolors = true
opt.completeopt = { 'menuone', 'noselect' }
opt.smarttab = true
opt.showmode = false
opt.ruler = false
opt.clipboard = 'unnamedplus'
opt.ignorecase = true
opt.smartcase = true
opt.wildignorecase = true
opt.wildignore = { '*.o', '*.obj', '*.hi' }
opt.hidden = true
opt.autoread = true
opt.splitbelow = true
opt.splitright = true
opt.timeoutlen = 500
opt.updatetime = 250
opt.scrolloff = 5
opt.sidescroll = 1
opt.sidescrolloff = 5
opt.linebreak = true
opt.expandtab = true
opt.tabstop = 8
opt.softtabstop = 4
opt.shiftwidth = 4
opt.relativenumber = true
opt.wildmode = { 'longest:full', 'full' }
opt.listchars = { tab = '> ', trail = '-', extends = '>', precedes = '<', nbsp = '+', eol = '$' }
opt.shortmess = 'atToOIc'
opt.whichwrap = 'b,s,<,>,[,]'
opt.inccommand = 'nosplit'
opt.foldenable = false

g.netrw_sort_options = "i"
g.netrw_keepdir = 0
g.netrw_hide = 0
g.netrw_banner = 0
g.netrw_browse_split = 4
g.netrw_liststyle = 3
g.netrw_sizestyle = "H"

local disabled_built_ins = {
    "tutor_mode_plugin",
    "remote_plugins",
}

for _, plugin in pairs(disabled_built_ins) do
    vim.g["loaded_" .. plugin] = 1
end
