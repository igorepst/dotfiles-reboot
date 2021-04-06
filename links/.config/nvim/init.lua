require('ie.plugins')

local cmd = vim.cmd
local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

local function set_options(scope,options)
    for key, value in pairs(options) do
        scopes[scope][key] = value
        if scope ~= 'o' then scopes['o'][key] = value end
    end
end

cmd 'colorscheme PaperColor'
--opt('w', 'foldmethod', 'expr')
--opt('w', 'foldexpr', 'nvim_treesitter#foldexpr()')


local options_global = {
    background = 'light',
    mouse = 'a',
    termguicolors = true,
    shortmess = 'atToOIc',
    completeopt = 'menuone,noselect',
    smarttab = true,
    showmode = false,
    clipboard = 'unnamedplus',
}

local options_buffer = {
    expandtab = true,
    tabstop = 8,
    softtabstop = 4,
    shiftwidth = 4,
}

local options_window = {
}

set_options('o', options_global)
set_options('b', options_buffer)
set_options('w', options_window)
