vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

local function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend('force', options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

--  Actually 'C-/' and not 'C-_'
map('n', '<C-_>', ':lua ToggleComment("n")<CR>')
map('v', '<C-_>', ':lua ToggleComment("v")<CR>')
map('i', '<C-_>', '<Esc>:lua ToggleComment("i")<CR>i')

map('c', 'ee<Space>', "(getcmdtype() == ':' && getcmdline() == '')? 'edit **/*' : 'ee<Space>'", {expr = true})

