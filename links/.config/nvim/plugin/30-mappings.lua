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

map('n', '[b', ':<C-U>exe v:count1 . "bprevious!"<CR>', {silent = true})
map('n', ']b', ':<C-U>exe v:count1 . "bnext!"<CR>', {silent = true})
map('n', '[q', ':<C-U>exe v:count1 . "cprevious!"<CR>', {silent = true})
map('n', ']q', ':<C-U>exe v:count1 . "cnext!"<CR>', {silent = true})

