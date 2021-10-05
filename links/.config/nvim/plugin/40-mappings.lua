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
map('n', '<C-_>', '<Plug>(IgToggleComment)', { noremap = false })
map('v', '<C-_>', '<Plug>(IgToggleComment)', { noremap = false })
map('i', '<C-_>', '<Plug>(IgToggleComment)', { noremap = false })

map('c', 'ee<Space>', '(getcmdtype() == \':\' && getcmdline() == \'\')? \'edit **/*\' : \'ee<Space>\'', {
    expr = true,
})

map('n', '[b', ':<C-U>exe v:count1 . "bprevious!"<CR>', { silent = true })
map('n', ']b', ':<C-U>exe v:count1 . "bnext!"<CR>', { silent = true })
map('n', '[q', ':<C-U>exe v:count1 . "cprevious!"<CR>', { silent = true })
map('n', ']q', ':<C-U>exe v:count1 . "cnext!"<CR>', { silent = true })

map('n', '<F8>', ':lua ShowCurrentWordHighlight()<CR>', { silent = true })
