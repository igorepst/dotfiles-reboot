local opts = { noremap = true, silent = true }
local map = vim.api.nvim_set_keymap

map('', '<Space>', '<Nop>', opts)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

--  Actually 'C-/' and not 'C-_'
map('n', '<C-_>', '<Plug>(IgToggleComment)', { noremap = false })
map('v', '<C-_>', '<Plug>(IgToggleComment)', { noremap = false })
map('i', '<C-_>', '<Plug>(IgToggleComment)', { noremap = false })

map('c', 'ee<Space>', '(getcmdtype() == \':\' && getcmdline() == \'\')? \'edit **/*\' : \'ee<Space>\'', {
    expr = true,
})

map('n', '[b', ':<C-U>exe v:count1 . "bprevious!"<CR>', opts)
map('n', ']b', ':<C-U>exe v:count1 . "bnext!"<CR>', opts)
map('n', '[q', ':<C-U>exe v:count1 . "cprevious!"<CR>', opts)
map('n', ']q', ':<C-U>exe v:count1 . "cnext!"<CR>', opts)

map('n', '<F8>', ':lua ShowCurrentWordHighlight()<CR>', opts)

map("n", "<C-h>", "<C-w>h", opts)
map("n", "<C-j>", "<C-w>j", opts)
map("n", "<C-k>", "<C-w>k", opts)
map("n", "<C-l>", "<C-w>l", opts)

-- Resize with arrows
map("n", "<C-Up>", ":resize -2<CR>", opts)
map("n", "<C-Down>", ":resize +2<CR>", opts)
map("n", "<C-Left>", ":vertical resize -2<CR>", opts)
map("n", "<C-Right>", ":vertical resize +2<CR>", opts)

map("i", "jk", "<ESC>", opts)
map("v", "p", '"_dP', opts)

-- Stay in indent mode
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

-- Move text up and down
map("i", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
map("i", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)
map("n", "<A-j>", ":m .+1<CR>==", opts)
map("n", "<A-k>", ":m .-2<CR>==", opts)
map("v", "<A-j>", ":move '>+1<CR>gv=gv", opts)
map("v", "<A-k>", ":move '<-2<CR>gv=gv", opts)
