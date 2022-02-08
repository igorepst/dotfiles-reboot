local fzf_lua = require('fzf-lua')
fzf_lua.setup({ winopts = { preview = { layout = 'vertical' } } })

function FindFileInRuntime()
    fzf_lua.files({
        raw_cmd = 'fd --type file . '
            .. vim.fn.stdpath('config')
            .. ' '
            .. vim.fn.stdpath('data')
            .. ' '
            .. vim.fn.getenv('VIMRUNTIME'),
        prompt = 'FileInRuntime> ',
    })
end

function SearchInRuntime()
    fzf_lua.live_grep({
        filespec = '-- $(fd --type file . '
            .. vim.fn.stdpath('config')
            .. ' '
            .. vim.fn.stdpath('data')
            .. ' '
            .. vim.fn.getenv('VIMRUNTIME')
            .. ')',
        prompt = 'SearchInRuntime> ',
    })
end

local opts = { noremap = true, silent = true }
local map = vim.api.nvim_set_keymap

map('n', '<leader>ff', ':lua require("fzf-lua").files()<CR>', opts)
map('n', '<leader>fb', ':lua require("fzf-lua").buffers()<CR>', opts)
map('n', '<leader>ffr', ':lua FindFileInRuntime()<CR>', opts)
map('n', '<leader>fsr', ':lua SearchInRuntime()<CR>', opts)
