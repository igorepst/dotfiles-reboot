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

local opts = { silent = true }
local map = vim.keymap.set

map('n', '<leader>ff', fzf_lua.files, opts)
map('n', '<leader>fb', fzf_lua.buffers, opts)
map('n', '<leader>ffr', FindFileInRuntime, opts)
map('n', '<leader>fsr', SearchInRuntime, opts)
