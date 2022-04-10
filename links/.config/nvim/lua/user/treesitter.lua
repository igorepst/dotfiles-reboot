local treesitter = {}

treesitter.config = function()
    require('nvim-treesitter.configs').setup({
        ensure_installed = 'all',
        highlight = {
            enable = true,
        },
        incremental_selection = {
            enable = true,
            keymaps = {
                init_selection = '<Leader>n',
                node_incremental = 'n',
                scope_incremental = '<Leader>m',
                node_decremental = 'm',
            },
        },
        indent = {
            enable = true,
        },
    })
end

vim.opt.foldmethod='expr'
vim.opt.foldexpr='nvim_treesitter#foldexpr()'

return treesitter
