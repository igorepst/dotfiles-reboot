local treesitter = {}

treesitter.config = function()
    require('nvim-treesitter.configs').setup({
        ensure_installed = "maintained",
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

return treesitter