vim.cmd([[
    augroup packComp
    autocmd!
    autocmd BufWritePost 20-pluginList.lua source <afile> | PackerCompile
    augroup END
]])

return require('packer').startup({
    function(use)
        use({ 'wbthomason/packer.nvim' })
        use({
            'igorepst/igTermColors.nvim',
            config = function()
                local color_overrides = nil
                local f = loadfile(vim.fn.expand('~/.theme/nvimThemeColors.lua'))
                if f then
                    color_overrides = f()
                end
                require('igTermColors').setup({ color_overrides = color_overrides, invert_for_dark = true })
                vim.cmd([[colorscheme igTermColors]])
            end,
        })
        use({ 'igorepst/igToggleComment.nvim', keys = { '<Plug>(IgToggleComment)' } })
        use({
            'igorepst/igCommands.nvim',
            config = function()
                require('igCommands').setup({
                    prefix = 'ig',
                    commands = {
                        {
                            name = 'Toggle list',
                            cmd = 'lua vim.opt.list = not vim.opt.list:get()',
                            key = '<Leader>tl',
                        },
                        { name = 'Toggle wrap', cmd = 'lua vim.opt.wrap = not vim.opt.wrap:get()' },
                        { name = 'Toggle hlsearch', cmd = 'lua vim.opt.hlsearch = not vim.opt.hlsearch:get()' },
                        { name = 'Convert to DOS', cmd = 'set ff=dos' },
                        { name = 'Convert to Unix', cmd = 'set ff=unix' },
                        { name = 'Show DOS line endings', cmd = 'edit ++ff=unix' },
                        { name = 'Convert to UTF-8', cmd = 'set fileencoding=utf8' },
                        { name = 'Format JSON', cmd = '%!jq -R \'. as $line | try fromjson catch $line\'' },
                        { name = 'Trim trailing space', cmd = 'execute \':%s/\\s\\+$//e\'' },
                        { name = 'Reindent file', cmd = 'execute \'normal! mzgg=G`z\' | IgTrimTrailingSpace' },
                        {
                            name = 'Diff saved',
                            cmd = 'vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis',
                        },
                        { name = 'Find file in runtime', cmd = 'lua FindFileInRuntime()' },
                        { name = 'Search in runtime', cmd = 'lua SearchInRuntime()' },
                    },
                })
            end,
        })
        use({
            'ibhagwan/fzf-lua',
            requires = {
                'vijaymarupudi/nvim-fzf',
                'kyazdani42/nvim-web-devicons',
            },
        })
        use({
            'nvim-treesitter/nvim-treesitter',
            run = ':TSUpdate',
            config = function()
                require('treesitter').config()
            end,
        })
        use({
            'famiu/feline.nvim',
            requires = { 'kyazdani42/nvim-web-devicons', opt = true },
            config = require('theme').statusline,
        })
        use({
            'neovim/nvim-lspconfig',
            config = function()
                require('lsp')
            end,
        })
        use('hrsh7th/nvim-cmp')
        use('hrsh7th/cmp-nvim-lsp')
        use({
            'lewis6991/gitsigns.nvim',
            requires = { 'nvim-lua/plenary.nvim' },
            config = function()
                require('gitsigns').setup({
                    numhl = true,
                    signcolumn = false,
                })
            end,
        })
        use({
            'jose-elias-alvarez/null-ls.nvim',
            after = 'nvim-lspconfig',
            requires = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' },
            config = function()
                local null_ls = require('null-ls')
                null_ls.setup({
                    sources = {
                        null_ls.builtins.code_actions.gitsigns,
                        null_ls.builtins.diagnostics.shellcheck,
                        null_ls.builtins.formatting.shfmt.with({
                            args = { '-ci', '-sr', '-i', 4 },
                        }),
                        null_ls.builtins.formatting.stylua.with({
                            args = {
                                '--quote-style',
                                'ForceSingle',
                                '--indent-type',
                                'Spaces',
                                '--indent-width',
                                4,
                                '-',
                            },
                        }),
                    },
                    on_attach = require('lsp').on_attach,
                    capabilities = require('lsp').capabilities,
                })
            end,
        })
        use('martinda/Jenkinsfile-vim-syntax')
    end,
    config = {
        display = {
            open_fn = function()
                return require('packer.util').float({ border = 'single' })
            end,
        },
    },
})
