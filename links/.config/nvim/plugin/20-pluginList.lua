local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.system({ 'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path })
    vim.api.nvim_command('packadd packer.nvim')
end

vim.cmd([[
    augroup packComp
    autocmd!
    autocmd BufWritePost 20-pluginList.lua source <afile> | PackerCompile
    augroup END
]])

return require('packer').startup({
    function(use)
        use({ 'wbthomason/packer.nvim' })
        use({'igorepst/igCommands.nvim',
        config = function()
            require('igCommands').setup({
                prefix = 'ig',
                commands = {
                        {name = 'Toggle list', cmd = 'lua vim.opt.list = not vim.opt.list:get()', key = '<Leader>tl'},
                        {name = 'Toggle wrap', cmd = 'lua vim.opt.wrap = not vim.opt.wrap:get()'},
                        {name = 'Toggle hlsearch', cmd = 'lua vim.opt.hlsearch = not vim.opt.hlsearch:get()'},
                        {name = 'Convert to DOS', cmd = 'set ff=dos'},
                        {name = 'Convert to Unix', cmd = 'set ff=unix'},
                        {name = 'Show DOS line endings', cmd = 'edit ++ff=unix'},
                        {name = 'Convert to UTF-8', cmd = 'set fileencoding=utf8'},
                        {name = 'Format JSON', cmd = "%!jq -R '. as $line | try fromjson catch $line'"},
                        {name = 'Trim trailing space', cmd = "execute ':%s/\\s\\+$//e'"},
                        {name = 'Reindent file',  cmd = "execute 'normal! mzgg=G`z' | IgTrimTrailingSpace"},
                        {name = 'Diff saved', cmd = 'vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis'},
                    }
                })
        end,
        })
        use({
            'igorepst/hemisu.nvim',
            requires = { 'rktjmp/lush.nvim' },
            config = function()
                require('theme').colorscheme()
            end,
        })
--         use({'rakr/vim-one', config = function()
--             vim.cmd[[
--                 let g:one_allow_italics = 1
--                 colorscheme one]]
--         end})
--       use({'kaicataldo/material.vim', config = function()
--        vim.cmd[[
--            let g:material_terminal_italics = 1
--            let g:material_theme_style = 'lighter'
--         colorscheme material
--         ]]
--             end
--         })
        use({'Pocco81/Catppuccino.nvim', config = function()
            local catppuccino = require("catppuccino")

-- configure it
catppuccino.setup(
    {
		colorscheme = "light_melya",
		transparency = false,
		styles = {
			comments = "italic",
			functions = "italic",
			keywords = "italic",
			strings = "NONE",
			variables = "NONE",
		},
		integrations = {
			treesitter = true,
			native_lsp = {
				enabled = true,
				styles = {
					errors = "italic",
					hints = "italic",
					warnings = "italic",
					information = "italic"
				}
			},
			lsp_trouble = false,
			lsp_saga = false,
			gitgutter = false,
			gitsigns = false,
			telescope = false,
			nvimtree = false,
			which_key = false,
			indent_blankline = false,
			dashboard = false,
			neogit = false,
			vim_sneak = false,
			fern = false,
			barbar = false,
			bufferline = false,
			markdown = false,
		}
	}
)

-- load it
-- catppuccino.load()

            end})
        use({
            'nvim-treesitter/nvim-treesitter',
            run = ':TSUpdate',
            config = function()
                require('treesitter')
            end,
        })
        use({
            'famiu/feline.nvim',
            requires = { 'kyazdani42/nvim-web-devicons', opt = true },
            config = require('theme').statusline,
        })
        use({
            'neovim/nvim-lspconfig',
            requires = { 'kabouzeid/nvim-lspinstall' },
            config = function()
                require('lsp')
            end,
        })
        use({
            'lewis6991/gitsigns.nvim',
            requires = { 'nvim-lua/plenary.nvim' },
            config = function()
                require('gitsigns').setup()
            end,
        })
        use({
            'jose-elias-alvarez/null-ls.nvim',
            after = 'nvim-lspconfig',
            requires = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' },
            config = function()
                require('null-ls').config({
                    sources = {
                        require('null-ls').builtins.formatting.stylua.with({
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
                })
                require('lspconfig')['null-ls'].setup({
                    on_attach = require('lsp').on_attach,
                })
            end,
        })
    end,
    config = {
        display = {
            open_fn = function()
                return require('packer.util').float({ border = 'single' })
            end,
        },
    },
})
