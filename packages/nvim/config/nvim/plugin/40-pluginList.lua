vim.cmd([[
    augroup packComp
    autocmd!
    autocmd BufWritePost 40-pluginList.lua source <afile> | PackerCompile
    augroup END
]])

pcall(require, 'packer_compiled')

local packer = require('packer')
local util = require('packer.util')
packer.init({
    compile_path = util.join_paths(vim.fn.stdpath('config'), 'lua', 'packer_compiled.lua'),
    display = {
        open_fn = function()
            return util.float({ border = 'single' })
        end,
        prompt_border = 'single',
    },
})
local use = packer.use
packer.reset()

use({ 'wbthomason/packer.nvim' })
use({ 'lewis6991/impatient.nvim' })
use({
    'igorepst/igTermColors.nvim',
    config = [[
        local color_overrides = nil
        local f = loadfile(vim.fn.expand('~/.theme/nvimThemeColors.lua'))
        if f then
            color_overrides = f()
        end
        require('igTermColors').setup({ color_overrides = color_overrides, invert_for_dark = true })
        vim.cmd('colorscheme igTermColors')
    ]],
})
use({
    'igorepst/igToggleComment.nvim',
    keys = {
        { 'n', '<Plug>(IgToggleComment)' },
        { 'i', '<Plug>(IgToggleComment)' },
        { 'v', '<Plug>(IgToggleComment)' },
    },
})
use({
    'igorepst/igCommands.nvim',
    config = [[require('user.igCommands')]],
})
use({
    'ibhagwan/fzf-lua',
    requires = {
        'kyazdani42/nvim-web-devicons',
    },
    config = [[require('user.fzf')]],
})
use({
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = [[require('user.treesitter').config()]],
})
use({'rebelot/heirline.nvim',
    requires = { 'kyazdani42/nvim-web-devicons'},
    after = 'igTermColors.nvim',
    config = [[require('user.heirline')]]
})
use({
    'neovim/nvim-lspconfig',
    config = [[require('user.lsp')]],
})
use('hrsh7th/nvim-cmp')
use('hrsh7th/cmp-nvim-lsp')
use('hrsh7th/cmp-buffer')
use('hrsh7th/cmp-path')
use('hrsh7th/cmp-cmdline')
use('L3MON4D3/LuaSnip')
use('saadparwaiz1/cmp_luasnip')
use({
    'lewis6991/gitsigns.nvim',
    requires = { 'nvim-lua/plenary.nvim' },
    config = [[
        require('gitsigns').setup({
            numhl = true,
            signcolumn = false,
        })
]],
})
use({
    'jose-elias-alvarez/null-ls.nvim',
    after = 'nvim-lspconfig',
    requires = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' },
    config = [[require('user.null-ls')]],
})
use('martinda/Jenkinsfile-vim-syntax')
