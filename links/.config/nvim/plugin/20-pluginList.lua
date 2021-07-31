local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
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
        use {'wbthomason/packer.nvim'}
        use {'igorepst/hemisu.nvim',
            requires = {'rktjmp/lush.nvim'},
            config = function() require('theme').colorscheme() end}
        use {'nvim-treesitter/nvim-treesitter',
            run = ':TSUpdate',
            config = function() require('treesitter') end}
        use {'famiu/feline.nvim',
            requires = {'kyazdani42/nvim-web-devicons', opt = true},
            config = require('theme').statusline}
        use {'neovim/nvim-lspconfig',
            requires = {'kabouzeid/nvim-lspinstall'},
            config = function() require('lsp') end}
        use {'lewis6991/gitsigns.nvim',
            requires = {'nvim-lua/plenary.nvim'},
            config = function() require('gitsigns').setup() end}
    end,
    config = {
        display = {
            open_fn = function()
                return require('packer.util').float({ border = 'single' })
            end
        }}})

