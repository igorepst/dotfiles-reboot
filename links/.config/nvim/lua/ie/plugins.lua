local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
	execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
	execute 'packadd packer.nvim'
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
	use {'wbthomason/packer.nvim', opt = true}
	use {'NLKNguyen/papercolor-theme'}
	use {'junegunn/fzf'}
	use {'junegunn/fzf.vim'}
	use {'neovim/nvim-lspconfig'}
	use {'nvim-treesitter/nvim-treesitter', config = function()
		require ('nvim-treesitter.configs').setup {
			ensure_installed = "all",
			highlight = {
				enable = true,
			},
			indent = {
				enable = true
			},
		}
	end, run = ':execute "TSInstall all" | execute "TSUpdate"',}
end)
