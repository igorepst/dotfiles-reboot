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
	use {'junegunn/fzf.vim', requires = {{'junegunn/fzf'}}}
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
			incremental_selection = {
				enable = true
			},
		}
	end, run = ':execute "TSInstall all" | execute "TSUpdate"',}
use {'tjdevries/express_line.nvim', requires = {{'nvim-lua/plenary.nvim'}}, config = function()
	local bui        = require('el.builtin')
local ext        = require('el.extensions')
local sec        = require('el.sections')
local sub        = require('el.subscribe')
local col        = sec.collapse_builtin
local _space     = ' '
local _separator = ' | '
local _icon      = ext.file_icon
local _file      = bui.full_file -- TODO: have [NO NAME] showing if no file name
local _readonly  = col{ _space, bui.readonly_list, } -- TODO: fix the extra space when the read-only is not visible
local _modefied  = col{ _space, bui.modified_flag }
local _filetype  = col{ _separator, bui.filetype_list, _separator}
local _gbranch   = col{ _separator, ext.git_branch}
local _gchanges  = sub.buf_autocmd( "el_git_changes", "BufWritePost", function(window, buffer) return ext.git_changes(window, buffer) end)
local _buflines  = bui.line_with_width(1) .. ',' .. bui.column_with_width(1)
local _bufloc    = bui.percentage_through_window
local _split     = sec.split
local statusline = function()
  return {
    _space, 
    _icon,
    _space,
    _file, 
    _readonly,
    _modefied,
    _split,
    -- _gbranch,
    _filetype,
    _buflines,
    _separator,
    _bufloc,
    _space,
  }
end

  require('el').setup({ generator = statusline })
	end}
end)
