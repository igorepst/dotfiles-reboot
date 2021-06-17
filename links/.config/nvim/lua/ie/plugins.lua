local execute = vim.api.nvim_command
local fn = vim.fn
local cmd = vim.cmd

local install_path = fn.stdpath('data') .. '/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
    execute 'packadd packer.nvim'
end

cmd 'packadd packer.nvim'
cmd 'autocmd BufWritePost plugins.lua execute "luafile %" | PackerCompile'

return require('packer').startup({function(use)
    use {'wbthomason/packer.nvim', opt = true}
    use 'noahfrederick/vim-hemisu'
    use {'junegunn/fzf.vim', requires = {{'junegunn/fzf'}}}
    use 'vifm/vifm.vim'
    use {'neovim/nvim-lspconfig', config = function()
        require'lspconfig'.hls.setup{
                -- cmd = { "haskell-language-server-wrapper", "--lsp", "--logfile", "/tmp/hls.log", "--debug" }
            }
            require'lspconfig'.sumneko_lua.setup {
                cmd = {'/usr/bin/lua-language-server', "-E", '/usr/share/lua-language-server/main.lua'};
                settings = {
                    Lua = {
                        runtime = {
                            version = 'LuaJIT',
                            path = vim.split(package.path, ';'),
                        },
                        diagnostics = {
                            globals = {'vim'},
                        },
                        workspace = {
                            library = {
                                [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                                [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
                            },
                        },
                    },
                },
            }
        end}
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
    end, run = ':TSUpdate'}
    use {'hoob3rt/lualine.nvim', requires = {'kyazdani42/nvim-web-devicons', opt = true}, config = function()
        require'lualine'.setup {
            options = {
                icons_enabled = true,
                theme = 'onelight',
                component_separators = {'', ''},
                section_separators = {'', ''},
                disabled_filetypes = {}
            },
            sections = {
                lualine_a = {'mode'},
                lualine_b = {'branch'},
                lualine_c = {'filename'},
                lualine_x = {'encoding', 'fileformat', 'filetype'},
                lualine_y = {'progress'},
                lualine_z = {'location'}
            },
            inactive_sections = {
                lualine_a = {},
                lualine_b = {},
                lualine_c = {'filename'},
                lualine_x = {'encoding', 'fileformat', 'filetype'},
                lualine_y = {},
                lualine_z = {'location'}
            },
            tabline = {},
            extensions = {}
        }
    end}
    use {'hrsh7th/nvim-compe', config = function()
        require ('compe').setup {
            enabled = true;
            autocomplete = false;
            debug = false;
            min_length = 1;
            preselect = 'enable';
            throttle_time = 80;
            source_timeout = 200;
            incomplete_delay = 400;
            max_abbr_width = 100;
            max_kind_width = 100;
            max_menu_width = 100;
            documentation = true;

            source = {
                path = true;
                buffer = true;
                calc = true;
                nvim_lsp = true;
                nvim_lua = true;
                vsnip = false;
                ultisnips = false;
            };
        }
    end
    }
end,
config = {
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'single' })
    end
  }
}})

