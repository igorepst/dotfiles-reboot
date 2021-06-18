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
    use {'kabouzeid/nvim-lspinstall', config = function()
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  local opts = { noremap = true, silent = true }
  buf_set_keymap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  buf_set_keymap("n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
  buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  buf_set_keymap("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  buf_set_keymap("n", "<Leader>a", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  buf_set_keymap("n", "<Leader>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
  buf_set_keymap("n", "<Leader>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
  buf_set_keymap("n", "<Leader>wl",
                 "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
  buf_set_keymap("n", "<Leader>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  buf_set_keymap("n", "<Leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  buf_set_keymap("n", "<Leader>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
  buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
  buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
  buf_set_keymap("n", "<Leader>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
  buf_set_keymap("n", "<Leader>p",
                 "<cmd>lua vim.lsp.buf.formatting_seq_sync(nil, 1000, { 'html', 'php', 'efm' })<CR>",
                 opts)
  buf_set_keymap("n", "<Leader>P", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  buf_set_keymap("v", "<Leader>p", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  buf_set_keymap("n", "<Leader>l", "<cmd>lua require'lsp-codelens'.buf_codelens_action()<CR>", opts)
  -- vim already has builtin docs
  if vim.bo.ft ~= "vim" then buf_set_keymap("n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts) end

  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec([[
    augroup lsp_document_highlight
      autocmd! * <buffer>
      autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
      autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
    augroup END
    ]], false)
  end

  if client.resolved_capabilities.code_lens then
    vim.cmd [[
    augroup lsp_codelens
      autocmd! * <buffer>
      autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua require"lsp-codelens".buf_codelens_refresh()
    augroup END
    ]]
  end

  if client.server_capabilities.colorProvider then
    require"lsp-documentcolors".buf_attach(bufnr, { single_column = true })
  end
end
-- Configure lua language server for neovim development
local lua_settings = {
  Lua = {
    runtime = {
      -- LuaJIT in the case of Neovim
      version = "LuaJIT",
      path = vim.split(package.path, ";"),
    },
    diagnostics = {
      -- Get the language server to recognize the `vim` global
      globals = { "vim" },
    },
    workspace = {
      -- Make the server aware of Neovim runtime files
      library = {
        [vim.fn.expand("$VIMRUNTIME/lua")] = true,
        [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
      },
    },
  },
}

-- config that activates keymaps and enables snippet support
local function make_config()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities.textDocument.colorProvider = { dynamicRegistration = false }
  return {
    -- enable snippet support
    capabilities = capabilities,
    -- map buffer local keybindings when the language server attaches
    on_attach = on_attach,
  }
end
local function setup_servers()
  require"lspinstall".setup()

  -- get all installed servers
  local servers = require"lspinstall".installed_servers()
  -- ... and add manually installed servers
--   table.insert(servers, "sourcekit")
--   table.insert(servers, "hls")

  for _, server in pairs(servers) do
    local config = make_config()

    -- language specific config
    if server == "lua" then
      config.settings = lua_settings
      config.root_dir = function(fname)
        if fname:match("lush_theme") ~= nil then return nil end
        local util = require "lspconfig/util"
        return util.find_git_ancestor(fname) or util.path.dirname(fname)
      end
    end
    if server == "sourcekit" then
      config.filetypes = { "swift", "objective-c", "objective-cpp" } -- we don't want c and cpp!
    end
    if server == "clangd" then
      config.filetypes = { "c", "cpp" } -- we don't want objective-c and objective-cpp!
    end
    if server == "efm" then config = vim.tbl_extend("force", config, require "efm") end
    if server == "diagnosticls" then
      config = vim.tbl_extend("force", config, require "diagnosticls")
    end
    if server == "vim" then config.init_options = { isNeovim = true } end
    if server == "hls" then
      config.root_dir = require"lspconfig/util".root_pattern("*.cabal", "stack.yaml",
          "cabal.project", "package.yaml",
                                                             "hie.yaml", ".git");
    end

    require"lspconfig"[server].setup(config)
  end
end

setup_servers()
        require'lspinstall'.post_install_hook = function ()
            setup_servers()
            vim.cmd("bufdo e")
        end
    end}
    use {'neovim/nvim-lspconfig'}
-- , config = function()
--         require'lspconfig'.hls.setup{
            -- cmd = { "haskell-language-server-wrapper", "--lsp", "--logfile", "/tmp/hls.log", "--debug" }
--         }
--         require'lspconfig'.efm.setup {
--             filetypes = {'sh'},
--     init_options = {documentFormatting = true},
--     settings = {
--         rootMarkers = {".git/"},
--         languages = {
--             sh = {
--                 {
--                             formatCommand = "shfmt -ci -s -bn",
--                             formatStdin = true, 
--                             lintCommand = "shellcheck -f gcc -x", 
--                             lintSource ="shellcheck", 
--                             lintFormats = {"%f:%l:%c: %trror: %m", "%f:%l:%c: %tarning: %m", "%f:%l:%c: %tote: %m"}
--                         }
--             }
--         }
--     }
-- }
--         require'lspconfig'.sumneko_lua.setup {
--             cmd = {'/usr/bin/lua-language-server', "-E", '/usr/share/lua-language-server/main.lua'};
--             settings = {
--                 Lua = {
--                     runtime = {
--                         version = 'LuaJIT',
--                         path = vim.split(package.path, ';'),
--                     },
--                     diagnostics = {
--                         globals = {'vim'},
--                     },
--                     workspace = {
--                         library = {
--                             [vim.fn.expand('$VIMRUNTIME/lua')] = true,
--                             [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
--                         },
--                     },
--                 },
--             },
--         }
--     end}
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

