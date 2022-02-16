local opts = { silent = true }
local map = vim.keymap.set
map('n', '<leader>e', '<cmd>lua vim.diagnostic.open_float(0, {scope = "line"})<CR>', opts)
map('n', '<leader>d', '<cmd>lua vim.diagnostic.open_float(0, {scope = "buffer"})<CR>', opts)
map('n', '[d', vim.diagnostic.goto_prev, opts)
map('n', ']d', vim.diagnostic.goto_next, opts)
map('n', '<leader>q', vim.diagnostic.setloclist, opts)
map('n', '<leader>f', vim.lsp.buf.formatting, opts)

local on_attach = function(_, bufnr)
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    local bopts = { silent = true, buffer = bufnr }
    map('n', 'gD', vim.lsp.buf.declaration, bopts)
    map('n', 'gd', vim.lsp.buf.definition, bopts)
    map('n', 'K', vim.lsp.buf.hover, bopts)
    map('n', 'gi', vim.lsp.buf.implementation, bopts)
    map('n', '<C-k>', vim.lsp.buf.signature_help, bopts)
    map('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, bopts)
    map('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, bopts)
    map('n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', bopts)
    map('n', '<leader>D', vim.lsp.buf.type_definition, bopts)
    map('n', '<leader>rn', vim.lsp.buf.rename, bopts)
    map('n', '<leader>ca', vim.lsp.buf.code_action, bopts)
    map('n', 'gr', vim.lsp.buf.references, bopts)
end

vim.cmd([[autocmd ColorScheme * highlight NormalFloat guibg=white]])
vim.cmd([[autocmd ColorScheme * highlight FloatBorder guifg=black guibg=white]])

local border = {
    { '🭽', 'FloatBorder' },
    { '▔', 'FloatBorder' },
    { '🭾', 'FloatBorder' },
    { '▕', 'FloatBorder' },
    { '🭿', 'FloatBorder' },
    { '▁', 'FloatBorder' },
    { '🭼', 'FloatBorder' },
    { '▏', 'FloatBorder' },
}

local handlers = {
    ['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
    ['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local lspconfig = require('lspconfig')

lspconfig.rust_analyzer.setup({ capabilities = capabilities, on_attach = on_attach, handlers = handlers })
lspconfig.gopls.setup({ capabilities = capabilities, on_attach = on_attach, handlers = handlers })
lspconfig.dockerls.setup({
    capabilities = capabilities,
    cmd = { vim.fn.stdpath('cache') .. '/dockerfile/node_modules/.bin/docker-langserver', '--stdio' },
    on_attach = on_attach,
    handlers = handlers,
})
local vscode_lsp = vim.fn.stdpath('cache') .. '/lspServers/vscode-langservers-extracted/node_modules/.bin/'
lspconfig.cssls.setup({ capabilities = capabilities, cmd = { vscode_lsp .. 'vscode-css-language-server', '--stdio' } })
lspconfig.eslint.setup({
    capabilities = capabilities,
    cmd = { vscode_lsp .. 'vscode-eslint-language-server', '--stdio' },
    on_attach = on_attach,
    handlers = handlers,
})
lspconfig.html.setup({
    capabilities = capabilities,
    cmd = { vscode_lsp .. 'vscode-html-language-server', '--stdio' },
    on_attach = on_attach,
    handlers = handlers,
})
lspconfig.jsonls.setup({
    capabilities = capabilities,
    cmd = { vscode_lsp .. 'vscode-json-language-server', '--stdio' },
    on_attach = on_attach,
    handlers = handlers,
})

lspconfig.bashls.setup({
    cmd = { vim.fn.stdpath('cache') .. '/lspServers/bash/node_modules/.bin/bash-language-server', 'start' },
    capabilities = capabilities,
    on_attach = on_attach,
    handlers = handlers,
})

local sumneko_root_path = vim.fn.stdpath('cache') .. '/lspServers/lua/sumneko-lua/extension/server'
local sumneko_binary = sumneko_root_path .. '/bin/lua-language-server'

-- Make runtime files discoverable to the server
local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, 'lua/?.lua')
table.insert(runtime_path, 'lua/?/init.lua')

local function find_and_trim(str, ptn)
    local ind = str:find(ptn)
    if ind then
        return str:sub(0, ind + #ptn - 1)
    end
    return nil
end

local function ends_with(str, ending)
    return str:sub(-#ending) == ending
end

lspconfig.sumneko_lua.setup({
    cmd = { sumneko_binary, '-E', sumneko_root_path .. '/main.lua' },
    on_attach = on_attach,
    capabilities = capabilities,
    handlers = handlers,
    root_dir = function(fname)
        local dirname = lspconfig.util.path.dirname(fname)
        local cand = find_and_trim(dirname, '.config/awesome') or find_and_trim(dirname, '.config/nvim')
        if cand then
            return cand
        end
        return lspconfig.sumneko_lua.document_config.default_config.root_dir(fname)
    end,
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = 'LuaJIT',
                -- Setup your lua path
                path = runtime_path,
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = { 'vim' },
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = vim.api.nvim_get_runtime_file('', true),
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
                enable = false,
            },
        },
    },
    on_new_config = function(new_config, new_root_dir)
        if ends_with(new_root_dir, '.config/awesome') then
            new_config.settings.Lua.diagnostics.globals = {
                'awesome',
                'client',
                'screen',
                'tag',
                'mouse',
                'keygrabber',
            }
            new_config.settings.Lua.workspace.library = { new_root_dir, '/usr/share/awesome/lib' }
            --             TODO runtime path
        elseif ends_with(new_root_dir, '.config/nvim') then
            new_config.settings.Lua.diagnostics.globals = { 'vim' }
            new_config.settings.Lua.workspace.library = vim.api.nvim_get_runtime_file('', true)
        end
    end,
})

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = true,
})

local signs = { Error = ' ', Warn = ' ', Hint = ' ', Info = ' ' }
for type, icon in pairs(signs) do
    local hl = 'DiagnosticSign' .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

return {
    on_attach = on_attach,
}
