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
    on_attach = require('user.lsp').on_attach,
    capabilities = require('user.lsp').capabilities,
})
