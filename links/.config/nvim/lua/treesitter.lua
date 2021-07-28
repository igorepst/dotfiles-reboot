local present, ts_config = pcall(require, "nvim-treesitter.configs")
if not present then
    return
end

ts_config.setup {
    ensure_installed = {
        "bash",
        "c",
        "comment",
        "cpp",
        "css",
        "dockerfile",
        "go",
        "gomod",
        "haskell",
        "html",
        "java",
        "javascript",
        "json",
        "lua",
        "python",
        "regex",
        "rust",
        "scss",
        "toml",
        "typescript",
        "yaml"
    },
    highlight = {
        enable = true,
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "gnn",
            node_incremental = "grn",
            scope_incremental = "grc",
            node_decremental = "grm",
        },
    },indent = {
        enable = true
    }
}
