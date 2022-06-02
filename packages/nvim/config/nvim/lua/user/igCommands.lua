require('igCommands').setup({
    prefix = 'ig',
    commands = {
        {
            name = 'Toggle list',
            cmd = 'lua vim.opt.list = not vim.opt.list:get()',
            key = '<Leader>tl',
        },
        { name = 'Toggle wrap', cmd = 'lua vim.opt.wrap = not vim.opt.wrap:get()' },
        { name = 'Toggle hlsearch', cmd = 'lua vim.opt.hlsearch = not vim.opt.hlsearch:get()' },
        { name = 'Convert to DOS', cmd = 'set ff=dos' },
        { name = 'Convert to Unix', cmd = 'set ff=unix' },
        { name = 'Show DOS line endings', cmd = 'edit ++ff=unix' },
        { name = 'Convert to UTF-8', cmd = 'set fileencoding=utf8' },
        { name = 'Format JSON', cmd = '%!jq -R \'. as $line | try fromjson catch $line\'' },
        { name = 'Trim trailing space', cmd = 'execute \':%s/\\s\\+$//e\'' },
        { name = 'Reindent file', cmd = 'execute \'normal! mzgg=G`z\' | IgTrimTrailingSpace' },
        {
            name = 'Diff saved',
            cmd = 'vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis',
        },
        { name = 'Find file in runtime', cmd = 'lua FindFileInRuntime()' },
        { name = 'Search in runtime', cmd = 'lua SearchInRuntime()' },
    },
})
