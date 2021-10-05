function FindFileInRuntime()
    require('fzf-lua').files({
        raw_cmd = 'fd --type file . '
            .. vim.fn.stdpath('config')
            .. ' '
            .. vim.fn.stdpath('data')
            .. ' '
            .. vim.fn.getenv('VIMRUNTIME'),
    })
end

function SearchInRuntime()
    require('fzf-lua').live_grep({
        filespec = '-- $(fd --type file . '
            .. vim.fn.stdpath('config')
            .. ' '
            .. vim.fn.stdpath('data')
            .. ' '
            .. vim.fn.getenv('VIMRUNTIME')
            .. ')',
    })
end

function ShowCurrentWordHighlight()
    vim.cmd([[TSBufDisable highlight]])
    for _, v in ipairs(vim.fn.synstack(vim.fn.line('.'), vim.fn.col('.'))) do
        print(vim.fn.synIDattr(v, 'name') .. ' -> ' .. vim.fn.synIDattr(vim.fn.synIDtrans(v), 'name'))
    end
    vim.cmd([[TSBufEnable highlight]])
end
