function ShowCurrentWordHighlight()
    vim.cmd([[TSBufDisable highlight]])
    for _, v in ipairs(vim.fn.synstack(vim.fn.line('.'), vim.fn.col('.'))) do
        print(vim.fn.synIDattr(v, 'name') .. ' -> ' .. vim.fn.synIDattr(vim.fn.synIDtrans(v), 'name'))
    end
    vim.cmd([[TSBufEnable highlight]])
end
