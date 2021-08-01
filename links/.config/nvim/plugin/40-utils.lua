-- TODO switch for commentstring
function ToggleComment(mode)
    if not vim.b.comment_leader then
        error('Comment leader is undefined')
    end
    local lines = {}
    local line_number_start, line_number_end, line, hl, ms, tmp
    if mode == 'v' then
        line_number_start = vim.fn.line('\'<')
        line_number_end = vim.fn.line('\'>')
    else
        line_number_start = vim.fn.line('.')
        line_number_end = line_number_start + (vim.v.count == 0 and 0 or vim.v.count - 1)
    end
    for i = line_number_start, line_number_end do
        line = vim.fn.getline(i)
        if string.find(line, '^%s*$') then
            table.insert(lines, line)
        else
            tmp = string.gsub(vim.b.comment_leader, '%p', '%%%1')
            ms = string.match(line, '^%s*' .. tmp)
            if ms then
                line = string.gsub(line, tmp, '', 1)
                if string.sub(line, 1, 1) == ' ' then
                    line = string.sub(line, 2)
                end
                table.insert(lines, line)
                hl = true
            else
                table.insert(lines, vim.b.comment_leader .. ' ' .. line)
                hl = true
            end
        end
    end
    if hl then
        vim.api.nvim_buf_set_lines(0, line_number_start - 1, line_number_end, false, lines)
        vim.fn.cursor(line_number_end, 0)
        vim.api.nvim_command('nohlsearch')
    end
end

-- https://github.com/norcalli/nvim_utils
local function nvim_create_augroups(definitions)
    for group_name, definition in pairs(definitions) do
        vim.api.nvim_command('augroup ' .. group_name)
        vim.api.nvim_command('autocmd!')
        for _, def in ipairs(definition) do
            local command = table.concat(vim.tbl_flatten({ 'autocmd', def }), ' ')
            vim.api.nvim_command(command)
        end
        vim.api.nvim_command('augroup END')
    end
end

local autocmds = {
    ig_au = {
        { 'FileType', 'c,cpp,java,rust', 'let b:comment_leader = \'//\'' },
        { 'FileType', 'sh,bash,zsh,jproperties,tmux,conf,xf86conf,fstab,ps1,python,yaml', 'let b:comment_leader = \'#\'' },
        { 'FileType', 'vim,vifm', 'let b:comment_leader = \'"\'' },
        { 'FileType', 'xdefaults', 'let b:comment_leader = \'!\'' },
        { 'FileType', 'dosini', 'let b:comment_leader = \';\'' },
        { 'FileType', 'lua,haskell,cabalconfig', 'let b:comment_leader = \'--\'' },
    },
}

nvim_create_augroups(autocmds)
