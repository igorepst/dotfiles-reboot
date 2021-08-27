function ToggleComment(mode)
    local commentstring = vim.opt.commentstring:get()
    if commentstring == nil or commentstring == '' then
        print('Comment string is undefined, using default: /*%s*/')
        commentstring = '/*%s*/'
    end
    local lines = {}
    local line_number_start, line_number_end, line, hl
    if mode == 'v' then
        line_number_start = vim.fn.line('\'<')
        line_number_end = vim.fn.line('\'>')
    else
        line_number_start = vim.fn.line('.')
        line_number_end = line_number_start + (vim.v.count == 0 and 0 or vim.v.count - 1)
    end
    local left, right = string.match(commentstring, '^(.*)%%s(.*)')
    for i = line_number_start, line_number_end do
        line = vim.fn.getline(i)
        if string.find(line, '^%s*$') then
            table.insert(lines, line)
        else
            local fi,sec,th = string.match(line, '^(%s*)' .. vim.pesc(left) .. '(.*)' .. vim.pesc(right) .. '(%s*)$')
            if sec then
                if string.sub(sec, 1, 1) == ' ' then
                    sec = string.sub(sec, 2)
                end
                table.insert(lines, fi .. sec .. th)
            else
                table.insert(lines, left .. ' ' .. line .. right)
            end
            hl = true
        end
    end
    if hl then
        vim.api.nvim_buf_set_lines(0, line_number_start - 1, line_number_end, false, lines)
        vim.fn.cursor(line_number_end, 0)
        vim.api.nvim_command('nohlsearch')
    end
end

