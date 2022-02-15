local conditions = require('heirline.conditions')
local utils = require('heirline.utils')
local devicons = require('nvim-web-devicons')

local c = require('igTermColors').getColors()
c.diag = {
    warn = utils.get_highlight('DiagnosticWarn').fg,
    error = utils.get_highlight('DiagnosticError').fg,
    hint = utils.get_highlight('DiagnosticHint').fg,
    info = utils.get_highlight('DiagnosticInfo').fg,
}
c.git = {
    del = utils.get_highlight('DiffDelete').fg,
    add = utils.get_highlight('DiffAdd').fg,
    change = utils.get_highlight('DiffChange').fg,
}

local sur = function(comp)
    return utils.surround({ ' ', ' ' }, nil, comp)
end

local viMode = {
    init = function(self)
        self.mode = vim.fn.mode():lower()
    end,
    condition = function(_)
        return conditions.is_active()
    end,
    static = {
        modes = {
            n = { n = 'NORM', c = c.color2 },
            v = { n = 'VIS', c = c.color6 },
            s = { n = 'SEL', c = c.color6 },
            i = { n = 'INS', c = c.color1 },
            r = { n = 'REPL', c = c.color1 },
            c = { n = 'COMM', c = c.color5 },
            t = { n = 'TERM', c = c.color8 },
        },
    },
    hl = function(self)
        local m = self.modes[self.mode]
        return { fg = c.color7, bg = m and m.c or c.color0, style = 'bold' }
    end,
}

local viModeP = {
    provider = function(self)
        local m = self.modes[self.mode]
        return ' %-5(' .. (m and m.n or 'NONE') .. '%)'
    end,
}

local fileIcon = {
    init = function(self)
        local filename = self.filename
        local extension = vim.fn.fnamemodify(filename, ':e')
        self.icon, self.icon_color = devicons.get_icon_color(filename, extension, { default = true })
    end,
    provider = function(self)
        return self.icon
    end,
    hl = function(self)
        return { fg = self.icon_color }
    end,
}

local fileName = {
    provider = function(self)
        local filename = self.filename
        return filename == '' and '[No Name]' or vim.fn.fnamemodify(filename, ':t')
    end,
    hl = { style = 'bold' },
}

local fileFlags = {
    {
        provider = function()
            if vim.bo.modified then
                return ' +'
            end
        end,
        hl = { fg = c.color2, style = 'bold' },
    },
    {
        provider = function()
            if not vim.bo.modifiable or vim.bo.readonly then
                return ' '
            end
        end,
        hl = { fg = c.color3, style = 'bold' },
    },
}

local fileSize = {
    provider = function()
        local suffix = { 'b', 'k', 'M', 'G', 'T', 'P', 'E' }
        local fsize = vim.fn.getfsize(vim.api.nvim_buf_get_name(0))
        if fsize <= 0 then
            return ''
        end
        local index = 1
        while fsize > 1024 and index < 7 do
            fsize = fsize / 1024
            index = index + 1
        end
        return string.format(index == 1 and '%g%s' or '%.2f%s', fsize, suffix[index])
    end,
}

local fileNameBlock = utils.insert({
    init = function(self)
        self.filename = vim.api.nvim_buf_get_name(0)
    end,
}, fileIcon, sur(fileName), fileSize, fileFlags, { provider = '%<' })

local ruler = {
    provider = ' %7(%l/%3L%):%-c %P ',
}

local fileEncoding = {
    provider = function()
        local enc = (vim.bo.fenc ~= '' and vim.bo.fenc or vim.o.enc) .. ' '
        return enc:upper()
    end,
}

local fileFormat = {
    provider = function()
        local os = vim.bo.fileformat:upper()
        local icon
        if os == 'UNIX' then
            icon = ' '
        elseif os == 'MAC' then
            icon = ' '
        else
            icon = ' '
        end
        return icon
    end,
}

local lspActive = {
    condition = conditions.lsp_attached,
    provider = function()
        local names = {}
        for _, server in ipairs(vim.lsp.buf_get_clients(0)) do
            table.insert(names, server.name)
        end
        return ' ' .. table.concat(names, ' ')
    end,
    hl = { fg = c.color2 },
}

local diagnostics = {
    condition = conditions.has_diagnostics,
    static = {
        error_icon = vim.fn.sign_getdefined('DiagnosticSignError')[1].text,
        warn_icon = vim.fn.sign_getdefined('DiagnosticSignWarn')[1].text,
        info_icon = vim.fn.sign_getdefined('DiagnosticSignInfo')[1].text,
        hint_icon = vim.fn.sign_getdefined('DiagnosticSignHint')[1].text,
    },
    init = function(self)
        self.errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
        self.warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
        self.hints = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
        self.info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
    end,
    {
        provider = function(self)
            return self.errors > 0 and (' ' .. self.error_icon .. self.errors)
        end,
        hl = { fg = c.diag.error },
    },
    {
        provider = function(self)
            return self.warnings > 0 and (' ' .. self.warn_icon .. self.warnings)
        end,
        hl = { fg = c.diag.warn },
    },
    {
        provider = function(self)
            return self.info > 0 and (' ' .. self.info_icon .. self.info)
        end,
        hl = { fg = c.diag.info },
    },
    {
        provider = function(self)
            return self.hints > 0 and (' ' .. self.hint_icon .. self.hints)
        end,
        hl = { fg = c.diag.hint },
    },
}

local git = {
    condition = conditions.is_git_repo,
    init = function(self)
        self.status_dict = vim.b.gitsigns_status_dict
        self.has_changes = self.status_dict.added ~= 0 or self.status_dict.removed ~= 0 or self.status_dict.changed ~= 0
    end,
    hl = { fg = c.color6 },
    {
        provider = function(self)
            local count = self.status_dict.added or 0
            return count > 0 and (' ' .. count)
        end,
        hl = { fg = c.git.add },
    },
    {
        provider = function(self)
            local count = self.status_dict.removed or 0
            return count > 0 and ('  ' .. count)
        end,
        hl = { fg = c.git.del },
    },
    {
        provider = function(self)
            local count = self.status_dict.changed or 0
            return count > 0 and (' 柳' .. count)
        end,
        hl = { fg = c.git.change },
    },
    {
        provider = function(self)
            return '  ' .. self.status_dict.head .. ' '
        end,
    },
}

local statusline = {
    utils.insert(viMode, viModeP),
    sur(fileNameBlock),
    lspActive,
    diagnostics,
    { provider = '%=' },
    git,
    fileFormat,
    fileEncoding,
    utils.insert(viMode, ruler),
    hl = { fg = c.foreground, bg = c.background },
}

require('heirline').setup(statusline)
