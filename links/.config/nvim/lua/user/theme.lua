local _M = {}

_M.statusline = function()
    vim.cmd([[
    augroup felineReset
    autocmd!
    autocmd User PackerCompileDone lua require('feline').reset_highlights()
    autocmd OptionSet background PackerCompile
    augroup END
    ]])

    local c = require('igTermColors').getColors()
    local colors = {
        fg = c.foreground,
        bg = '#e4e4e4',
        black = c.color0,
        skyblue = c.color12,
        cyan = c.color6,
        green = c.color2,
        oceanblue = c.color4,
        magenta = c.color5,
        orange = '#FF9000',
        red = c.color9,
        violet = '#9E93E8',
        white = c.color15,
        yellow = c.color3,
    }

    local function file_osinfo()
        local os = vim.bo.fileformat:upper()
        local icon
        if os == 'UNIX' then
            icon = ''
        elseif os == 'MAC' then
            icon = ''
        else
            icon = ''
        end
        return icon
    end

    local vi_mode_utils = require('feline.providers.vi_mode')

    local comps = {
        vi_mode = {
            left = {
                provider = function()
                    return ' ' .. vi_mode_utils.get_vim_mode() .. ' '
                end,
                hl = function()
                    return {
                        name = vi_mode_utils.get_mode_highlight_name(),
                        fg = colors.bg,
                        bg = vi_mode_utils.get_mode_color(),
                        style = 'bold',
                    }
                end,
            },
        },
        file = {
            info = {
                provider = {
                    name = 'file_info',
                    opts = {
                        type = 'unique',
                        file_modified_icon = '+',
                    },
                },
                hl = {
                    fg = colors.blue,
                    style = 'bold',
                },
                left_sep = ' ',
            },
            size = {
                provider = 'file_size',
                enabled = function()
                    return vim.fn.filereadable(vim.fn.expand('%p')) > 0
                end,
            },
            encoding = {
                provider = 'file_encoding',
                left_sep = ' ',
                hl = {
                    fg = colors.violet,
                    style = 'bold',
                },
            },
            os = {
                provider = file_osinfo,
                left_sep = ' ',
                hl = {
                    fg = colors.violet,
                    style = 'bold',
                },
            },
        },
        position = {
            provider = function()
                local curr_line = vim.fn.line('.')
                local lines = vim.fn.line('$')
                return string.format(
                    ' %d/%d:%d %d%%%% ',
                    curr_line,
                    lines,
                    vim.fn.col('.'),
                    vim.fn.round(curr_line / lines * 100)
                )
            end,
            left_sep = ' ',
            hl = function()
                local val = {
                    name = vi_mode_utils.get_mode_highlight_name(),
                    fg = colors.bg,
                    bg = vi_mode_utils.get_mode_color(),
                    style = 'bold',
                }
                return val
            end,
        },
        diagnos = {
            err = {
                provider = 'diagnostic_errors',
                hl = {
                    fg = colors.red,
                },
            },
            warn = {
                provider = 'diagnostic_warnings',
                hl = {
                    fg = colors.yellow,
                },
            },
            hint = {
                provider = 'diagnostic_hints',
                hl = {
                    fg = colors.cyan,
                },
            },
            info = {
                provider = 'diagnostic_info',
                hl = {
                    fg = colors.blue,
                },
            },
        },
        lsp = {
            name = {
                provider = 'lsp_client_names',
                left_sep = ' ',
                hl = {
                    fg = colors.yellow,
                },
            },
        },
        git = {
            branch = {
                provider = 'git_branch',
                icon = ' ',
                left_sep = ' ',
                hl = {
                    fg = colors.violet,
                    style = 'bold',
                },
            },
            add = {
                provider = 'git_diff_added',
                hl = {
                    fg = colors.green,
                },
            },
            change = {
                provider = 'git_diff_changed',
                hl = {
                    fg = colors.orange,
                },
            },
            remove = {
                provider = 'git_diff_removed',
                hl = {
                    fg = colors.red,
                },
            },
        },
    }

    local components = {
        active = {
            {
                comps.vi_mode.left,
                comps.file.info,
                comps.file.size,
                comps.lsp.name,
                comps.diagnos.err,
                comps.diagnos.warn,
                comps.diagnos.hint,
                comps.diagnos.info,
            },
            {},
            {
                comps.git.add,
                comps.git.change,
                comps.git.remove,
                comps.file.os,
                comps.file.encoding,
                comps.git.branch,
                comps.position,
            },
        },
        inactive = {
            {
                comps.file.info,
            },
            {},
            {
                comps.file.os,
            },
        },
    }

    require('feline').setup({
        components = components,
        colors = colors
    })
end

return _M
