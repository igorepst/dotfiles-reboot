local _M = {}

_M.colorscheme = function()
    vim.cmd('silent! colorscheme hemisu')
end

_M.statusline = function()
    --     local present, feline = pcall(require, 'feline')
    --     if not present then
    --         return
    --     end

    vim.cmd([[
    augroup felineReset
    autocmd!
    autocmd User PackerCompileDone lua require('feline').reset_highlights()
    autocmd OptionSet background PackerCompile
    augroup END
    ]])

    local hemisu_c = require('lush_theme.hemisu_colors')
    local colors = {
        bg = hemisu_c.bg.hex,
        fg = hemisu_c.norm.hex,
        yellow = '#503d15',
        cyan = '#538192',
        darkblue = hemisu_c.faintBlue.hex,
        green = hemisu_c.normGreen.hex,
        orange = '#d19a66',
        violet = '#b294bb',
        magenta = '#ff80ff',
        blue = hemisu_c.normBlue.hex,
        red = hemisu_c.normRed.hex,
    }

    local vi_mode_colors = {
        NORMAL = colors.green,
        OP = colors.green,
        INSERT = colors.blue,
        VISUAL = colors.violet,
        BLOCK = colors.blue,
        REPLACE = colors.red,
        ['V-REPLACE'] = colors.red,
        ENTER = colors.cyan,
        MORE = colors.cyan,
        SELECT = colors.orange,
        COMMAND = colors.magenta,
        SHELL = colors.green,
        TERM = colors.blue,
        NONE = colors.yellow,
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

    local lsp = require('feline.providers.lsp')
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
                provider = 'file_info',
                file_modified_icon = '+',
                type = 'unique',
                hl = {
                    fg = colors.blue,
                    style = 'bold',
                },
                left_sep = ' ',
            },
            size = {
                provider = 'file_size',
                enabled = function() return vim.fn.filereadable(vim.fn.expand('%p')) > 0 end
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
                enabled = function()
                    return lsp.diagnostics_exist('Error')
                end,
                hl = {
                    fg = colors.red,
                },
            },
            warn = {
                provider = 'diagnostic_warnings',
                enabled = function()
                    return lsp.diagnostics_exist('Warning')
                end,
                hl = {
                    fg = colors.yellow,
                },
            },
            hint = {
                provider = 'diagnostic_hints',
                enabled = function()
                    return lsp.diagnostics_exist('Hint')
                end,
                hl = {
                    fg = colors.cyan,
                },
            },
            info = {
                provider = 'diagnostic_info',
                enabled = function()
                    return lsp.diagnostics_exist('Information')
                end,
                hl = {
                    fg = colors.blue,
                },
            },
        },
        lsp = {
            name = {
                provider = function(component)
                    local clients = {}
                    local icon = component.icon or ' '

                    for _, client in pairs(vim.lsp.buf_get_clients()) do
                        clients[#clients + 1] = client.name
                    end

                    return icon .. table.concat(clients, ',')
                end,
                enabled = function()
                    return lsp.is_lsp_attached()
                end,
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

    local properties = {
        force_inactive = {
            filetypes = {
                'NvimTree',
                'dbui',
                'packer',
                'startify',
                'fugitive',
                'fugitiveblame',
            },
            buftypes = { 'terminal' },
            bufnames = {},
        },
    }

    local components = {
        left = {
            active = {
                comps.vi_mode.left,
                comps.file.info,
                comps.file.size,
                comps.lsp.name,
                comps.diagnos.err,
                comps.diagnos.warn,
                comps.diagnos.hint,
                comps.diagnos.info,
            },
            inactive = {
                comps.file.info,
            },
        },
        mid = {
            active = {},
            inactive = {},
        },
        right = {
            active = {
                comps.git.add,
                comps.git.change,
                comps.git.remove,
                comps.file.os,
                comps.file.encoding,
                comps.git.branch,
                comps.position,
            },
            inactive = {
                comps.file.os,
            },
        },
    }

    require('feline').setup({
        default_bg = colors.bg,
        default_fg = colors.fg,
        components = components,
        properties = properties,
        vi_mode_colors = vi_mode_colors,
    })
end

return _M
