local _M = {}

_M.colorscheme = function()
    vim.cmd('silent! colorscheme hemisu')
end

_M.statusline = function()
    local present, feline = pcall(require, 'feline')
    if not present then
        return
    end
    local lsp = require('feline.providers.lsp')
    local vi_mode_utils = require('feline.providers.vi_mode')

    local b = vim.b
    local fn = vim.fn

    local properties = {
        force_inactive = {
            filetypes = {},
            buftypes = {},
            bufnames = {}
        }
    }

    local components = {
        left = {
            active = {},
            inactive = {}
        },
        mid = {
            active = {},
            inactive = {}
        },
        right = {
            active = {},
            inactive = {}
        }
    }

    properties.force_inactive.filetypes = {
        'NvimTree',
        'dbui',
        'packer',
        'startify',
        'fugitive',
        'fugitiveblame'
    }

    properties.force_inactive.buftypes = {
        'terminal'
    }

    components.left.active[1] = {
        provider = 'vi_mode',
        hl = function()
            local val = {}

            val.name = vi_mode_utils.get_mode_highlight_name()
            val.bg = vi_mode_utils.get_mode_color()
            val.style = 'bold'

            return val
        end,
        right_sep = ' '
    }

    components.left.active[2] = {
        provider = 'file_info',
        hl = {
            fg = 'white',
            bg = 'oceanblue',
            style = 'bold'
        },
        left_sep = {
            ' ', 'slant_left_2',
            {str = ' ', hl = {bg = 'oceanblue', fg = 'NONE'}}
        },
        right_sep = {'slant_right_2', ' '}
    }

    components.left.active[3] = {
        provider = 'file_size',
        enabled = function() return fn.getfsize(fn.expand('%:p')) > 0 end,
        right_sep = {
            ' ',
            {
                str = 'slant_left_2_thin',
                hl = {
                    fg = 'fg',
                    bg = 'bg'
                }
            },
        }
    }

    components.left.active[4] = {
        provider = 'position',
        left_sep = ' ',
        right_sep = {
            ' ',
            {
                str = 'slant_right_2_thin',
                hl = {
                    fg = 'fg',
                    bg = 'bg'
                }
            }
        }
    }

    components.left.active[5] = {
        provider = 'diagnostic_errors',
        enabled = function() return lsp.diagnostics_exist('Error') end,
        hl = { fg = 'red' }
    }

    components.left.active[6] = {
        provider = 'diagnostic_warnings',
        enabled = function() return lsp.diagnostics_exist('Warning') end,
        hl = { fg = 'yellow' }
    }

    components.left.active[7] = {
        provider = 'diagnostic_hints',
        enabled = function() return lsp.diagnostics_exist('Hint') end,
        hl = { fg = 'cyan' }
    }

    components.left.active[8] = {
        provider = 'diagnostic_info',
        enabled = function() return lsp.diagnostics_exist('Information') end,
        hl = { fg = 'skyblue' }
    }

    components.right.active[1] = {
        provider = 'git_branch',
        hl = {
            fg = 'white',
            bg = 'black',
            style = 'bold'
        },
        right_sep = function()
            local val = {hl = {fg = 'NONE', bg = 'black'}}
            if b.gitsigns_status_dict then val.str = ' ' else val.str = '' end

            return val
        end
    }

    components.right.active[2] = {
        provider = 'git_diff_added',
        hl = {
            fg = 'green',
            bg = 'black'
        }
    }

    components.right.active[3] = {
        provider = 'git_diff_changed',
        hl = {
            fg = 'orange',
            bg = 'black'
        }
    }

    components.right.active[4] = {
        provider = 'git_diff_removed',
        hl = {
            fg = 'red',
            bg = 'black'
        },
        right_sep = function()
            local val = {hl = {fg = 'NONE', bg = 'black'}}
            if b.gitsigns_status_dict then val.str = ' ' else val.str = '' end

            return val
        end
    }

    components.right.active[5] = {
        provider = 'line_percentage',
        hl = {
            style = 'bold'
        },
        left_sep = '  ',
        right_sep = ' '
    }

    components.left.inactive[1] = {
        provider = 'file_type',
        hl = {
            fg = 'white',
            bg = 'oceanblue',
            style = 'bold'
        },
        left_sep = {
            str = ' ',
            hl = {
                fg = 'NONE',
                bg = 'oceanblue'
            }
        },
        right_sep = {
            {
                str = ' ',
                hl = {
                    fg = 'NONE',
                    bg = 'oceanblue'
                }
            },
            'slant_right'
        }
    }

    -- This table is equal to the default colors table
    local colors = {
        black = '#1B1B1B',
        skyblue = '#50B0F0',
        cyan = '#009090',
        green = '#60A040',
        oceanblue = '#0066cc',
        magenta = '#C26BDB',
        orange = '#FF9000',
        red = '#D10000',
        violet = '#9E93E8',
        white = '#FFFFFF',
        yellow = '#E1E120'
    }

    -- This table is equal to the default separators table
    local separators = {
        vertical_bar = '┃',
        vertical_bar_thin = '│',
        left = '',
        right = '',
        block = '█',
        left_filled = '',
        right_filled = '',
        slant_left = '',
        slant_left_thin = '',
        slant_right = '',
        slant_right_thin = '',
        slant_left_2 = '',
        slant_left_2_thin = '',
        slant_right_2 = '',
        slant_right_2_thin = '',
        left_rounded = '',
        left_rounded_thin = '',
        right_rounded = '',
        right_rounded_thin = '',
        circle = '●'
    }

    -- This table is equal to the default vi_mode_colors table
    local vi_mode_colors = {
        NORMAL = 'green',
        OP = 'green',
        INSERT = 'red',
        VISUAL = 'skyblue',
        BLOCK = 'skyblue',
        REPLACE = 'violet',
        ['V-REPLACE'] = 'violet',
        ENTER = 'cyan',
        MORE = 'cyan',
        SELECT = 'orange',
        COMMAND = 'green',
        SHELL = 'green',
        TERM = 'green',
        NONE = 'yellow'
    }

    feline.setup({
        default_bg = '#1F1F23',
        default_fg = '#D0D0D0',
        colors = colors,
        separators = separators,
        components = components,
        properties = properties,
        vi_mode_colors = vi_mode_colors
    })
end

return _M
