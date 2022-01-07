-- See https://github.com/sayanarijit/xplr/wiki/Upgrade-Guide.

---@diagnostic disable
version = '0.17.0'
local xplr = xplr
---@diagnostic enable

local co = xplr.config
local genco = co.general

genco.enable_mouse = true
genco.show_hidden = true
genco.initial_layout = 'mine'
genco.initial_sorting = {
    { sorter = 'ByCanonicalIsDir', reverse = true },
    { sorter = 'ByIRelativePath', reverse = false },
}
genco.table.col_widths = {
    { Percentage = 78 },
    { Percentage = 9 },
    { Percentage = 13 },
}
genco.table.header.height = 0
genco.table.tree = {
    { format = '' },
    { format = '' },
    { format = '' },
}

genco.default_ui.prefix = ' '

genco.focus_ui.prefix = ' '
genco.focus_ui.suffix = ''
genco.focus_ui.style.add_modifiers = { 'Bold' }
genco.focus_ui.style.bg = 'Gray'

genco.selection_ui.prefix = '*'
genco.selection_ui.suffix = ''
genco.selection_ui.style.add_modifiers = nil
genco.selection_ui.style.bg = { Rgb = { 116, 177, 209 } }
genco.selection_ui.style.fg = 'White'

genco.focus_selection_ui.prefix = '*'
genco.focus_selection_ui.suffix = ''
genco.focus_selection_ui.style.add_modifiers = { 'Bold' }
genco.focus_selection_ui.style.bg = 'Gray'
genco.focus_selection_ui.style.fg = nil
genco.sort_and_filter_ui.default_identifier.style.add_modifiers = nil

local con = co.node_types
con.directory.meta.icon = ''
con.directory.style.add_modifiers = { 'Bold' }
con.directory.style.fg = 'Cyan'
con.file.meta.icon = ''
con.symlink.meta.icon = ''
con.symlink.style.add_modifiers = { 'Italic' }
con.symlink.style.fg = 'Magenta'

local mic = function(s)
    return { meta = { icon = s } }
end
con.mime_essence = {
    video = {
        ['*'] = mic(''),
    },
    audio = {
        ['*'] = mic(''),
    },
    image = {
        ['*'] = mic(''),
    },
}

con.extension = {
    fb2 = mic(''),
    vim = mic(''),
    sql = mic(''),
    db = mic(''),
    rc = mic(''),
    sh = mic(''),
    zsh = mic(''),
    js = mic(''),
    ts = mic(''),
    pub = mic(''),
    pgp = mic(''),
    gpg = mic(''),
    sig = mic(''),
    css = mic(''),
    scss = mic(''),
    py = mic(''),
    md = mic(''),
    json = mic(''),
    lua = mic(''),
    java = mic(''),
    jar = mic(''),
    class = mic(''),
    pdf = mic(''),
    zip = mic(''),
    rar = mic(''),
    bz2 = mic(''),
    cab = mic(''),
    deb = mic(''),
    gz = mic(''),
    gzip = mic(''),
    ['7z'] = mic(''),
    rpm = mic(''),
    tar = mic(''),
    xz = mic(''),
    zst = mic(''),
    lzma = mic(''),
    log = mic(''),
    doc = mic(''),
    docx = mic(''),
    rs = mic(''),
    rlib = mic(''),
    zwc = mic(''),
    ini = mic(''),
}

con.special = {
    Downloads = mic(''),
    Documents = mic(''),
    Desktop = mic(''),
    Music = mic(''),
    Pictures = mic(''),
    Videos = mic(''),
    tmp = mic(''),
    home = mic('ﮟ'),
    root = mic('ﮈ'),
    github = mic(''),
    ['dotfiles-reboot'] = mic(''),
    ['.zshrc'] = mic(''),
    ['.zsh'] = mic(''),
    ['.zprofile'] = mic(''),
    ['.zshenv'] = mic(''),
    ['.zsh_history'] = mic(''),
    ['.xinitrc'] = mic(''),
    ['.xprofile'] = mic(''),
    ['.profile'] = mic(''),
    ['.Xresources'] = mic(''),
    ['.bashrc'] = mic(''),
    ['.bash_profile'] = mic(''),
    ['.bash_logout'] = mic(''),
    ['.bash_history'] = mic(''),
    ['.toprc'] = mic(''),
    ['.config'] = mic(''),
    ['.git'] = mic(''),
    ['.gitignore'] = mic(''),
    ['.gitconfig'] = mic(''),
    ['.gitmodules'] = mic(''),
    ['.gitattributes'] = mic(''),
    ['.ssh'] = mic(''),
    ['.gnupg'] = mic(''),
    ['lost+found'] = mic(''),
    ['.npm'] = mic(''),
    ['node_modules'] = mic(''),
    ['.rustup'] = mic(''),
    ['.cargo'] = mic(''),
    ['Cargo.toml'] = mic(''),
    ['Cargo.lock'] = mic(''),
    ['.mozilla'] = mic(''),
    ['.java'] = mic(''),
    ['.cache'] = mic(''),
}

co.layouts.custom.mine = {
    Vertical = {
        config = { constraints = { { Percentage = 80 }, { Percentage = 20 } } },
        splits = {
            'Table',
            {
                Horizontal = {
                    config = { constraints = { { Percentage = 50 }, { Percentage = 50 } } },
                    splits = {
                        {
                            Vertical = {
                                config = { constraints = { { Percentage = 50 }, { Percentage = 50 } } },
                                splits = {
                                    {
                                        Horizontal = {
                                            config = { constraints = { { Percentage = 10 }, { Percentage = 90 } } },
                                            splits = {
                                                {
                                                    CustomContent = {
                                                        title = 'Ctx',
                                                        body = {
                                                            DynamicParagraph = {
                                                                render = 'custom.render_context_num',
                                                            },
                                                        },
                                                    },
                                                },
                                                'SortAndFilter',
                                            },
                                        },
                                    },
                                    'InputAndLogs',
                                },
                            },
                        },
                        'Selection',
                    },
                },
            },
        },
    },
}

local help_below = function()
    return {
        Vertical = {
            config = { constraints = { { Percentage = 45 }, { Percentage = 10 }, { Percentage = 45 } } },
            splits = { 'Table', 'SortAndFilter', 'HelpMenu' },
        },
    }
end

co.modes.builtin.default = {
    name = 'default',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['.'] = {
                help = 'show hidden',
                messages = {
                    { ToggleNodeFilter = { filter = 'RelativePathDoesNotStartWith', input = '.' } },
                    'ExplorePwdAsync',
                },
            },
            [':'] = {
                help = 'action',
                messages = { 'PopMode', { SwitchModeBuiltin = 'action' } },
            },
            ['?'] = {
                help = 'global help menu',
                messages = {
                    {
                        Call = {
                            command = 'zsh',
                            args = { '-c', [[cat -- "${XPLR_PIPE_GLOBAL_HELP_MENU_OUT}" | nvim -R]] },
                        },
                    },
                },
            },
            ['G'] = { help = 'go to bottom', messages = { 'PopMode', 'FocusLast' } },
            ['ctrl-a'] = { help = 'select/unselect all', messages = { 'ToggleSelectAll' } },
            ['/'] = {
                help = 'search',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'search' },
                    { SetInputBuffer = '' },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-i'] = { help = 'next visited path', messages = { 'NextVisitedPath' } },
            ['ctrl-o'] = { help = 'last visited path', messages = { 'LastVisitedPath' } },
            ['ctrl-r'] = { help = 'refresh screen', messages = { 'ClearScreen' } },
            ['ctrl-u'] = { help = 'clear selection', messages = { 'ClearSelection' } },
            ['ctrl-w'] = { help = 'switch layout', messages = { { SwitchModeBuiltin = 'switch_layout' } } },
            ['d'] = { help = 'delete', messages = { 'PopMode', { SwitchModeBuiltin = 'delete' } } },
            down = { help = 'down', messages = { 'FocusNext' } },
            ['f3'] = { help = 'preview', messages = { { CallLuaSilently = 'custom.preview_tui' } } },
            ['f4'] = { help = 'edit file', messages = { { CallLuaSilently = 'custom.edit_file' } } },
            ['f7'] = {
                help = 'create directory',
                messages = { 'PopMode', { SwitchModeBuiltin = 'create directory' }, { SetInputBuffer = '' } },
            },
            ['s'] = { help = 'open shell', messages = { { CallLuaSilently = 'custom.open_shell' } } },
            enter = { help = 'enter', messages = { { CallLuaSilently = 'custom.opener' } } },
            esc = { help = nil, messages = {} },
            ['f'] = {
                help = 'filter',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'filter' },
                },
            },
            ['g'] = { help = 'go to', messages = { 'PopMode', { SwitchModeBuiltin = 'go_to' } } },
            left = { help = 'back', messages = { 'Back' } },
            ['q'] = { help = 'quit', messages = { { CallLuaSilently = 'custom.quit' }, 'Quit' } },
            ['r'] = {
                help = 'rename',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'rename' },
                    {
                        BashExecSilently = [===[
            echo SetInputBuffer: "'"$(basename "${XPLR_FOCUS_PATH}")"'" >> "${XPLR_PIPE_MSG_IN:?}"
            ]===],
                    },
                },
            },
            ['S'] = { help = 'sort', messages = { 'PopMode', { SwitchModeBuiltin = 'sort' } } },
            space = { help = 'toggle selection', messages = { 'ToggleSelection', 'FocusNext' } },
            up = { help = 'up', messages = { 'FocusPrevious' } },
            ['~'] = {
                help = 'go home',
                messages = {
                    {
                        BashExecSilently = [===[
            echo ChangeDirectory: "'"${HOME:?}"'" >> "${XPLR_PIPE_MSG_IN:?}"
            ]===],
                    },
                },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
}

co.modes.builtin.default.key_bindings.on_key['tab'] = co.modes.builtin.default.key_bindings.on_key['ctrl-i']
co.modes.builtin.default.key_bindings.on_key['v'] = co.modes.builtin.default.key_bindings.on_key.space
co.modes.builtin.default.key_bindings.on_key['V'] = co.modes.builtin.default.key_bindings.on_key['ctrl-a']
co.modes.builtin.default.key_bindings.on_key.right = co.modes.builtin.default.key_bindings.on_key.enter

co.modes.builtin.selection_ops = {
    name = 'selection ops',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['c'] = {
                help = 'copy here',
                messages = {
                    {
                        BashExec = [===[
            (while IFS= read -r line; do
            if cp -vr -- "${line:?}" ./; then
              echo LogSuccess: $line copied to $PWD >> "${XPLR_PIPE_MSG_IN:?}"
            else
              echo LogError: Failed to copy $line to $PWD >> "${XPLR_PIPE_MSG_IN:?}"
            fi
            done < "${XPLR_PIPE_SELECTION_OUT:?}")
            echo ExplorePwdAsync >> "${XPLR_PIPE_MSG_IN:?}"
            echo ClearSelection >> "${XPLR_PIPE_MSG_IN:?}"
            read -p "[enter to continue]"
            ]===],
                    },
                    'PopMode',
                },
            },
            esc = { help = 'cancel', messages = { 'PopMode' } },
            ['m'] = {
                help = 'move here',
                messages = {
                    {
                        BashExec = [===[
            (while IFS= read -r line; do
            if mv -v -- "${line:?}" ./; then
              echo LogSuccess: $line moved to $PWD >> "${XPLR_PIPE_MSG_IN:?}"
            else
              echo LogError: Failed to move $line to $PWD >> "${XPLR_PIPE_MSG_IN:?}"
            fi
            done < "${XPLR_PIPE_SELECTION_OUT:?}")
            echo ExplorePwdAsync >> "${XPLR_PIPE_MSG_IN:?}"
            read -p "[enter to continue]"
            ]===],
                    },
                    'PopMode',
                },
            },
            ['x'] = {
                help = 'open in gui',
                messages = {
                    {
                        BashExecSilently = [===[
            if [ -z "$OPENER" ]; then
              if command -v xdg-open; then
                OPENER=xdg-open
                elif command -v open; then
                OPENER=open
              else
                echo 'LogError: $OPENER not found' >> "${XPLR_PIPE_MSG_IN:?}"
                exit 1
              fi
            fi
            (while IFS= read -r line; do
            $OPENER "${line:?}" > /dev/null 2>&1
            done < "${XPLR_PIPE_RESULT_OUT:?}")
            ]===],
                    },
                    'ClearScreen',
                    'PopMode',
                },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
}

co.modes.builtin.create = {
    name = 'create',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            esc = { help = 'cancel', messages = { 'PopMode' } },
            ['d'] = {
                help = 'create directory',
                messages = { 'PopMode', { SwitchModeBuiltin = 'create directory' }, { SetInputBuffer = '' } },
            },
            ['f'] = {
                help = 'create file',
                messages = { 'PopMode', { SwitchModeBuiltin = 'create file' }, { SetInputBuffer = '' } },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
}

local create_thing = function(opName, opCmd)
    return {
        name = opName,
        help = nil,
        extra_help = nil,
        key_bindings = {
            on_key = {
                backspace = { help = 'remove last character', messages = { 'RemoveInputBufferLastCharacter' } },
                ['ctrl-u'] = { help = 'remove line', messages = { { SetInputBuffer = '' } } },
                ['ctrl-w'] = { help = 'remove last word', messages = { 'RemoveInputBufferLastWord' } },
                enter = {
                    help = opName,
                    messages = {
                        {
                            BashExecSilently = [===[
            PTH="$XPLR_INPUT_BUFFER"
            if [ "${PTH}" ]; then
                            ]===] .. opCmd .. [===[ -- "${PTH:?}" \
              && echo "SetInputBuffer: ''" >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo LogSuccess: $PTH created >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo ExplorePwd >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo FocusByFileName: "'"$PTH"'" >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo PopMode >> "${XPLR_PIPE_MSG_IN:?}"
            else
              echo PopMode >> "${XPLR_PIPE_MSG_IN:?}"
            fi
            ]===],
                        },
                    },
                },
                esc = { help = 'cancel', messages = { 'PopMode' } },
            },
            on_alphabet = nil,
            on_number = nil,
            on_special_character = nil,
            default = { help = nil, messages = { 'BufferInputFromKey' } },
        },
    }
end

co.modes.builtin.create_directory = create_thing('create directory', 'mkdir -p')
co.modes.builtin.create_file = create_thing('create file', 'touch')

co.modes.builtin.go_to = {
    name = 'go to',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            esc = { help = 'cancel', messages = { 'PopMode' } },
            ['f'] = { help = 'follow symlink', messages = { 'FollowSymlink', 'PopMode' } },
            ['g'] = { help = 'top', messages = { 'FocusFirst', 'PopMode' } },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
}

co.modes.builtin.rename = {
    name = 'rename',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = { help = 'remove last character', messages = { 'RemoveInputBufferLastCharacter' } },
            ['ctrl-u'] = { help = 'remove line', messages = { { SetInputBuffer = '' } } },
            ['ctrl-w'] = { help = 'remove last word', messages = { 'RemoveInputBufferLastWord' } },
            enter = {
                help = 'rename',
                messages = {
                    {
                        BashExecSilently = [===[
            SRC="${XPLR_FOCUS_PATH:?}"
            TARGET="${XPLR_INPUT_BUFFER:?}"
            mv -- "${SRC:?}" "${TARGET:?}" \
              && echo ExplorePwd >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo FocusByFileName: "'"$TARGET"'" >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo LogSuccess: $SRC renamed to $TARGET >> "${XPLR_PIPE_MSG_IN:?}"
            ]===],
                    },
                    'PopMode',
                },
            },
            esc = { help = 'cancel', messages = { 'PopMode' } },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = { help = nil, messages = { 'BufferInputFromKey' } },
    },
}

co.modes.builtin.delete = {
    name = 'delete',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['d'] = {
                help = 'force delete',
                messages = {
                    {
                        BashExec = [===[
            (while IFS= read -r line; do
            if rm -rf -- "${line:?}"; then
              echo LogSuccess: $line deleted >> "${XPLR_PIPE_MSG_IN:?}"
            else
              echo LogError: Failed to delete $line >> "${XPLR_PIPE_MSG_IN:?}"
            fi
            done < "${XPLR_PIPE_RESULT_OUT:?}")
            echo ExplorePwdAsync >> "${XPLR_PIPE_MSG_IN:?}"
            ]===],
                    },
                    'PopMode',
                },
            },
            esc = { help = 'cancel', messages = { 'PopMode' } },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
}

co.modes.builtin.action = {
    name = 'action to',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['!'] = {
                help = 'shell',
                messages = {
                    { Call = { command = 'zsh', args = { '-i' } } },
                    'ExplorePwdAsync',
                    'PopMode',
                },
            },
            ['c'] = { help = 'create', messages = { 'PopMode', { SwitchModeBuiltin = 'create' } } },
            esc = { help = 'cancel', messages = { 'PopMode' } },
            ['l'] = {
                help = 'logs',
                messages = {
                    { Call = { command = 'zsh', args = { '-c', [[cat -- "${XPLR_PIPE_LOGS_OUT}" | nvim -R]] } } },
                    'PopMode',
                },
            },
            ['s'] = {
                help = 'selection operations',
                messages = { 'PopMode', { SwitchModeBuiltin = 'selection_ops' } },
            },
            ['q'] = { help = 'quit options', messages = { 'PopMode', { SwitchModeBuiltin = 'quit' } } },
            ['m'] = {
                help = 'show mimetype',
                messages = {
                    {
                        BashExecSilently = [===[
                        echo LogSuccess: Mime=$(file -bLi "${XPLR_FOCUS_PATH:?}") >> "${XPLR_PIPE_MSG_IN:?}"
                        ]===],
                    },
                    'PopMode',
                },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
}

co.modes.builtin.quit = {
    name = 'quit',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            q = { help = 'quit', messages = { { CallLuaSilently = 'custom.quit' }, 'Quit' } },
            p = { help = 'quit printing pwd', messages = { { CallLuaSilently = 'custom.quit' }, 'PrintPwdAndQuit' } },
            f = {
                help = 'quit printing focus',
                messages = { { CallLuaSilently = 'custom.quit' }, 'PrintFocusPathAndQuit' },
            },
            s = {
                help = 'quit printing selection',
                messages = { { CallLuaSilently = 'custom.quit' }, 'PrintSelectionAndQuit' },
            },
            r = {
                help = 'quit printing result',
                messages = { { CallLuaSilently = 'custom.quit' }, 'PrintResultAndQuit' },
            },
            esc = { help = 'cancel', messages = { 'PopMode' } },
            ['ctrl-c'] = { help = 'terminate', messages = { { CallLuaSilently = 'custom.quit' }, 'Terminate' } },
            ['#'] = {
                help = 'quit printing app state',
                messages = { { CallLuaSilently = 'custom.quit' }, 'PrintAppStateAndQuit' },
            },
        },
    },
    layout = help_below(),
}

co.modes.builtin.search = {
    name = 'search',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'RemoveInputBufferLastCharacter',
                    { AddNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-u'] = {
                help = 'remove line',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    { SetInputBuffer = '' },
                    { AddNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-w'] = {
                help = 'remove last word',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'RemoveInputBufferLastWord',
                    { AddNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'ExplorePwdAsync',
                },
            },
            down = { help = 'down', messages = { 'FocusNext' } },
            enter = {
                help = 'focus',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'PopMode',
                    'ExplorePwdAsync',
                },
            },
            left = {
                help = 'back',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'Back',
                    { SetInputBuffer = '' },
                    'ExplorePwdAsync',
                },
            },
            right = {
                help = 'enter',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'Enter',
                    { SetInputBuffer = '' },
                    'ExplorePwdAsync',
                },
            },
            tab = { help = 'toggle selection', messages = { 'ToggleSelection', 'FocusNext' } },
            up = { help = 'up', messages = { 'FocusPrevious' } },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = {
            help = nil,
            messages = {
                { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                'BufferInputFromKey',
                { AddNodeFilterFromInput = 'IRelativePathDoesContain' },
                'ExplorePwdAsync',
            },
        },
    },
}

co.modes.builtin.search.key_bindings.on_key['esc'] = co.modes.builtin.search.key_bindings.on_key.enter
co.modes.builtin.search.key_bindings.on_key['ctrl-n'] = co.modes.builtin.search.key_bindings.on_key.down
co.modes.builtin.search.key_bindings.on_key['ctrl-p'] = co.modes.builtin.search.key_bindings.on_key.up

co.modes.builtin.filter = {
    name = 'filter',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['R'] = {
                help = 'relative does not contain',
                messages = {
                    { SwitchModeBuiltin = 'relative_path_does_not_contain' },
                    { SetInputBuffer = '' },
                    { AddNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                    'ExplorePwdAsync',
                },
            },
            backspace = { help = 'remove last filter', messages = { 'RemoveLastNodeFilter', 'ExplorePwdAsync' } },
            ['ctrl-r'] = { help = 'reset filters', messages = { 'ResetNodeFilters', 'ExplorePwdAsync' } },
            ['ctrl-u'] = { help = 'clear filters', messages = { 'ClearNodeFilters', 'ExplorePwdAsync' } },
            enter = { help = 'done', messages = { 'PopMode' } },
            ['r'] = {
                help = 'relative does contain',
                messages = {
                    { SwitchModeBuiltin = 'relative_path_does_contain' },
                    { SetInputBuffer = '' },
                    { AddNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'ExplorePwdAsync',
                },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
}

co.modes.builtin.filter.key_bindings.on_key['esc'] = co.modes.builtin.filter.key_bindings.on_key.enter

co.modes.builtin.relative_path_does_contain = {
    name = 'relative path does contain',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'RemoveInputBufferLastCharacter',
                    { AddNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-u'] = {
                help = 'remove line',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    { SetInputBuffer = '' },
                    { AddNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-w'] = {
                help = 'remove last word',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'RemoveInputBufferLastWord',
                    { AddNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'ExplorePwdAsync',
                },
            },
            enter = { help = 'apply filter', messages = { 'PopMode' } },
            esc = {
                help = 'cancel',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                    'PopMode',
                    'ExplorePwdAsync',
                },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = {
            help = nil,
            messages = {
                { RemoveNodeFilterFromInput = 'IRelativePathDoesContain' },
                'BufferInputFromKey',
                { AddNodeFilterFromInput = 'IRelativePathDoesContain' },
                'ExplorePwdAsync',
            },
        },
    },
}

co.modes.builtin.relative_path_does_not_contain = {
    name = 'relative path does not contain',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                    'RemoveInputBufferLastCharacter',
                    { AddNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-u'] = {
                help = 'remove line',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                    { SetInputBuffer = '' },
                    { AddNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-w'] = {
                help = 'remove last word',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                    'RemoveInputBufferLastWord',
                    { AddNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                    'ExplorePwdAsync',
                },
            },
            enter = { help = 'apply filter', messages = { 'PopMode' } },
            esc = {
                help = 'cancel',
                messages = {
                    { RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                    'PopMode',
                    'ExplorePwdAsync',
                },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = {
            help = nil,
            messages = {
                { RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                'BufferInputFromKey',
                { AddNodeFilterFromInput = 'IRelativePathDoesNotContain' },
                'ExplorePwdAsync',
            },
        },
    },
}

co.modes.builtin.sort = {
    name = 'sort',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['!'] = { help = 'reverse sorters', messages = { 'ReverseNodeSorters', 'ExplorePwdAsync' } },
            ['E'] = {
                help = 'by canonical extension reverse',
                messages = {
                    { AddNodeSorter = { sorter = 'ByCanonicalExtension', reverse = true } },
                    'ExplorePwdAsync',
                },
            },
            ['M'] = {
                help = 'by canonical mime essence reverse',
                messages = {
                    { AddNodeSorter = { sorter = 'ByCanonicalMimeEssence', reverse = true } },
                    'ExplorePwdAsync',
                },
            },
            ['N'] = {
                help = 'by node type reverse',
                messages = {
                    { AddNodeSorter = { sorter = 'ByCanonicalIsDir', reverse = true } },
                    { AddNodeSorter = { sorter = 'ByCanonicalIsFile', reverse = true } },
                    { AddNodeSorter = { sorter = 'ByIsSymlink', reverse = true } },
                    'ExplorePwdAsync',
                },
            },
            ['R'] = {
                help = 'by relative path reverse',
                messages = {
                    { AddNodeSorter = { sorter = 'ByIRelativePath', reverse = true } },
                    'ExplorePwdAsync',
                },
            },
            ['S'] = {
                help = 'by size reverse',
                messages = {
                    { AddNodeSorter = { sorter = 'BySize', reverse = true } },
                    'ExplorePwdAsync',
                },
            },
            backspace = { help = 'remove last sorter', messages = { 'RemoveLastNodeSorter', 'ExplorePwdAsync' } },
            ['ctrl-r'] = { help = 'reset sorters', messages = { 'ResetNodeSorters', 'ExplorePwdAsync' } },
            ['ctrl-u'] = { help = 'clear sorters', messages = { 'ClearNodeSorters', 'ExplorePwdAsync' } },
            ['e'] = {
                help = 'by canonical extension',
                messages = {
                    { AddNodeSorter = { sorter = 'ByCanonicalExtension', reverse = false } },
                    'ExplorePwdAsync',
                },
            },
            enter = { help = 'done', messages = { 'PopMode' } },
            ['m'] = {
                help = 'by canonical mime essence',
                messages = {
                    { AddNodeSorter = { sorter = 'ByCanonicalMimeEssence', reverse = false } },
                    'ExplorePwdAsync',
                },
            },
            ['n'] = {
                help = 'by node type',
                messages = {
                    { AddNodeSorter = { sorter = 'ByCanonicalIsDir', reverse = false } },
                    { AddNodeSorter = { sorter = 'ByCanonicalIsFile', reverse = false } },
                    { AddNodeSorter = { sorter = 'ByIsSymlink', reverse = false } },
                    'ExplorePwdAsync',
                },
            },
            ['r'] = {
                help = 'by relative path',
                messages = {
                    { AddNodeSorter = { sorter = 'ByIRelativePath', reverse = false } },
                    'ExplorePwdAsync',
                },
            },
            ['s'] = {
                help = 'by size',
                messages = {
                    { AddNodeSorter = { sorter = 'BySize', reverse = false } },
                    'ExplorePwdAsync',
                },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
    layout = help_below(),
}

co.modes.builtin.sort.key_bindings.on_key['esc'] = co.modes.builtin.sort.key_bindings.on_key.enter

co.modes.builtin.switch_layout = {
    name = 'switch layout',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['1'] = { help = 'mine', messages = { { SwitchLayoutCustom = 'mine' }, 'PopMode' } },
            ['2'] = { help = 'default', messages = { { SwitchLayoutBuiltin = 'default' }, 'PopMode' } },
            esc = { help = 'cancel', messages = { 'PopMode' } },
        },
    },
}

co.modes.custom = {}

-- Format path column
xplr.fn.builtin.fmt_general_table_row_cols_0 = function(m)
    local r = m.tree .. m.prefix

    if m.meta.icon == nil then
        r = r .. ''
    else
        r = r .. m.meta.icon .. ' '
    end

    r = r .. m.relative_path

    if m.is_dir then
        r = r .. '/'
    end

    r = r .. m.suffix .. ' '

    if m.is_symlink then
        r = r .. '-> '

        if m.is_broken then
            r = r .. '×'
        else
            r = r .. m.symlink.absolute_path

            if m.symlink.is_dir then
                r = r .. '/'
            end
        end
    end

    return r
end

-- Format permissions column
xplr.fn.builtin.fmt_general_table_row_cols_1 = function(m)
    local function num(x, y, z)
        return 4 * (x and 1 or 0) + 2 * (y and 1 or 0) + (z and 1 or 0)
    end

    local p = m.permissions
    local r = ''
    r = r .. num(p.setuid, p.setgid, p.sticky)
    r = r .. num(p.user_read, p.user_write, p.user_execute)
    r = r .. num(p.group_read, p.group_write, p.group_execute)
    r = r .. num(p.other_read, p.other_write, p.other_execute)
    return r
end

-- Format size column
xplr.fn.builtin.fmt_general_table_row_cols_2 = function(m)
    return m.is_dir and '' or m.human_size
end

xplr.fn.custom.quit = function(_)
    return {
        'StopFifo',
        {
            Call = {
                command = 'kitty',
                args = {
                    '@close-window',
                    '--match=title:PreviewTUI',
                },
            },
        },
    }
end

xplr.fn.custom.open_shell = function(a)
    return {
        {
            Call = {
                command = 'kitty',
                args = {
                    '@launch',
                    '--type=tab',
                    '--no-response',
                    '--location=after',
                    '--cwd=' .. a.pwd,
                },
            },
        },
    }
end

local preview_tui_enabled = false
xplr.fn.custom.preview_tui = function(_)
    if preview_tui_enabled then
        preview_tui_enabled = false
        return { { CallLuaSilently = 'custom.quit' } }
    else
        local preview_tui_fifo = '/tmp/preview-tui.fifo' .. os.time()
        os.execute('[ ! -p \'' .. preview_tui_fifo .. '\' ] && mkfifo \'' .. preview_tui_fifo .. '\'')
        preview_tui_enabled = true
        return {
            {
                CallSilently = {
                    command = 'kitty',
                    args = {
                        '@launch',
                        '--type=window',
                        '--no-response',
                        '--location=vsplit',
                        '--keep-focus',
                        '--title=PreviewTUI',
                        '--env',
                        'PREVIEW_TUI_FIFO=' .. preview_tui_fifo,
                        os.getenv('HOME') .. '/bin/preview-tui',
                    },
                },
            },
            { StartFifo = preview_tui_fifo },
        }
    end
end

xplr.fn.custom.edit_file = function(a)
    local res = xplr.fn.custom.open_shell(a)
    table.insert(res[1].Call.args, 'nvim')
    table.insert(res[1].Call.args, a.focused_node and a.focused_node.canonical.absolute_path or a.pwd)
    return res
end

xplr.fn.custom.opener = function(a)
    local c = a.focused_node.canonical
    return c.is_dir and { 'Enter' }
        or { { CallSilently = { command = os.getenv('HOME') .. '/bin/run-tui', args = { c.absolute_path } } } }
end

package.path = os.getenv('HOME') .. '/.config/xplr/plugins/?/src/init.lua'

require('xclip').setup({
    copy_command = 'xclip-copyfile',
    copy_paths_command = 'xclip -sel clip',
    paste_command = 'xclip-pastefile',
    keep_selection = false,
})

local term = require('term')
local k_hsplit = term.profile_kitty_hsplit()
k_hsplit.key = 'ctrl-h'
term.setup({ term.profile_kitty_vsplit(), k_hsplit })

local csw = require('context-switch')
csw.setup()
xplr.fn.custom.render_context_num = function(_)
    return tostring(csw.get_current_context_num())
end
