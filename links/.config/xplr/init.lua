-- See https://github.com/sayanarijit/xplr/wiki/Upgrade-Guide.

---@diagnostic disable
version = '0.16.0'
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
    { Percentage = 73 },
    { Percentage = 4 },
    { Percentage = 8 },
    { Percentage = 15 },
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

local function mic(s)
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
    rc = mic(''),
    sh = mic(''),
    zsh = mic(''),
    js = mic(''),
    ts = mic(''),
    pub = mic(''),
    pgp = mic(''),
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
    rpm = mic(''),
    tar = mic(''),
    xz = mic(''),
    zst = mic(''),
    log = mic(''),
    doc = mic(''),
    docx = mic(''),
}

con.special = {
    Downloads = mic(''),
    Documents = mic(''),
    Desktop = mic(''),
}
con.special['.git'] = mic('')
con.special['.ssh'] = mic('')
con.special['lost+found'] = mic('')
con.special['.npm'] = mic('')
con.special['node_modules'] = mic('')
con.special['package.json'] = mic('')
con.special['package-lock.json'] = mic('')
con.special['7z'] = mic('')

co.layouts.custom.mine = {
    Vertical = {
        config = {
            constraints = {
                { Percentage = 80 },
                { Percentage = 20 },
            },
        },
        splits = {
            'Table',
            {
                Horizontal = {
                    config = {
                        constraints = {
                            { Percentage = 50 },
                            { Percentage = 50 },
                        },
                    },
                    splits = {
                        {
                            Vertical = {
                                config = {
                                    constraints = {
                                        { Percentage = 50 },
                                        { Percentage = 50 },
                                    },
                                },
                                splits = {
                                    {
                                        Horizontal = {
                                            config = {
                                                constraints = {
                                                    { Percentage = 10 },
                                                    { Percentage = 90 },
                                                },
                                            },
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

co.modes.builtin.default = {
    name = 'default',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['#'] = {
                help = nil,
                messages = { 'PrintAppStateAndQuit' },
            },
            ['.'] = {
                help = 'show hidden',
                messages = {
                    { ToggleNodeFilter = { filter = 'RelativePathDoesNotStartWith', input = '.' } },
                    'ExplorePwdAsync',
                },
            },
            [':'] = {
                help = 'action',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'action' },
                },
            },
            ['?'] = {
                help = 'global help menu',
                messages = {
                    {
                        BashExec = [===[
            [ -z "$PAGER" ] && PAGER="less -+F"
            cat -- "${XPLR_PIPE_GLOBAL_HELP_MENU_OUT}" | ${PAGER:?}
            ]===],
                    },
                },
            },
            ['G'] = {
                help = 'go to bottom',
                messages = { 'PopMode', 'FocusLast' },
            },
            ['ctrl-a'] = {
                help = 'select/unselect all',
                messages = { 'ToggleSelectAll' },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['/'] = {
                help = 'search',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'search' },
                    { SetInputBuffer = '' },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-i'] = {
                help = 'next visited path',
                messages = { 'NextVisitedPath' },
            },
            ['ctrl-o'] = {
                help = 'last visited path',
                messages = { 'LastVisitedPath' },
            },
            ['ctrl-r'] = {
                help = 'refresh screen',
                messages = { 'ClearScreen' },
            },
            ['ctrl-u'] = {
                help = 'clear selection',
                messages = { 'ClearSelection' },
            },
            ['ctrl-w'] = {
                help = 'switch layout',
                messages = {
                    { SwitchModeBuiltin = 'switch_layout' },
                },
            },
            ['d'] = {
                help = 'delete',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'delete' },
                },
            },
            down = {
                help = 'down',
                messages = { 'FocusNext' },
            },
            ['f4'] = {
                help = 'edit file',
                messages = { { CallLuaSilently = 'custom.edit_file' } },
            },
            enter = {
                help = 'enter',
                messages = { { CallLuaSilently = 'custom.opener' } },
            },
            esc = {
                help = nil,
                messages = {},
            },
            ['f'] = {
                help = 'filter',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'filter' },
                },
            },
            ['g'] = {
                help = 'go to',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'go_to' },
                },
            },
            left = {
                help = 'back',
                messages = { 'Back' },
            },
            ['q'] = {
                help = 'quit',
                messages = { 'Quit' },
            },
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
            right = {
                help = 'enter',
                messages = { { CallLuaSilently = 'custom.opener' } },
            },
            ['s'] = {
                help = 'sort',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'sort' },
                },
            },
            space = {
                help = 'toggle selection',
                messages = { 'ToggleSelection', 'FocusNext' },
            },
            up = {
                help = 'up',
                messages = { 'FocusPrevious' },
            },
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
        on_number = {
            help = 'input',
            messages = {
                'PopMode',
                { SwitchModeBuiltin = 'number' },
                'BufferInputFromKey',
            },
        },
        on_special_character = nil,
        default = nil,
    },
}

co.modes.builtin.default.key_bindings.on_key['tab'] = co.modes.builtin.default.key_bindings.on_key['ctrl-i']

co.modes.builtin.default.key_bindings.on_key['v'] = co.modes.builtin.default.key_bindings.on_key.space

co.modes.builtin.default.key_bindings.on_key['V'] = co.modes.builtin.default.key_bindings.on_key['ctrl-a']

co.modes.builtin.default.key_bindings.on_key['h'] = co.modes.builtin.default.key_bindings.on_key.left

co.modes.builtin.default.key_bindings.on_key['j'] = co.modes.builtin.default.key_bindings.on_key.down

co.modes.builtin.default.key_bindings.on_key['k'] = co.modes.builtin.default.key_bindings.on_key.up

co.modes.builtin.default.key_bindings.on_key['l'] = co.modes.builtin.default.key_bindings.on_key.right

------ Selection ops
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
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
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

------ Create
co.modes.builtin.create = {
    name = 'create',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['d'] = {
                help = 'create directory',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'create directory' },
                    { SetInputBuffer = '' },
                },
            },
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
            ['f'] = {
                help = 'create file',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'create file' },
                    { SetInputBuffer = '' },
                },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
}

------ Create directory
co.modes.builtin.create_directory = {
    name = 'create directory',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = { 'RemoveInputBufferLastCharacter' },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['ctrl-u'] = {
                help = 'remove line',
                messages = {
                    {
                        SetInputBuffer = '',
                    },
                },
            },
            ['ctrl-w'] = {
                help = 'remove last word',
                messages = { 'RemoveInputBufferLastWord' },
            },
            enter = {
                help = 'create directory',
                messages = {
                    {
                        BashExecSilently = [===[
            PTH="$XPLR_INPUT_BUFFER"
            if [ "${PTH}" ]; then
              mkdir -p -- "${PTH:?}" \
              && echo "SetInputBuffer: ''" >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo ExplorePwd >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo LogSuccess: $PTH created >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo FocusByFileName: "'"$PTH"'" >> "${XPLR_PIPE_MSG_IN:?}"
            else
              echo PopMode >> "${XPLR_PIPE_MSG_IN:?}"
            fi
            ]===],
                    },
                },
            },
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = {
            help = nil,
            messages = { 'BufferInputFromKey' },
        },
    },
}

------ Create file
co.modes.builtin.create_file = {
    name = 'create file',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = { 'RemoveInputBufferLastCharacter' },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['ctrl-u'] = {
                help = 'remove line',
                messages = {
                    {
                        SetInputBuffer = '',
                    },
                },
            },
            ['ctrl-w'] = {
                help = 'remove last word',
                messages = { 'RemoveInputBufferLastWord' },
            },
            enter = {
                help = 'create file',
                messages = {
                    {
                        BashExecSilently = [===[
            PTH="$XPLR_INPUT_BUFFER"
            if [ "${PTH}" ]; then
              touch -- "${PTH:?}" \
              && echo "SetInputBuffer: ''" >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo LogSuccess: $PTH created >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo ExplorePwd >> "${XPLR_PIPE_MSG_IN:?}" \
              && echo FocusByFileName: "'"$PTH"'" >> "${XPLR_PIPE_MSG_IN:?}"
            else
              echo PopMode >> "${XPLR_PIPE_MSG_IN:?}"
            fi
            ]===],
                    },
                },
            },
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = {
            help = nil,
            messages = { 'BufferInputFromKey' },
        },
    },
}

------ Number
co.modes.builtin.number = {
    name = 'number',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = { 'RemoveInputBufferLastCharacter' },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['ctrl-u'] = {
                help = 'remove line',
                messages = {
                    {
                        SetInputBuffer = '',
                    },
                },
            },
            ['ctrl-w'] = {
                help = 'remove last word',
                messages = { 'RemoveInputBufferLastWord' },
            },
            down = {
                help = 'to down',
                messages = { 'FocusNextByRelativeIndexFromInput', 'PopMode' },
            },
            enter = {
                help = 'to index',
                messages = { 'FocusByIndexFromInput', 'PopMode' },
            },
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
            up = {
                help = 'to up',
                messages = { 'FocusPreviousByRelativeIndexFromInput', 'PopMode' },
            },
        },
        on_alphabet = nil,
        on_number = {
            help = 'input',
            messages = { 'BufferInputFromKey' },
        },
        on_special_character = nil,
        default = nil,
    },
}

co.modes.builtin.number.key_bindings.on_key['j'] = co.modes.builtin.number.key_bindings.on_key.down
co.modes.builtin.number.key_bindings.on_key['k'] = co.modes.builtin.number.key_bindings.on_key.up

------ Go to
co.modes.builtin.go_to = {
    name = 'go to',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
            ['f'] = {
                help = 'follow symlink',
                messages = { 'FollowSymlink', 'PopMode' },
            },
            ['g'] = {
                help = 'top',
                messages = { 'FocusFirst', 'PopMode' },
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
            $OPENER "${XPLR_FOCUS_PATH:?}" > /dev/null 2>&1
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

------ Rename
co.modes.builtin.rename = {
    name = 'rename',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = { 'RemoveInputBufferLastCharacter' },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['ctrl-u'] = {
                help = 'remove line',
                messages = {
                    {
                        SetInputBuffer = '',
                    },
                },
            },
            ['ctrl-w'] = {
                help = 'remove last word',
                messages = { 'RemoveInputBufferLastWord' },
            },
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
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = {
            help = nil,
            messages = { 'BufferInputFromKey' },
        },
    },
}

------ Delete
co.modes.builtin.delete = {
    name = 'delete',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['D'] = {
                help = 'force delete',
                messages = {
                    {
                        BashExec = [===[
            (while IFS= read -r line; do
            if rm -rfv -- "${line:?}"; then
              echo LogSuccess: $line deleted >> "${XPLR_PIPE_MSG_IN:?}"
            else
              echo LogError: Failed to delete $line >> "${XPLR_PIPE_MSG_IN:?}"
            fi
            done < "${XPLR_PIPE_RESULT_OUT:?}")
            echo ExplorePwdAsync >> "${XPLR_PIPE_MSG_IN:?}"
            read -p "[enter to continue]"
            ]===],
                    },
                    'PopMode',
                },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['d'] = {
                help = 'delete',
                messages = {
                    {
                        BashExec = [===[
            (while IFS= read -r line; do
            if [ -d "$line" ] && [ ! -L "$line" ]; then
              if rmdir -v -- "${line:?}"; then
                echo LogSuccess: $line deleted >> "${XPLR_PIPE_MSG_IN:?}"
              else
                echo LogError: Failed to delete $line >> "${XPLR_PIPE_MSG_IN:?}"
              fi
            else
              if rm -v -- "${line:?}"; then
                echo LogSuccess: $line deleted >> "${XPLR_PIPE_MSG_IN:?}"
              else
                echo LogError: Failed to delete $line >> "${XPLR_PIPE_MSG_IN:?}"
              fi
            fi
            done < "${XPLR_PIPE_RESULT_OUT:?}")
            echo ExplorePwdAsync >> "${XPLR_PIPE_MSG_IN:?}"
            read -p "[enter to continue]"
            ]===],
                    },
                    'PopMode',
                },
            },
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = nil,
    },
}

------ Action
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
            ['c'] = {
                help = 'create',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'create' },
                },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['e'] = {
                help = 'open in editor',
                messages = {
                    {
                        BashExec = [===[
            ${EDITOR:-vi} "${XPLR_FOCUS_PATH:?}"
            ]===],
                    },
                    'PopMode',
                },
            },
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
            ['l'] = {
                help = 'logs',
                messages = {
                    {
                        BashExec = [===[
            [ -z "$PAGER" ] && PAGER="less -+F"
            cat -- "${XPLR_PIPE_LOGS_OUT}" | ${PAGER:?}
            ]===],
                    },
                    'PopMode',
                },
            },
            ['s'] = {
                help = 'selection operations',
                messages = {
                    'PopMode',
                    {
                        SwitchModeBuiltin = 'selection_ops',
                    },
                },
            },
            ['q'] = {
                help = 'quit options',
                messages = {
                    'PopMode',
                    { SwitchModeBuiltin = 'quit' },
                },
            },
        },
        on_alphabet = nil,
        on_number = {
            help = 'go to index',
            messages = {
                'PopMode',
                {
                    SwitchModeBuiltin = 'number',
                },
                'BufferInputFromKey',
            },
        },
        on_special_character = nil,
        default = nil,
    },
}

------ Quit
co.modes.builtin.quit = {
    name = 'quit',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            enter = {
                help = 'just quit',
                messages = {
                    'Quit',
                },
            },
            p = {
                help = 'quit printing pwd',
                messages = {
                    'PrintPwdAndQuit',
                },
            },
            f = {
                help = 'quit printing focus',
                messages = {
                    'PrintFocusPathAndQuit',
                },
            },
            s = {
                help = 'quit printing selection',
                messages = {
                    'PrintSelectionAndQuit',
                },
            },
            r = {
                help = 'quit printing result',
                messages = {
                    'PrintResultAndQuit',
                },
            },
            esc = {
                help = 'cancel',
                messages = {
                    'PopMode',
                },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = {
                    'Terminate',
                },
            },
        },
    },
}

------ Search
co.modes.builtin.search = {
    name = 'search',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'RemoveInputBufferLastCharacter',
                    {
                        AddNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
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
            down = {
                help = 'down',
                messages = { 'FocusNext' },
            },
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
            tab = {
                help = 'toggle selection',
                messages = { 'ToggleSelection', 'FocusNext' },
            },
            up = {
                help = 'up',
                messages = { 'FocusPrevious' },
            },
        },
        on_alphabet = nil,
        on_number = nil,
        on_special_character = nil,
        default = {
            help = nil,
            messages = {
                {
                    RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                },
                'BufferInputFromKey',
                {
                    AddNodeFilterFromInput = 'IRelativePathDoesContain',
                },
                'ExplorePwdAsync',
            },
        },
    },
}

co.modes.builtin.search.key_bindings.on_key['esc'] = co.modes.builtin.search.key_bindings.on_key.enter
co.modes.builtin.search.key_bindings.on_key['ctrl-n'] = co.modes.builtin.search.key_bindings.on_key.down
co.modes.builtin.search.key_bindings.on_key['ctrl-p'] = co.modes.builtin.search.key_bindings.on_key.up

------ Filter
co.modes.builtin.filter = {
    name = 'filter',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['R'] = {
                help = 'relative does not contain',
                messages = {
                    {
                        SwitchModeBuiltin = 'relative_path_does_not_contain',
                    },
                    {
                        SetInputBuffer = '',
                    },
                    {
                        AddNodeFilterFromInput = 'IRelativePathDoesNotContain',
                    },
                    'ExplorePwdAsync',
                },
            },
            backspace = {
                help = 'remove last filter',
                messages = { 'RemoveLastNodeFilter', 'ExplorePwdAsync' },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['ctrl-r'] = {
                help = 'reset filters',
                messages = { 'ResetNodeFilters', 'ExplorePwdAsync' },
            },
            ['ctrl-u'] = {
                help = 'clear filters',
                messages = { 'ClearNodeFilters', 'ExplorePwdAsync' },
            },
            enter = {
                help = 'done',
                messages = { 'PopMode' },
            },
            ['r'] = {
                help = 'relative does contain',
                messages = {
                    {
                        SwitchModeBuiltin = 'relative_path_does_contain',
                    },
                    {
                        SetInputBuffer = '',
                    },
                    {
                        AddNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
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

------ Relative path does contain
co.modes.builtin.relative_path_does_contain = {
    name = 'relative path does contain',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'RemoveInputBufferLastCharacter',
                    {
                        AddNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['ctrl-u'] = {
                help = 'remove line',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    {
                        SetInputBuffer = '',
                    },
                    {
                        AddNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-w'] = {
                help = 'remove last word',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'RemoveInputBufferLastWord',
                    {
                        AddNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'ExplorePwdAsync',
                },
            },
            enter = {
                help = 'apply filter',
                messages = { 'PopMode' },
            },
            esc = {
                help = 'cancel',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
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
                {
                    RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                },
                'BufferInputFromKey',
                {
                    AddNodeFilterFromInput = 'IRelativePathDoesContain',
                },
                'ExplorePwdAsync',
            },
        },
    },
}

------ Relative path does not contain
co.modes.builtin.relative_path_does_not_contain = {
    name = 'relative path does not contain',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            backspace = {
                help = 'remove last character',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain',
                    },
                    'RemoveInputBufferLastCharacter',
                    {
                        AddNodeFilterFromInput = 'IRelativePathDoesNotContain',
                    },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['ctrl-u'] = {
                help = 'remove line',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain',
                    },
                    {
                        SetInputBuffer = '',
                    },
                    {
                        AddNodeFilterFromInput = 'IRelativePathDoesNotContain',
                    },
                    'ExplorePwdAsync',
                },
            },
            ['ctrl-w'] = {
                help = 'remove last word',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain',
                    },
                    'RemoveInputBufferLastWord',
                    {
                        AddNodeFilterFromInput = 'IRelativePathDoesNotContain',
                    },
                    'ExplorePwdAsync',
                },
            },
            enter = {
                help = 'apply filter',
                messages = { 'PopMode' },
            },
            esc = {
                help = 'cancel',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain',
                    },
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
                {
                    RemoveNodeFilterFromInput = 'IRelativePathDoesNotContain',
                },
                'BufferInputFromKey',
                {
                    AddNodeFilterFromInput = 'IRelativePathDoesNotContain',
                },
                'ExplorePwdAsync',
            },
        },
    },
}

------ Sort
co.modes.builtin.sort = {
    name = 'sort',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['!'] = {
                help = 'reverse sorters',
                messages = { 'ReverseNodeSorters', 'ExplorePwdAsync' },
            },
            ['E'] = {
                help = 'by canonical extension reverse',
                messages = {
                    {
                        AddNodeSorter = { sorter = 'ByCanonicalExtension', reverse = true },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['M'] = {
                help = 'by canonical mime essence reverse',
                messages = {
                    {
                        AddNodeSorter = { sorter = 'ByCanonicalMimeEssence', reverse = true },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['N'] = {
                help = 'by node type reverse',
                messages = {
                    {
                        AddNodeSorter = { sorter = 'ByCanonicalIsDir', reverse = true },
                    },
                    {
                        AddNodeSorter = { sorter = 'ByCanonicalIsFile', reverse = true },
                    },
                    {
                        AddNodeSorter = { sorter = 'ByIsSymlink', reverse = true },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['R'] = {
                help = 'by relative path reverse',
                messages = {
                    {
                        AddNodeSorter = { sorter = 'ByIRelativePath', reverse = true },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['S'] = {
                help = 'by size reverse',
                messages = {
                    {
                        AddNodeSorter = { sorter = 'BySize', reverse = true },
                    },
                    'ExplorePwdAsync',
                },
            },
            backspace = {
                help = 'remove last sorter',
                messages = { 'RemoveLastNodeSorter', 'ExplorePwdAsync' },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            ['ctrl-r'] = {
                help = 'reset sorters',
                messages = { 'ResetNodeSorters', 'ExplorePwdAsync' },
            },
            ['ctrl-u'] = {
                help = 'clear sorters',
                messages = { 'ClearNodeSorters', 'ExplorePwdAsync' },
            },
            ['e'] = {
                help = 'by canonical extension',
                messages = {
                    {
                        AddNodeSorter = { sorter = 'ByCanonicalExtension', reverse = false },
                    },
                    'ExplorePwdAsync',
                },
            },
            enter = {
                help = 'done',
                messages = { 'PopMode' },
            },
            ['m'] = {
                help = 'by canonical mime essence',
                messages = {
                    {
                        AddNodeSorter = { sorter = 'ByCanonicalMimeEssence', reverse = false },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['n'] = {
                help = 'by node type',
                messages = {
                    {
                        AddNodeSorter = { sorter = 'ByCanonicalIsDir', reverse = false },
                    },
                    {
                        AddNodeSorter = { sorter = 'ByCanonicalIsFile', reverse = false },
                    },
                    {
                        AddNodeSorter = { sorter = 'ByIsSymlink', reverse = false },
                    },
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
                    {
                        AddNodeSorter = { sorter = 'BySize', reverse = false },
                    },
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

co.modes.builtin.sort.key_bindings.on_key['esc'] = co.modes.builtin.sort.key_bindings.on_key.enter

------ Switch layout
co.modes.builtin.switch_layout = {
    name = 'switch layout',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['1'] = {
                help = 'mine',
                messages = {
                    { SwitchLayoutCustom = 'mine' },
                    'PopMode',
                },
            },
            ['2'] = {
                help = 'default',
                messages = {
                    { SwitchLayoutBuiltin = 'default' },
                    'PopMode',
                },
            },
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            esc = {
                help = 'cancel',
                messages = { 'PopMode' },
            },
        },
    },
}

---- Custom
co.modes.custom = {}

-------- Format path column
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

-------- Format permissions column
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

-------- Format size column
xplr.fn.builtin.fmt_general_table_row_cols_2 = function(m)
    return m.is_dir and '' or m.human_size
end

-------- Format mime column
xplr.fn.builtin.fmt_general_table_row_cols_3 = function(m)
    if m.is_symlink and not m.is_broken then
        return m.symlink.mime_essence
    else
        return m.mime_essence
    end
end

xplr.fn.custom.edit_file = function(a)
    return {
        {
            Call = {
                command = 'kitty',
                args = {
                    '@launch',
                    '--type=tab',
                    '--no-response',
                    '--location=after',
                    '--cwd=' .. a.focused_node.parent,
                    'nvim',
                    a.focused_node.canonical.absolute_path,
                },
            },
        },
    }
end

xplr.fn.custom.opener = function(a)
    local c = a.focused_node.canonical
    return c.is_dir and { 'Enter' }
        or {
            {
                CallSilently = {
                    command = 'xdg-open',
                    args = { c.absolute_path },
                },
            },
        }
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

require('preview-tabbed').setup({
    mode = 'default',
    key = 'f3',
    previewer = os.getenv('HOME') .. '/.config/xplr/volatile/nnn/plugins/preview-tui',
})

local csw = require('context-switch')
csw.setup()
xplr.fn.custom.render_context_num = function(_)
    return '  ' .. tostring(csw.get_current_context_num())
end
