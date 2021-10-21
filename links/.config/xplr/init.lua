-- You need to define the script version for compatibility check.
-- See https://github.com/sayanarijit/xplr/wiki/Upgrade-Guide.
version = '0.15.0'

local xplr = xplr
local genco = xplr.config.general

genco.enable_mouse = true
genco.show_hidden = true
genco.cursor.style.bg = { Rgb = { 255, 255, 250 } }
genco.cursor.style.fg = { Rgb = { 252, 151, 30 } }
genco.initial_layout = 'mine'
genco.initial_sorting = {
    { sorter = 'ByCanonicalIsDir', reverse = true },
    { sorter = 'ByIRelativePath', reverse = false },
}
genco.table.col_widths = {
    { Percentage = 8 },
    { Percentage = 61 },
    { Percentage = 8 },
    { Percentage = 8 },
    { Percentage = 15 },
}
genco.table.header.style.add_modifiers = { 'Bold' }
genco.table.header.cols = {
    {
        format = '  Index',
    },
    {
        format = '  Path',
    },
    {
        format = 'Permissions',
    },
    {
        format = 'Size',
    },
    {
        format = 'Type',
    },
}
genco.table.tree = {
    {
        format = '',
    },
    {
        format = '',
    },
    {
        format = '',
    },
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

---- Node types
------ Directory
xplr.config.node_types.directory.meta.icon = 'ð'
xplr.config.node_types.directory.style.add_modifiers = { 'Bold' }
xplr.config.node_types.directory.style.sub_modifiers = nil
xplr.config.node_types.directory.style.bg = nil
xplr.config.node_types.directory.style.fg = 'Cyan'

------ File
xplr.config.node_types.file.meta.icon = 'ƒ'
xplr.config.node_types.file.style.add_modifiers = nil
xplr.config.node_types.file.style.sub_modifiers = nil
xplr.config.node_types.file.style.bg = nil
xplr.config.node_types.file.style.fg = nil

------ Symlink
xplr.config.node_types.symlink.meta.icon = '§'
xplr.config.node_types.symlink.style.add_modifiers = { 'Italic' }
xplr.config.node_types.symlink.style.sub_modifiers = nil
xplr.config.node_types.symlink.style.bg = nil
xplr.config.node_types.symlink.style.fg = 'Magenta'

------ Mime essence
xplr.config.node_types.mime_essence = {}

------ Extension
xplr.config.node_types.extension = {}

------ Special
xplr.config.node_types.special = {}

xplr.config.layouts.custom = {
    mine = {
        Vertical = {
            config = {
                constraints = {
                    {
                        Percentage = 80,
                    },
                    {
                        Percentage = 20,
                    },
                },
            },
            splits = {
                'Table',
                {
                    Horizontal = {
                        config = {
                            constraints = {
                                {
                                    Percentage = 50,
                                },
                                {
                                    Percentage = 50,
                                },
                            },
                        },
                        splits = {
                            {
                                Vertical = {
                                    config = {
                                        constraints = {
                                            {
                                                Percentage = 50,
                                            },
                                            {
                                                Percentage = 50,
                                            },
                                        },
                                    },
                                    splits = {
                                        'SortAndFilter',
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
    },
}

-- Modes
---- Builtin
------ Default
xplr.config.modes.builtin.default = {
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
                    {
                        ToggleNodeFilter = {
                            filter = 'RelativePathDoesNotStartWith',
                            input = '.',
                        },
                    },
                    'ExplorePwdAsync',
                },
            },
            [':'] = {
                help = 'action',
                messages = {
                    'PopMode',
                    {
                        SwitchModeBuiltin = 'action',
                    },
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
            ['ctrl-f'] = {
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
                    {
                        SwitchModeBuiltin = 'switch_layout',
                    },
                },
            },
            ['d'] = {
                help = 'delete',
                messages = {
                    'PopMode',
                    {
                        SwitchModeBuiltin = 'delete',
                    },
                },
            },
            down = {
                help = 'down',
                messages = { 'FocusNext' },
            },
            enter = {
                help = 'quit with result',
                messages = { 'PrintResultAndQuit' },
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
                messages = { 'Enter' },
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

xplr.config.modes.builtin.default.key_bindings.on_key['tab'] =
    xplr.config.modes.builtin.default.key_bindings.on_key['ctrl-i']

xplr.config.modes.builtin.default.key_bindings.on_key['v'] = xplr.config.modes.builtin.default.key_bindings.on_key.space

xplr.config.modes.builtin.default.key_bindings.on_key['V'] =
    xplr.config.modes.builtin.default.key_bindings.on_key['ctrl-a']

xplr.config.modes.builtin.default.key_bindings.on_key['/'] =
    xplr.config.modes.builtin.default.key_bindings.on_key['ctrl-f']

xplr.config.modes.builtin.default.key_bindings.on_key['h'] = xplr.config.modes.builtin.default.key_bindings.on_key.left

xplr.config.modes.builtin.default.key_bindings.on_key['j'] = xplr.config.modes.builtin.default.key_bindings.on_key.down

xplr.config.modes.builtin.default.key_bindings.on_key['k'] = xplr.config.modes.builtin.default.key_bindings.on_key.up

xplr.config.modes.builtin.default.key_bindings.on_key['l'] = xplr.config.modes.builtin.default.key_bindings.on_key.right

------ Recover
xplr.config.modes.builtin.recover = {
    name = 'recover',
    layout = {
        CustomContent = {
            title = ' recover ',
            body = {
                StaticParagraph = {
                    render = [[

  You pressed an invalid key and went into "recover" mode.
  This mode saves you from performing unwanted actions.

  Let's calm down, press `escape`, and try again.
          ]],
                },
            },
        },
    },
    key_bindings = {
        on_key = {
            ['ctrl-c'] = {
                help = 'terminate',
                messages = { 'Terminate' },
            },
            esc = {
                help = 'escape',
                messages = { 'PopMode' },
            },
        },
        default = {
            messages = {},
        },
    },
}

------ Selection ops
xplr.config.modes.builtin.selection_ops = {
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
xplr.config.modes.builtin.create = {
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
                    {
                        SwitchModeBuiltin = 'create directory',
                    },
                    {
                        SetInputBuffer = '',
                    },
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
                    {
                        SwitchModeBuiltin = 'create file',
                    },
                    {
                        SetInputBuffer = '',
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

------ Create directory
xplr.config.modes.builtin.create_directory = {
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
xplr.config.modes.builtin.create_file = {
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
xplr.config.modes.builtin.number = {
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

xplr.config.modes.builtin.number.key_bindings.on_key['j'] = xplr.config.modes.builtin.number.key_bindings.on_key.down
xplr.config.modes.builtin.number.key_bindings.on_key['k'] = xplr.config.modes.builtin.number.key_bindings.on_key.up

------ Go to
xplr.config.modes.builtin.go_to = {
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
xplr.config.modes.builtin.rename = {
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
xplr.config.modes.builtin.delete = {
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
xplr.config.modes.builtin.action = {
    name = 'action to',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['!'] = {
                help = 'shell',
                messages = {
                    {
                        Call = {
                            command = 'zsh',
                            args = { '-i' },
                        },
                    },
                    'ExplorePwdAsync',
                    'PopMode',
                },
            },
            ['c'] = {
                help = 'create',
                messages = {
                    'PopMode',
                    {
                        SwitchModeBuiltin = 'create',
                    },
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
            ['m'] = {
                help = 'toggle mouse',
                messages = {
                    'PopMode',
                    'ToggleMouse',
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
xplr.config.modes.builtin.quit = {
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
xplr.config.modes.builtin.search = {
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
            down = {
                help = 'down',
                messages = { 'FocusNext' },
            },
            enter = {
                help = 'focus',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'PopMode',
                    'ExplorePwdAsync',
                },
            },
            left = {
                help = 'back',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'Back',
                    {
                        SetInputBuffer = '',
                    },
                    'ExplorePwdAsync',
                },
            },
            right = {
                help = 'enter',
                messages = {
                    {
                        RemoveNodeFilterFromInput = 'IRelativePathDoesContain',
                    },
                    'Enter',
                    {
                        SetInputBuffer = '',
                    },
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

xplr.config.modes.builtin.search.key_bindings.on_key['esc'] = xplr.config.modes.builtin.search.key_bindings.on_key.enter
xplr.config.modes.builtin.search.key_bindings.on_key['ctrl-n'] =
    xplr.config.modes.builtin.search.key_bindings.on_key.down
xplr.config.modes.builtin.search.key_bindings.on_key['ctrl-p'] = xplr.config.modes.builtin.search.key_bindings.on_key.up

------ Filter
xplr.config.modes.builtin.filter = {
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

xplr.config.modes.builtin.filter.key_bindings.on_key['esc'] = xplr.config.modes.builtin.filter.key_bindings.on_key.enter

------ Relative path does contain
xplr.config.modes.builtin.relative_path_does_contain = {
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
xplr.config.modes.builtin.relative_path_does_not_contain = {
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
xplr.config.modes.builtin.sort = {
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
                        AddNodeSorter = {
                            sorter = 'ByCanonicalExtension',
                            reverse = true,
                        },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['M'] = {
                help = 'by canonical mime essence reverse',
                messages = {
                    {
                        AddNodeSorter = {
                            sorter = 'ByCanonicalMimeEssence',
                            reverse = true,
                        },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['N'] = {
                help = 'by node type reverse',
                messages = {
                    {
                        AddNodeSorter = {
                            sorter = 'ByCanonicalIsDir',
                            reverse = true,
                        },
                    },
                    {
                        AddNodeSorter = {
                            sorter = 'ByCanonicalIsFile',
                            reverse = true,
                        },
                    },
                    {
                        AddNodeSorter = {
                            sorter = 'ByIsSymlink',
                            reverse = true,
                        },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['R'] = {
                help = 'by relative path reverse',
                messages = {
                    {
                        AddNodeSorter = {
                            sorter = 'ByIRelativePath',
                            reverse = true,
                        },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['S'] = {
                help = 'by size reverse',
                messages = {
                    {
                        AddNodeSorter = {
                            sorter = 'BySize',
                            reverse = true,
                        },
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
                        AddNodeSorter = {
                            sorter = 'ByCanonicalExtension',
                            reverse = false,
                        },
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
                        AddNodeSorter = {
                            sorter = 'ByCanonicalMimeEssence',
                            reverse = false,
                        },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['n'] = {
                help = 'by node type',
                messages = {
                    {
                        AddNodeSorter = {
                            sorter = 'ByCanonicalIsDir',
                            reverse = false,
                        },
                    },
                    {
                        AddNodeSorter = {
                            sorter = 'ByCanonicalIsFile',
                            reverse = false,
                        },
                    },
                    {
                        AddNodeSorter = {
                            sorter = 'ByIsSymlink',
                            reverse = false,
                        },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['r'] = {
                help = 'by relative path',
                messages = {
                    {
                        AddNodeSorter = {
                            sorter = 'ByIRelativePath',
                            reverse = false,
                        },
                    },
                    'ExplorePwdAsync',
                },
            },
            ['s'] = {
                help = 'by size',
                messages = {
                    {
                        AddNodeSorter = {
                            sorter = 'BySize',
                            reverse = false,
                        },
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

xplr.config.modes.builtin.sort.key_bindings.on_key['esc'] = xplr.config.modes.builtin.sort.key_bindings.on_key.enter

------ Switch layout
xplr.config.modes.builtin.switch_layout = {
    name = 'switch layout',
    help = nil,
    extra_help = nil,
    key_bindings = {
        on_key = {
            ['1'] = {
                help = 'mine',
                messages = {
                    {
                        SwitchLayoutCustom = 'mine',
                    },
                    'PopMode',
                },
            },
            ['2'] = {
                help = 'default',
                messages = {
                    {
                        SwitchLayoutBuiltin = 'default',
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
        },
    },
}

---- Custom
xplr.config.modes.custom = {}

-- Function
---- Builtin
------ Formaters
-------- Format index column
xplr.fn.builtin.fmt_general_table_row_cols_0 = function(m)
    return (m.is_before_focus and ' -' or '  ') .. m.relative_index .. '│' .. m.index
end

-------- Format path column
xplr.fn.builtin.fmt_general_table_row_cols_1 = function(m)
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
xplr.fn.builtin.fmt_general_table_row_cols_2 = function(m)
    local no_color = os.getenv('NO_COLOR')

    local function green(x)
        if no_color == nil then
            return '\x1b[32m' .. x .. '\x1b[0m'
        else
            return x
        end
    end

    local function yellow(x)
        if no_color == nil then
            return '\x1b[33m' .. x .. '\x1b[0m'
        else
            return x
        end
    end

    local function red(x)
        if no_color == nil then
            return '\x1b[31m' .. x .. '\x1b[0m'
        else
            return x
        end
    end

    local function bit(x, color, cond)
        if cond then
            return color(x)
        else
            return color('-')
        end
    end

    local p = m.permissions

    local r = ''

    -- User
    r = r .. bit('r', green, p.user_read)
    r = r .. bit('w', yellow, p.user_write)

    if p.user_execute == false and p.setuid == false then
        r = r .. bit('-', red, p.user_execute)
    elseif p.user_execute == true and p.setuid == false then
        r = r .. bit('x', red, p.user_execute)
    elseif p.user_execute == false and p.setuid == true then
        r = r .. bit('S', red, p.user_execute)
    else
        r = r .. bit('s', red, p.user_execute)
    end

    -- Group
    r = r .. bit('r', green, p.group_read)
    r = r .. bit('w', yellow, p.group_write)

    if p.group_execute == false and p.setuid == false then
        r = r .. bit('-', red, p.group_execute)
    elseif p.group_execute == true and p.setuid == false then
        r = r .. bit('x', red, p.group_execute)
    elseif p.group_execute == false and p.setuid == true then
        r = r .. bit('S', red, p.group_execute)
    else
        r = r .. bit('s', red, p.group_execute)
    end

    -- Other
    r = r .. bit('r', green, p.other_read)
    r = r .. bit('w', yellow, p.other_write)

    if p.other_execute == false and p.setuid == false then
        r = r .. bit('-', red, p.other_execute)
    elseif p.other_execute == true and p.setuid == false then
        r = r .. bit('x', red, p.other_execute)
    elseif p.other_execute == false and p.setuid == true then
        r = r .. bit('T', red, p.other_execute)
    else
        r = r .. bit('t', red, p.other_execute)
    end

    return r
end

-------- Format size column
xplr.fn.builtin.fmt_general_table_row_cols_3 = function(m)
    return m.is_dir and '' or m.human_size
end

-------- Format mime column
xplr.fn.builtin.fmt_general_table_row_cols_4 = function(m)
    if m.is_symlink and not m.is_broken then
        return m.symlink.mime_essence
    else
        return m.mime_essence
    end
end

---- Custom
xplr.fn.custom = {}
