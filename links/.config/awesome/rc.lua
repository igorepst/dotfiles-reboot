pcall(require, 'luarocks.loader')

-- Standard awesome library
local gears = require('gears')
local awful = require('awful')
require('awful.autofocus')
-- Widget and layout library
local wibox = require('wibox')
-- Theme handling library
local beautiful = require('beautiful')
-- Notification library
local naughty = require('naughty')
-- Declarative object management
local ruled = require('ruled')
local menubar = require('menubar')
local hotkeys_popup = require('awful.hotkeys_popup')
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require('awful.hotkeys_popup.keys')

naughty.connect_signal('request::display_error', function(message, startup)
    naughty.notification({
        urgency = 'critical',
        title = 'Oops, an error happened' .. (startup and ' during startup!' or '!'),
        message = message,
    })
end)

beautiful.init(gears.filesystem.get_configuration_dir() .. 'theme/gtk/theme.lua')

-- This is used later as the default terminal and editor to run.
local terminal = os.getenv('MYTERM')
-- local editor = os.getenv('EDITOR')
-- local editor_cmd = terminal .. ' -e ' .. editor
local modkey = 'Mod4'

-- local myawesomemenu = {
--     {
--         'hotkeys',
--         function()
--             hotkeys_popup.show_help(nil, awful.screen.focused())
--         end,
--     },
--     { 'manual', terminal .. ' -e man awesome' },
--     { 'edit config', editor_cmd .. ' ' .. awesome.conffile },
--     { 'restart', awesome.restart },
--     {
--         'quit',
--         function()
--             awesome.quit()
--         end,
--     },
-- }

-- local mymainmenu = awful.menu({
--     items = {
--         { 'awesome', myawesomemenu, beautiful.awesome_icon },
--         { 'open terminal', terminal },
--     },
-- })

-- local mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon, menu = mymainmenu })

menubar.utils.terminal = terminal -- Set the terminal for applications that require it

tag.connect_signal('request::default_layouts', function()
    awful.layout.append_default_layouts({
        awful.layout.suit.floating,
        awful.layout.suit.tile,
        awful.layout.suit.tile.left,
        awful.layout.suit.tile.bottom,
        awful.layout.suit.tile.top,
        awful.layout.suit.fair,
        awful.layout.suit.fair.horizontal,
        awful.layout.suit.spiral,
        awful.layout.suit.spiral.dwindle,
        awful.layout.suit.max,
        awful.layout.suit.max.fullscreen,
        awful.layout.suit.magnifier,
        awful.layout.suit.corner.nw,
    })
end)

local mykeyboardlayout = require('widgets.keyboardlayout.widget')
-- local mykeyboardlayout = awful.widget.keyboardlayout()
-- local gdb = require('gears.debug')
-- mykeyboardlayout.layout_name = function(v)
--     return v.file
-- end
-- mykeyboardlayout.widget.forced_width = 25
-- local capi = { awesome = awesome }
-- capi.awesome.emit_signal('xkb::map_changed')

screen.connect_signal('request::wallpaper', function(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        if type(wallpaper) == 'function' then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end)

screen.connect_signal('request::desktop_decoration', function(s)
    -- Each screen has its own tag table.
    awful.tag({
        '',
        '',
        --         ''
    }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox({
        screen = s,
        buttons = {
            awful.button({}, 1, function()
                awful.layout.inc(1)
            end),
            awful.button({}, 3, function()
                awful.layout.inc(-1)
            end),
            awful.button({}, 4, function()
                awful.layout.inc(-1)
            end),
            awful.button({}, 5, function()
                awful.layout.inc(1)
            end),
        },
    })

    s.mytaglist = awful.widget.taglist({
        screen = s,
        filter = awful.widget.taglist.filter.all,
        buttons = {
            awful.button({}, 1, function(t)
                t:view_only()
            end),
            awful.button({ modkey }, 1, function(t)
                if client.focus then
                    client.focus:move_to_tag(t)
                end
            end),
            awful.button({}, 3, awful.tag.viewtoggle),
            awful.button({ modkey }, 3, function(t)
                if client.focus then
                    client.focus:toggle_tag(t)
                end
            end),
            awful.button({}, 4, function(t)
                awful.tag.viewprev(t.screen)
            end),
            awful.button({}, 5, function(t)
                awful.tag.viewnext(t.screen)
            end),
        },
    })

    local wmargin = require('wibox.container.margin')
    local wtextbox = require('wibox.widget.textbox')
    local wbackground = require('wibox.container.background')
    local wfixed = require('wibox.layout.fixed')
    local dpi = require('beautiful').xresources.apply_dpi

    local default_template = {
        {
            {
                awful.widget.clienticon,
                margins = dpi(3),
                widget = wibox.container.margin,
            },
            {
                {
                    id = 'text_role',
                    widget = wtextbox,
                },
                id = 'text_margin_role',
                left = dpi(4),
                right = dpi(4),
                widget = wmargin,
            },
            fill_space = true,
            layout = wfixed.horizontal,
        },
        id = 'background_role',
        widget = wbackground,
    }

    s.mytasklist = awful.widget.tasklist({
        screen = s,
        filter = awful.widget.tasklist.filter.currenttags,
        buttons = {
            awful.button({}, 1, function(c)
                c:activate({ context = 'tasklist', action = 'toggle_minimization' })
            end),
            awful.button({}, 3, function()
                awful.menu.client_list({ theme = { width = 250 } })
            end),
            awful.button({}, 4, function()
                awful.client.focus.byidx(-1)
            end),
            awful.button({}, 5, function()
                awful.client.focus.byidx(1)
            end),
        },
        style = {
            font_focus = beautiful.font_bold,
        },
        widget_template = default_template,
    })

    local volume_widget = require('awesome-wm-widgets.volume-widget.volume')
    local upower_bat = require('widgets.battery.widget')
    local systray = wibox.widget.systray()
    systray:set_base_size(dpi(24))

    s.mywibox = awful.wibar({ position = 'bottom', screen = s })

    s.mywibox.widget = {
        layout = wibox.layout.align.horizontal,
        {
            layout = wibox.layout.fixed.horizontal,
            --             mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist,
        {
            layout = wibox.layout.fixed.horizontal,
            spacing = dpi(7),
            mykeyboardlayout(),
            volume_widget({
                widget_type = 'icon_and_text',
            }),
            upower_bat(),
            wibox.container.margin(systray, 0, 0, dpi(3), 0),
            wibox.widget({
                format = '%a %d/%m,%H:%M',
                widget = wibox.widget.textclock,
            }),
            s.mylayoutbox,
        },
    }
end)

awful.mouse.append_global_mousebindings({
    --     awful.button({}, 3, function()
    --         mymainmenu:toggle()
    --     end),
    awful.button({}, 4, awful.tag.viewprev),
    awful.button({}, 5, awful.tag.viewnext),
})

awful.keyboard.append_global_keybindings({
    awful.key({ modkey }, 's', hotkeys_popup.show_help, { description = 'show help', group = 'awesome' }),
    --     awful.key({ modkey }, 'w', function()
    --         mymainmenu:show()
    --     end, {
    --         description = 'show main menu',
    --         group = 'awesome',
    --     }),
    awful.key({ modkey, 'Control' }, 'r', awesome.restart, { description = 'reload awesome', group = 'awesome' }),
    awful.key({ modkey, 'Shift' }, 'q', awesome.quit, { description = 'quit awesome', group = 'awesome' }),
    awful.key({ modkey }, 'x', function()
        awful.prompt.run({
            prompt = 'Run Lua code: ',
            textbox = awful.screen.focused().mypromptbox.widget,
            exe_callback = awful.util.eval,
            history_path = awful.util.get_cache_dir() .. '/history_eval',
        })
    end, {
        description = 'lua execute prompt',
        group = 'awesome',
    }),
    awful.key({ modkey }, 'Return', function()
        awful.spawn(terminal)
    end, {
        description = 'open a terminal',
        group = 'launcher',
    }),
    awful.key({ modkey }, 'r', function()
        awful.spawn('rofiLauncher')
    end, {
        description = 'run Rofi',
        group = 'launcher',
    }),
    awful.key({ modkey }, 'e', function()
        awful.spawn('tvifm')
    end, {
        description = 'run Vifm',
        group = 'launcher',
    }),
    awful.key({ modkey }, 'p', function()
        menubar.show()
    end, {
        description = 'show the menubar',
        group = 'launcher',
    }),
    awful.key({}, 'XF86PowerOff', function()
        local logoutw =  require('widgets.logout.widget');
        logoutw.setup()
        logoutw.show()
    end, {
        description = 'Show logout screen',
        group = 'custom',
    }),
})

awful.keyboard.append_global_keybindings({
    awful.key({ modkey }, 'Left', awful.tag.viewprev, { description = 'view previous', group = 'tag' }),
    awful.key({ modkey }, 'Right', awful.tag.viewnext, { description = 'view next', group = 'tag' }),
    awful.key({ modkey }, 'Escape', awful.tag.history.restore, { description = 'go back', group = 'tag' }),
})

awful.keyboard.append_global_keybindings({
    awful.key({ modkey }, 'j', function()
        awful.client.focus.byidx(1)
    end, {
        description = 'focus next by index',
        group = 'client',
    }),
    awful.key({ modkey }, 'k', function()
        awful.client.focus.byidx(-1)
    end, {
        description = 'focus previous by index',
        group = 'client',
    }),
    awful.key({ modkey }, 'Tab', function()
        awful.client.focus.history.previous()
        if client.focus then
            client.focus:raise()
        end
    end, {
        description = 'go back',
        group = 'client',
    }),
    awful.key({ modkey, 'Control' }, 'j', function()
        awful.screen.focus_relative(1)
    end, {
        description = 'focus the next screen',
        group = 'screen',
    }),
    awful.key({ modkey, 'Control' }, 'k', function()
        awful.screen.focus_relative(-1)
    end, {
        description = 'focus the previous screen',
        group = 'screen',
    }),
    awful.key({ modkey, 'Control' }, 'n', function()
        local c = awful.client.restore()
        -- Focus restored client
        if c then
            c:activate({ raise = true, context = 'key.unminimize' })
        end
    end, {
        description = 'restore minimized',
        group = 'client',
    }),
})

awful.keyboard.append_global_keybindings({
    awful.key({ modkey, 'Shift' }, 'j', function()
        awful.client.swap.byidx(1)
    end, {
        description = 'swap with next client by index',
        group = 'client',
    }),
    awful.key({ modkey, 'Shift' }, 'k', function()
        awful.client.swap.byidx(-1)
    end, {
        description = 'swap with previous client by index',
        group = 'client',
    }),
    awful.key({ modkey }, 'u', awful.client.urgent.jumpto, { description = 'jump to urgent client', group = 'client' }),
    awful.key({ modkey }, 'l', function()
        awful.tag.incmwfact(0.05)
    end, {
        description = 'increase master width factor',
        group = 'layout',
    }),
    awful.key({ modkey }, 'h', function()
        awful.tag.incmwfact(-0.05)
    end, {
        description = 'decrease master width factor',
        group = 'layout',
    }),
    awful.key({ modkey, 'Shift' }, 'h', function()
        awful.tag.incnmaster(1, nil, true)
    end, {
        description = 'increase the number of master clients',
        group = 'layout',
    }),
    awful.key({ modkey, 'Shift' }, 'l', function()
        awful.tag.incnmaster(-1, nil, true)
    end, {
        description = 'decrease the number of master clients',
        group = 'layout',
    }),
    awful.key({ modkey, 'Control' }, 'h', function()
        awful.tag.incncol(1, nil, true)
    end, {
        description = 'increase the number of columns',
        group = 'layout',
    }),
    awful.key({ modkey, 'Control' }, 'l', function()
        awful.tag.incncol(-1, nil, true)
    end, {
        description = 'decrease the number of columns',
        group = 'layout',
    }),
    awful.key({ modkey }, 'space', function()
        awful.layout.inc(1)
    end, {
        description = 'select next',
        group = 'layout',
    }),
    awful.key({ modkey, 'Shift' }, 'space', function()
        awful.layout.inc(-1)
    end, {
        description = 'select previous',
        group = 'layout',
    }),
})

awful.keyboard.append_global_keybindings({
    awful.key({
        modifiers = { modkey },
        keygroup = 'numrow',
        description = 'only view tag',
        group = 'tag',
        on_press = function(index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                tag:view_only()
            end
        end,
    }),
    awful.key({
        modifiers = { modkey, 'Control' },
        keygroup = 'numrow',
        description = 'toggle tag',
        group = 'tag',
        on_press = function(index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                awful.tag.viewtoggle(tag)
            end
        end,
    }),
    awful.key({
        modifiers = { modkey, 'Shift' },
        keygroup = 'numrow',
        description = 'move focused client to tag',
        group = 'tag',
        on_press = function(index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:move_to_tag(tag)
                end
            end
        end,
    }),
    awful.key({
        modifiers = { modkey, 'Control', 'Shift' },
        keygroup = 'numrow',
        description = 'toggle focused client on tag',
        group = 'tag',
        on_press = function(index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:toggle_tag(tag)
                end
            end
        end,
    }),
    awful.key({
        modifiers = { modkey },
        keygroup = 'numpad',
        description = 'select layout directly',
        group = 'layout',
        on_press = function(index)
            local t = awful.screen.focused().selected_tag
            if t then
                t.layout = t.layouts[index] or t.layout
            end
        end,
    }),
})

client.connect_signal('request::default_mousebindings', function()
    awful.mouse.append_client_mousebindings({
        awful.button({}, 1, function(c)
            c:activate({ context = 'mouse_click' })
        end),
        awful.button({ modkey }, 1, function(c)
            c:activate({ context = 'mouse_click', action = 'mouse_move' })
        end),
        awful.button({ modkey }, 3, function(c)
            c:activate({ context = 'mouse_click', action = 'mouse_resize' })
        end),
    })
end)

client.connect_signal('request::default_keybindings', function()
    awful.keyboard.append_client_keybindings({
        awful.key({ modkey }, 'f', function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end, {
            description = 'toggle fullscreen',
            group = 'client',
        }),
        awful.key({ modkey }, 'm', function(c)
            local clientMenu = awful.menu({
                items = {
                    {
                        '&Close',
                        function()
                            c:kill()
                        end,
                        beautiful.titlebar_close_button_normal,
                    },
                    {
                        'Mi&nimize',
                        function()
                            c.minimized = true
                        end,
                        beautiful.titlebar_minimize_button_normal,
                    },
                    {
                        'Ma&ximize',
                        function()
                            c.maximized = not c.maximized
                            c:raise()
                        end,
                        beautiful.titlebar_maximized_button_normal_active,
                    },
                    {
                        'Max &vert',
                        function()
                            c.maximized_vertical = not c.maximized_vertical
                            c:raise()
                        end,
                        beautiful.titlebar_maximized_button_normal_active,
                    },
                    {
                        'Max hori&z',
                        function()
                            c.maximized_horizontal = not c.maximized_horizontal
                            c:raise()
                        end,
                        beautiful.titlebar_maximized_button_normal_active,
                    },
                    {
                        'On &top',
                        function()
                            c.ontop = not c.ontop
                        end,
                        beautiful.titlebar_ontop_button_normal_active,
                    },
                    {
                        '&Sticky',
                        function()
                            c.sticky = not c.sticky
                        end,
                        beautiful.titlebar_sticky_button_normal_active,
                    },
                },
            })
            clientMenu:toggle()
        end, {
            description = 'client menu',
            group = 'client',
        }),
        awful.key(
            { modkey, 'Control' },
            'space',
            awful.client.floating.toggle,
            { description = 'toggle floating', group = 'client' }
        ),
        awful.key({ modkey, 'Control' }, 'Return', function(c)
            c:swap(awful.client.getmaster())
        end, {
            description = 'move to master',
            group = 'client',
        }),
        awful.key({ modkey }, 'o', function(c)
            c:move_to_screen()
        end, {
            description = 'move to screen',
            group = 'client',
        }),
    })
end)

ruled.client.connect_signal('request::rules', function()
    ruled.client.append_rule({
        id = 'global',
        rule = {},
        properties = {
            focus = awful.client.focus.filter,
            raise = true,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen,
            size_hints_honor = false,
        },
    })

    -- Floating clients.
    ruled.client.append_rule({
        id = 'floating',
        rule_any = {
            instance = { 'copyq', 'pinentry' },
            class = {
                'Arandr',
                'Blueman-manager',
                'Gpick',
                'Kruler',
                'Sxiv',
                'Tor Browser',
                'Wpa_gui',
                'veromix',
                'xtightvncviewer',
            },
            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                'Event Tester', -- xev.
            },
            role = {
                'AlarmWindow', -- Thunderbird's calendar.
                'ConfigManager', -- Thunderbird's about:config.
                'pop-up', -- e.g. Google Chrome's (detached) Developer Tools.
            },
        },
        properties = { floating = true },
    })

--             ruled.client.append_rule({
--                 id = 'kitty',
--                 rule_any = { class = { 'kitty' } },
--                 properties = { maximized = true },
--             })

    ruled.client.append_rule({
        id = 'notetaker',
        rule_any = { class = { 'notetaker' } },
        properties = {
            titlebars_enabled = false,
            floating = true,
            width = 800,
            height = 600,
            placement = awful.placement.centered,
            ontop = true,
        },
    })
end)

client.connect_signal('request::manage', function(c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then
        awful.client.setslave(c)
        --     else
        --         if not c.size_hints.user_position and not c.size_hints.program_position then
        --             Prevent clients from being unreachable after screen count changes.
        --             awful.placement.no_offscreen(c)
        --         end
    end
end)

client.connect_signal('request::titlebars', function(c)
    -- buttons for the titlebar
    local buttons = {
        awful.button({}, 1, function()
            c:activate({ context = 'titlebar', action = 'mouse_move' })
        end),
        awful.button({}, 3, function()
            c:activate({ context = 'titlebar', action = 'mouse_resize' })
        end),
    }

    awful.titlebar(c).widget = {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout = wibox.layout.fixed.horizontal,
        },
        { -- Middle
            { -- Title
                align = 'center',
                widget = awful.titlebar.widget.titlewidget(c),
            },
            buttons = buttons,
            layout = wibox.layout.flex.horizontal,
        },
        { -- Right
            awful.titlebar.widget.floatingbutton(c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton(c),
            awful.titlebar.widget.ontopbutton(c),
            awful.titlebar.widget.closebutton(c),
            layout = wibox.layout.fixed.horizontal(),
        },
        layout = wibox.layout.align.horizontal,
    }
end)

ruled.notification.connect_signal('request::rules', function()
    ruled.notification.append_rule({
        rule = {},
        properties = {
            screen = awful.screen.preferred,
            implicit_timeout = 5,
        },
    })
end)

naughty.connect_signal('request::display', function(n)
    naughty.layout.box({ notification = n })
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal('mouse::enter', function(c)
    c:activate({ context = 'mouse_enter', raise = false })
end)

local function run_once(cmd_arr)
    for _, cmd in ipairs(cmd_arr) do
        local findme = cmd
        local firstspace = cmd:find(' ')
        if firstspace then
            findme = cmd:sub(0, firstspace - 1)
        end
        awful.spawn.with_shell(string.format('pgrep -u $USER -x %s > /dev/null || (%s)', findme, cmd))
    end
end
run_once({ 'picom -b', 'wifi' })
