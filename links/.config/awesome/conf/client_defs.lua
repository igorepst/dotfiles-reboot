local awful = require('awful')
local beautiful = require('beautiful')
local modkey = require('conf.config').modkey

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
            clientMenu:show()
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

client.connect_signal('request::manage', function(c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then
       awful.client.setslave(c)
    end
       if client.floating then
        client.placement = awful.placement.centered + awful.placement.no_overlap + awful.placement.no_offscreen
    end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal('mouse::enter', function(c)
    c:activate({ context = 'mouse_enter', raise = false })
end)
