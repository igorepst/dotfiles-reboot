local awful = require('awful')
local wibox = require('wibox')

client.connect_signal('request::titlebars', function(c)
    local buttons = {
        awful.button({}, 1, function()
            c:activate({ context = 'titlebar', action = 'mouse_move' })
        end),
        awful.button({}, 3, function()
            c:activate({ context = 'titlebar', action = 'mouse_resize' })
        end),
    }

    awful.titlebar(c).widget = {
        {
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout = wibox.layout.fixed.horizontal,
        },
        {
            {
                align = 'center',
                widget = awful.titlebar.widget.titlewidget(c),
            },
            buttons = buttons,
            layout = wibox.layout.flex.horizontal,
        },
        {
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
