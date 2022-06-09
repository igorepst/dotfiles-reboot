local naughty = require('naughty')
local awful = require('awful')
local ruled = require('ruled')

naughty.connect_signal('request::display_error', function(message, startup)
    naughty.notification({
        urgency = 'critical',
        title = 'Oops, an error happened' .. (startup and ' during startup!' or '!'),
        message = message,
    })
end)

naughty.connect_signal('request::display', function(n)
    naughty.layout.box({ notification = n })
end)

ruled.notification.connect_signal('request::rules', function()
    ruled.notification.append_rule({
        rule = {},
        properties = {
            screen = awful.screen.preferred,
            implicit_timeout = 5,
	    position = 'bottom_right',
        },
    })
end)
