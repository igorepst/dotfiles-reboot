local ruled = require('ruled')
local awful = require('awful')

function awful.rules.delayed_properties.delayed_placement(c, _, props)
    if props.delayed_placement then
        awful.rules.extra_properties.placement(c, props.delayed_placement, props)
    end
end

function awful.rules.delayed_properties.delayed_max(c, _, props)
    if props.delayed_max then
        c.maximized = awful.layout.getname(awful.layout.get(c.screen)) == 'floating'
    end
end

ruled.client.connect_signal('request::rules', function()
    ruled.client.append_rule({
        id = 'global',
        rule = {},
        properties = {
            focus = awful.client.focus.filter,
            raise = true,
            screen = awful.screen.preferred,
            placement = awful.placement.under_mouse + awful.placement.no_overlap + awful.placement.no_offscreen,
            size_hints_honor = false,
        },
    })

    -- Floating clients.
    ruled.client.append_rule({
        id = 'floating',
        rule_any = {
            instance = { 'copyq', 'pinentry' },
            class = {
	       'Blueman-manager',
            },
            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                'Event Tester', -- xev.
            },
            role = {
	       'pop-up',
	       'gimp-startup'
            },
        },
        properties = { floating = true, placement = awful.placement.centered,},
    })

     ruled.client.append_rule({
        id = 'mpv',
        rule_any = { class = { 'mpv' } },
	-- As of 06/06/22 there seems to be a bug in tiling layout:
	-- mpv.conf defines fullscreen, however awesome leaves some gap at the bottom
	-- fullscreen = false here causes mpv to start fullscreen without a gap!
        properties = { floating = true, fullscreen = false, placement = awful.placement.centered },
    })

    ruled.client.append_rule({
        id = 'vivaldi',
        rule_any = { class = { 'vivaldi-stable' } },
        properties = {
	   maximized_vertical   = true,
	   maximized_horizontal = true
	},
    })

    ruled.client.append_rule({
        id = 'pavucontrol',
        rule_any = { class = { 'Pavucontrol' } },
        properties = {
	   floating = true,
	   width = 600,
	   height = 400,
	   placement = awful.placement.centered,
	   titlebars_enabled = true
	},
    })

    ruled.client.append_rule({
        id = 'emacs_dlg',
        rule_any = { class = {'Emacs'}, name = {"Question"},  },
        properties = {
	   floating = true,
	   placement = awful.placement.centered,
	},
    })

    ruled.client.append_rule({
        id = 'zoom',
        rule_any = { class = { 'zoom', 'zoom ' } },
        properties = {
	   floating = true,
	   placement = awful.placement.centered,
	   titlebars_enabled = true
	},
    })
end)
