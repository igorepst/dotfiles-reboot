local awful = require('awful')
local modkey = require('conf.config').modkey
local wibox = require('wibox')
local beautiful = require('beautiful')

local mykeyboardlayout = require('widgets.keyboardlayout.widget')

tag.connect_signal('request::default_layouts', function()
    awful.layout.append_default_layouts({
        awful.layout.suit.tile,
    })
end)

screen.connect_signal('request::desktop_decoration', function(s)
    awful.tag(
        {
            '',
            '',
            '',
        },
        s,
	awful.layout.layouts[1]
    )

    s.mytaglist = awful.widget.taglist({
        screen = s,
        filter = awful.widget.taglist.filter.noempty,
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
                widget = wmargin,
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

    local cal = awful.popup({
        widget = {
            {
                id = 'cal',
                week_numbers = true,
                start_sunday = true,
                long_weekdays = true,
                widget = wibox.widget.calendar.month,
            },
            margins = 10,
            widget = wmargin,
        },
        hide_on_right_click = true,
        visible = false,
        ontop = true,
    })

    local clock = wibox.widget({
        format = '%a %d/%m,%H:%M',
        widget = wibox.widget.textclock,
        buttons = {
            awful.button({}, 1, function()
                if cal.visible then
                    cal.visible = false
                else
                    cal.widget:get_children_by_id('cal')[1].date = os.date('*t')
                    cal:move_next_to(mouse.current_widget_geometry)
                    cal.visible = true
                end
            end),
        },
    })

    s.mywibox = awful.wibar({ position = 'bottom', screen = s })

    s.mywibox.widget = {
        layout = wibox.layout.align.horizontal,
        s.mytaglist,
        wmargin(s.mytasklist, 0, dpi(7)),
        {
            layout = wfixed.horizontal,
            spacing = dpi(7),
            mykeyboardlayout(),
            volume_widget({
		  device = 'default',
		  widget_type = 'icon_and_text',
            }),
            upower_bat(),
            wmargin(systray, 0, 0, dpi(3), 0),
            wmargin(clock, 0, dpi(3), 0, 0),
        },
    }
end)
