local awful = require('awful')
local capi = { keygrabber = keygrabber }
local wibox = require('wibox')
local gears = require('gears')
local beautiful = require('beautiful')
local gears_color = require("gears.color")
local recolor_image = gears_color.recolor_image

local logout = {}

local onlogout = function()
    awesome.quit()
end
local onlock = function()
   awful.spawn.with_shell(os.getenv('HOME') .. '/bin/lockScreen')
end
local onreboot = function()
    awful.spawn.with_shell('systemctl reboot')
end
local onsuspend = function()
    awful.spawn.with_shell('systemctl suspend')
end
local onpoweroff = function()
    awful.spawn.with_shell('systemctl poweroff')
end

local _instance = nil

local w = wibox({
    bg = beautiful.fg_normal,
    max_widget_size = 500,
    ontop = true,
    height = 200,
    width = 420,
    shape = function(cr, width, height)
        gears.shape.rounded_rect(cr, width, height, 8)
    end,
})

local action = wibox.widget({
    text = ' ',
    widget = wibox.widget.textbox,
    forced_height = 32,
})

local phrase_widget = wibox.widget({
    align = 'center',
    widget = wibox.widget.textbox,
})

local function create_button(icon, action_name, color, label_color, onclick, icon_size, icon_margin)
    local onclickW = function()
        w.visible = false
        capi.keygrabber.stop()
        onclick()
    end

    local def_color = '#000000'

    local result = wibox.widget({
        {
            {
                image = icon,
                resize = true,
                forced_height = icon_size,
                forced_width = icon_size,
                widget = wibox.widget.imagebox,
            },
            margins = icon_margin,
            widget = wibox.container.margin,
        },
        bg = def_color,
        widget = wibox.container.background,
    })
    result:set_shape(gears.shape.circle)

    result:connect_signal('mouse::enter', function(c)
        c:set_bg(color)
        action:set_markup('<span color="' .. label_color .. '" size="20000">' .. action_name .. '</span>')
    end)
    result:connect_signal('mouse::leave', function(c)
        c:set_bg(def_color)
        action:set_text('')
    end)
    result:connect_signal('button::press', function()
        onclickW()
    end)

    return result
end

function logout.setup()
    if _instance then
        return
    end
    _instance = 1
    local img_path = gears.filesystem.get_configuration_dir() .. 'widgets/logout/icons/'

    local bg_color = beautiful.bg_normal
    local accent_color = beautiful.bg_focus
    local text_color = beautiful.fg_normal
    local icon_size = 40
    local icon_margin = 16

    w:set_bg(bg_color)
        phrase_widget:set_markup(
            '<span color="' .. text_color .. '" size="20000">Goodbye!</span>'
        )

    w:setup({
        {
            phrase_widget,
            {
                {
                    create_button(
                        img_path .. 'log-out.svg',
                        'Log Out (l)',
                        accent_color,
                        text_color,
                        onlogout,
                        icon_size,
                        icon_margin
                    ),
                    create_button(
                        -- img_path .. 'lock.svg',
		       -- recolor_image('/usr/share/icons/Adwaita/scalable/status/system-lock-screen-symbolic.svg','#eeeeee'),
		       recolor_image('/usr/share/icons/Yaru/scalable/actions/system-lock-screen-symbolic.svg', '#ffffff'),
		       -- '/usr/share/icons/Yaru/scalable/actions/system-lock-screen-symbolic.svg',
		       'Lock (k)',
                        accent_color,
                        text_color,
                        onlock,
                        icon_size,
                        icon_margin
                    ),
                    create_button(
                        img_path .. 'moon.svg',
                        'Suspend (u)',
                        accent_color,
                        text_color,
                        onsuspend,
                        icon_size,
                        icon_margin
                    ),
		     create_button(
                        img_path .. 'refresh-cw.svg',
                        'Reboot (r)',
                        accent_color,
                        text_color,
                        onreboot,
                        icon_size,
                        icon_margin
                    ),
                    create_button(
                        img_path .. 'power.svg',
                        'Power Off (s)',
                        accent_color,
                        text_color,
                        onpoweroff,
                        icon_size,
                        icon_margin
                    ),
                    id = 'buttons',
                    spacing = 10,
                    layout = wibox.layout.fixed.horizontal,
                },
                valigh = 'center',
                layout = wibox.container.place,
            },
            {
                action,
                haligh = 'center',
                layout = wibox.container.place,
            },
            spacing = 32,
            layout = wibox.layout.fixed.vertical,
        },
        id = 'a',
        shape_border_width = 1,
        valigh = 'center',
        layout = wibox.container.place,
    })

    awful.placement.centered(w)
end

function logout.show()
    if w.visible then
        return
    end
    w.screen = mouse.screen
    w.visible = true
    capi.keygrabber.run(function(_, key, event)
        if event == 'release' then
            return
        end
        if key then
            if key == 'Escape' then
                phrase_widget:set_text('')
                capi.keygrabber.stop()
                w.visible = false
            elseif key == 's' then
                onpoweroff()
            elseif key == 'r' then
                onreboot()
            elseif key == 'u' then
                onsuspend()
            elseif key == 'k' then
                onlock()
            elseif key == 'l' then
                onlogout()
            end

            if key == 'Escape' or string.match('srukl', key) then
                phrase_widget:set_text('')
                capi.keygrabber.stop()
                w.visible = false
            end
        end
    end)
end

return logout
