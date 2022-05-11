local wibox = require('wibox')
local signal = require('widgets.battery.signal')
local popup = require('awful.popup')
local gears = require('gears')

local _M = {}

local toTimeStr = function(seconds)
    if seconds <= 0 then
        return '00:00'
    else
        local hours = string.format('%02.f', math.floor(seconds / 3600))
        local mins = string.format('%02.f', math.floor(seconds / 60 - (hours * 60)))
        return hours .. ':' .. mins
    end
end

local sm = function(title, value, color)
    local c = color or 'black'
    return '<span weight="bold">' .. title .. ':</span><span color="' .. c .. '"> ' .. tostring(value) .. '</span>\r'
end

local bat_color = { good = '#00ff00', low = '#ffff00', empty = '#ff0000' }
local img_path = gears.filesystem.get_configuration_dir() .. 'widgets/battery/icons/'

local setup = function()
    local pp = popup({
        widget = {
            {
                id = 'txt',
                widget = wibox.widget.textbox,
            },
            margins = 10,
            widget = wibox.container.margin,
        },
        shape = gears.shape.rounded_rect,
        hide_on_right_click = true,
        visible = false,
        ontop = true,
    })
    local w = wibox.widget({
        {
            {
                id = 'icon',
                resize = false,
                widget = wibox.widget.imagebox(),
            },
            valign = 'center',
            layout = wibox.container.place,
        },
        {
            id = 'txt',
            widget = wibox.widget.textbox(),
        },
        layout = wibox.layout.fixed.horizontal,
        spacing = 2,
    })
    local update = function(upd)
        for _, bn in ipairs(upd) do
            local level = bn.battery_level == 'None' and (bn.percentage .. '%') or bn.battery_level
            w:get_children_by_id('txt')[1].markup = level
            local color
            if string.match(bn.icon_name, 'full') or string.match(bn.icon_name, 'good') then
                color = bat_color.good
            elseif string.match(bn.icon_name, 'low') then
                color = bat_color.low
            else
                color = bat_color.empty
            end
            pp.widget:get_children_by_id('txt')[1].markup = '<span font_size="large" weight="bold">Status:</span>\r'
                .. sm('Level', level, color)
                .. sm('State', bn.state, color)
                .. sm('Time Left', toTimeStr(bn.state == 'Discharging' and bn.time_to_empty or bn.time_to_full), color)
                .. sm('Update Time', os.date('%x %X', bn.update_time))
                .. sm('Energy', bn.energy .. ' Wh')
                .. sm('Energy Empty', bn.energy_empty .. ' Wh')
                .. sm('Energy Full', bn.energy_full .. ' Wh')
                .. sm('Energy Full Design', bn.energy_full_design .. ' Wh')
                .. sm('Energy Rate', bn.energy_rate .. ' W')
                .. sm('Voltage', bn.voltage .. ' V')
                .. sm('Temperature', bn.temperature > 0 and (bn.temperature .. ' °C') or 'Unavailable')
                .. sm('Capacity', string.format('%.4f%%', bn.capacity))
                .. '\r<span font_size="large" weight="bold">Data:</span>\r'
                .. sm('Vendor', bn.vendor)
                .. sm('Model', bn.model)
                .. sm('Serial', bn.serial)
                .. sm('Technology', bn.technology)
            w:get_children_by_id('icon')[1].image = gears.color.recolor_image(img_path .. bn.icon_name .. '.svg', color)
        end
    end

    awesome.connect_signal('upower::update', function(bn)
        update(bn)
    end)
    signal.setup()

    w:connect_signal('mouse::enter', function()
        pp:move_next_to(mouse.current_widget_geometry)
        pp.visible = true
    end)
    w:connect_signal('mouse::leave', function()
        pp.visible = false
    end)
    return w
end

return setmetatable(_M, {
    __call = function(_)
        return setup()
    end,
})
