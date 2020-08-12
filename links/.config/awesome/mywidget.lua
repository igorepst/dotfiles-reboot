local mywidget = {}
local lain = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local tonumber = tonumber

local image_path = "/usr/share/icons/breeze/status/22/"
local batstat = {}
mywidget.bat_icon = wibox.widget.imagebox()
mywidget.mybattery = lain.widget.bat({
    settings = function()
        local postfix = ""
        if bat_now.ac_status and bat_now.ac_status == 1 then 
            postfix = "-charging"
        end
        local icon = "missing"
        local perc = tonumber(bat_now.perc)
        if perc then
            if perc <= 5 then
                icon = "000"
            elseif perc <= 15 then
                icon = "010"
            elseif perc <= 25 then
                icon = "020"
            elseif perc <= 35 then
                icon = "030"
            elseif perc <= 45 then
                icon = "040"
            elseif perc <= 55 then
                icon = "050"
            elseif perc <= 65 then
                icon = "060"
            elseif perc <= 75 then
                icon = "070"
            elseif perc <= 85 then
                icon = "080"
            elseif perc <= 99 then
                icon = "090"
            elseif perc == 100 then
                icon = "100"
            end
        end
        mywidget.bat_icon:set_image(image_path .. "battery-" .. icon .. postfix .. ".svg")
        batstat = bat_now
    end
})

local mybattery_t = awful.tooltip {
    objects = { mywidget.bat_icon },
    timer_function = function()
        local msg = ""
        for i = 1, #batstat.n_status do
            msg = msg .. lain.util.markup.font("monospace 10",
            string.format("┌[Battery %d]\n├Status:\t%s\n└Percentage:\t%s\n\n",
            i-1, batstat.n_status[i], batstat.n_perc[i]))
        end
        return msg .. lain.util.markup.font("monospace 10", "Time left:\t" .. batstat.time)
    end
}

mywidget.wifi_icon = wibox.widget.imagebox()
-- local eth_icon = wibox.widget.imagebox()
mywidget.net = lain.widget.net({
    notify = "off",
    wifi_state = "on",
    eth_state = "off",
    settings = function()
        -- local eth0 = net_now.devices.eth0
        -- if eth0 then
        -- if eth0.ethernet then
        -- eth_icon:set_image(ethernet_icon_filename)
        -- else
        -- eth_icon:set_image()
        -- end
        -- end

        local wlan0 = net_now.devices.wlo1
        local icon = "network-wireless-disconnected" 
        if wlan0 then
            if wlan0.wifi then
                local signal = wlan0.signal
                if signal < -83 then
                    icon = "network-wireless-signal-weak"
                elseif signal < -70 then
                    icon = "network-wireless-signal-ok"
                elseif signal < -53 then
                    icon = "network-wireless-signal-good"
                elseif signal >= -53 then
                    icon = "network-wireless-signal-excellent"
                end
            end
        end
        mywidget.wifi_icon:set_image(image_path .. icon .. ".svg")
    end
})

mywidget.volume_icon = wibox.widget.imagebox()
mywidget.volume = lain.widget.pulse({settings = function()
    local icon
    if volume_now.muted == "yes" then
        icon = "audio-volume-muted"
    else
        local perc = tonumber(volume_now.left)
        if perc then
            if perc <= 25 then
                icon = "audio-volume-low"
            elseif perc <= 80 then
                icon = "audio-volume-medium"
            else
                icon = "audio-volume-high"
            end
        end
        vlevel = volume_now.left .. "-" .. volume_now.right .. "% | " .. volume_now.device
    end
        mywidget.volume_icon:set_image(image_path .. icon .. ".svg")
    end
})
mywidget.volume_icon:buttons(awful.util.table.join(
    awful.button({}, 1, function() -- left click
        awful.spawn("pavucontrol")
    end),
    awful.button({}, 2, function() -- middle click
        os.execute(string.format("pactl set-sink-volume %s 100%%", mywidget.volume.device))
        mywidget.volume.update()
    end),
    awful.button({}, 3, function() -- right click
        os.execute(string.format("pactl set-sink-mute %s toggle", mywidget.volume.device))
        mywidget.volume.update()
    end),
    awful.button({}, 4, function() -- scroll up
        os.execute(string.format("pactl set-sink-volume %s +1%%", mywidget.volume.device))
        mywidget.volume.update()
    end),
    awful.button({}, 5, function() -- scroll down
        os.execute(string.format("pactl set-sink-volume %s -1%%", mywidget.volume.device))
        mywidget.volume.update()
    end)
))

return mywidget
