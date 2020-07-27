local mywidget = {}
local lain = require("lain")
local awful = require("awful")
local wibox = require("wibox")
-- local markup     = lain.util.markup
-- local theme                                     = {}
-- theme.font                                      = "Terminus 10.5"
-- local gray       = "#9E9C9A"
local batstat = {}
mywidget.mybattery = lain.widget.bat({
    settings = function()
        bat_header = " Bat "
        bat_p      = bat_now.perc .. "% "
        -- bat_t      = bat_now.time .. " "
        -- bat_s      = (bat_now.status or "N/A/ ") .. " "
        -- widget:set_markup(markup.font(theme.font, markup(gray, bat_header) .. bat_p .. bat_t .. bat_s))
        widget.text = bat_header .. bat_p
        batstat = bat_now
    end
})

local mybattery_t = awful.tooltip {
    objects = { mywidget.mybattery.widget },
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
mywidget.net = lain.widget.net {
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
        local wifi_icon = mywidget.wifi_icon
        if wlan0 then
            if wlan0.wifi then
                local signal = wlan0.signal
                if signal < -83 then
                    wifi_icon:set_image("/usr/share/icons/breeze/status/22/network-wireless-signal-weak.svg")
                elseif signal < -70 then
                    wifi_icon:set_image("/usr/share/icons/breeze/status/22/network-wireless-signal-ok.svg")
                elseif signal < -53 then
                    wifi_icon:set_image("/usr/share/icons/breeze/status/22/network-wireless-signal-good.svg")
                elseif signal >= -53 then
                    wifi_icon:set_image("/usr/share/icons/breeze/status/22/network-wireless-signal-excellent.svg")
                end
            else
                    wifi_icon:set_image("/usr/share/icons/breeze/status/22/network-wireless-signal-none.svg")
            end
        else 
                    wifi_icon:set_image("/usr/share/icons/breeze/status/22/network-wireless-signal-none.svg")
        end
    end
}

return mywidget
