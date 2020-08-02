
local mykbdlayout = {}
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local capi = {awesome = awesome}

local image_path = gears.filesystem.get_configuration_dir() .. '/theme/kbd-layout/'
local settings = {"us","ru","il"}

mykbdlayout.kbd_icon = wibox.widget.imagebox()

local function update_status()
    local num = capi.awesome.xkb_get_layout_group()
    mykbdlayout.kbd_icon:set_image(image_path .. settings[num + 1] .. ".png")
end

local function next_layout()
    local num = capi.awesome.xkb_get_layout_group()
    capi.awesome.xkb_set_layout_group((num + 1) % (#settings + 1)) 
end

mykbdlayout.kbd_icon:buttons(gears.table.join(awful.button({ }, 1, next_layout)))
capi.awesome.connect_signal("xkb::group_changed", update_status)
update_status()

return mykbdlayout
