local wbox = require('wibox.widget.imagebox')
local widget_base = require('wibox.widget.base')
local button = require('awful.button')
local gears = require('gears')
local signal = require('widgets.keyboardlayout.signal')

local keyboardlayout = {}

local img_path = gears.filesystem.get_configuration_dir() .. 'widgets/keyboardlayout/flags/'

local setup = function()
    local widget = wbox()
    widget.valign = 'center'
    widget.resize = false
    local self = widget_base.make_widget(widget, nil, { enable_properties = true })
    self.widget = widget
    self.buttons = {
        button({}, 1, signal.next_layout),
    }

    local update = function(upd)
        widget.image = upd.name and img_path .. upd.name .. '.png' or nil
    end
    awesome.connect_signal('keyboardlayout::update', function(bn)
        update(bn)
    end)
    signal.setup()
    return self
end

return setmetatable(keyboardlayout, {
    __call = function(_)
        return setup()
    end,
})
