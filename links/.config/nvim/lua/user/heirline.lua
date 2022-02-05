local conditions = require('heirline.conditions')
local utils = require('heirline.utils')

local c = require('igTermColors').getColors()
local colors = {
    fg = c.foreground,
    bg = '#e4e4e4',
    black = c.color0,
    skyblue = c.color12,
    cyan = c.color6,
    green = c.color2,
    oceanblue = c.color4,
    magenta = c.color5,
    orange = '#FF9000',
    red = c.color9,
    violet = '#9E93E8',
    white = c.color15,
    yellow = c.color3,
}

local statusline = {}

require('heirline').setup(statusline)
