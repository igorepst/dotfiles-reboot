---------------------------
-- Default awesome theme --
---------------------------

local theme_assets = require('beautiful.theme_assets')
local xresources = require('beautiful.xresources')
local rnotification = require('ruled.notification')
local dpi = xresources.apply_dpi

local gfs = require('gears.filesystem')
local themes_path = gfs.get_configuration_dir() .. 'theme/mine/'

local theme = {}

theme.font = 'DejaVu Sans Mono Nerd Font Complete Mono 10'
theme.font_bold = 'DejaVu Sans Mono Bold Nerd Font Complete Mono Bold 10'
theme.menu_font = 'DejaVu Sans Mono Nerd Font Complete Mono 13'

theme.bg_normal = '#fffffa'
theme.bg_focus = '#bbbbbb'
theme.bg_urgent = '#ff0000'
theme.bg_minimize = theme.bg_normal
theme.bg_systray = theme.bg_normal

theme.fg_normal = '#282828'
theme.fg_focus = '#24292e'
theme.fg_urgent = '#ffffff'
theme.fg_minimize = theme.fg_normal

theme.useless_gap = dpi(0)
theme.border_width = dpi(1)
theme.border_color_normal = '#000000'
theme.border_color_active = '#535d6c'
theme.border_color_marked = '#91231c'

theme.dark_green = '#4e9a05'

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Generate taglist squares:
local taglist_square_size = dpi(8)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(taglist_square_size, theme.dark_green)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(taglist_square_size, theme.fg_normal)
theme.taglist_font = 'DejaVuSansMono Nerd Font Mono 18'

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path .. 'submenu.png'
theme.menu_height = dpi(20)
theme.menu_width = dpi(150)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = themes_path .. 'titlebar/close_normal.png'
theme.titlebar_close_button_focus = themes_path .. 'titlebar/close_focus.png'

theme.titlebar_minimize_button_normal = themes_path .. 'titlebar/minimize_normal.png'
theme.titlebar_minimize_button_focus = themes_path .. 'titlebar/minimize_focus.png'

theme.titlebar_ontop_button_normal_inactive = themes_path .. 'titlebar/ontop_normal_inactive.png'
theme.titlebar_ontop_button_focus_inactive = themes_path .. 'titlebar/ontop_focus_inactive.png'
theme.titlebar_ontop_button_normal_active = themes_path .. 'titlebar/ontop_normal_active.png'
theme.titlebar_ontop_button_focus_active = themes_path .. 'titlebar/ontop_focus_active.png'

theme.titlebar_sticky_button_normal_inactive = themes_path .. 'titlebar/sticky_normal_inactive.png'
theme.titlebar_sticky_button_focus_inactive = themes_path .. 'titlebar/sticky_focus_inactive.png'
theme.titlebar_sticky_button_normal_active = themes_path .. 'titlebar/sticky_normal_active.png'
theme.titlebar_sticky_button_focus_active = themes_path .. 'titlebar/sticky_focus_active.png'

theme.titlebar_floating_button_normal_inactive = themes_path .. 'titlebar/floating_normal_inactive.png'
theme.titlebar_floating_button_focus_inactive = themes_path .. 'titlebar/floating_focus_inactive.png'
theme.titlebar_floating_button_normal_active = themes_path .. 'titlebar/floating_normal_active.png'
theme.titlebar_floating_button_focus_active = themes_path .. 'titlebar/floating_focus_active.png'

theme.titlebar_maximized_button_normal_inactive = themes_path .. 'titlebar/maximized_normal_inactive.png'
theme.titlebar_maximized_button_focus_inactive = themes_path .. 'titlebar/maximized_focus_inactive.png'
theme.titlebar_maximized_button_normal_active = themes_path .. 'titlebar/maximized_normal_active.png'
theme.titlebar_maximized_button_focus_active = themes_path .. 'titlebar/maximized_focus_active.png'

theme.wallpaper = themes_path .. 'background.jpg'

theme.layout_fairh = themes_path .. 'layouts/fairhw.png'
theme.layout_fairv = themes_path .. 'layouts/fairvw.png'
theme.layout_floating = themes_path .. 'layouts/floatingw.png'
theme.layout_magnifier = themes_path .. 'layouts/magnifierw.png'
theme.layout_max = themes_path .. 'layouts/maxw.png'
theme.layout_fullscreen = themes_path .. 'layouts/fullscreenw.png'
theme.layout_tilebottom = themes_path .. 'layouts/tilebottomw.png'
theme.layout_tileleft = themes_path .. 'layouts/tileleftw.png'
theme.layout_tile = themes_path .. 'layouts/tilew.png'
theme.layout_tiletop = themes_path .. 'layouts/tiletopw.png'
theme.layout_spiral = themes_path .. 'layouts/spiralw.png'
theme.layout_dwindle = themes_path .. 'layouts/dwindlew.png'
theme.layout_cornernw = themes_path .. 'layouts/cornernww.png'
theme.layout_cornerne = themes_path .. 'layouts/cornernew.png'
theme.layout_cornersw = themes_path .. 'layouts/cornersww.png'
theme.layout_cornerse = themes_path .. 'layouts/cornersew.png'

theme.awesome_icon = theme_assets.awesome_icon(theme.menu_height, theme.bg_focus, theme.fg_focus)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

-- Set different colors for urgent notifications.
rnotification.connect_signal('request::rules', function()
    rnotification.append_rule({
        rule = { urgency = 'critical' },
        properties = { bg = '#ff0000', fg = '#ffffff' },
    })
end)

theme = theme_assets.recolor_layout(theme, theme.dark_green)
return theme
