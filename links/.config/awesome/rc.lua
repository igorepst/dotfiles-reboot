pcall(require, 'luarocks.loader')

require('awful.autofocus')
require('beautiful').init(require('gears').filesystem.get_configuration_dir() .. 'theme/gtk/theme.lua')
require('conf')

-- Move / resize floaters
-- awful.key({ modkey }, "Next",  function (c) c:relative_move( 20,  20, -40, -40) end),
-- awful.key({ modkey }, "Prior", function (c) c:relative_move(-20, -20,  40,  40) end),
-- awful.key({ modkey }, "Down",  function (c) c:relative_move(  0,  20,   0,   0) end),
-- awful.key({ modkey }, "Up",    function (c) c:relative_move(  0, -20,   0,   0) end),
-- awful.key({ modkey }, "Left",  function (c) c:relative_move(-20,   0,   0,   0) end),
-- awful.key({ modkey }, "Right", function (c) c:relative_move( 20,   0,   0,   0) end),
-- Move / resize tiled
-- awful.key({ modkey }, "l",          function () awful.tag.incmwfact( 0.05) end),
-- awful.key({ modkey }, "h",          function () awful.tag.incmwfact(-0.05) end),
-- awful.key({ modkey, "Shift" }, "l", function () awful.client.incwfact(-0.05) end),
-- awful.key({ modkey, "Shift" }, "h", function () awful.client.incwfact( 0.05) end),
