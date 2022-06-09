local awful = require('awful')

local config = require('conf.config')
local modkey = config.modkey

awful.keyboard.append_global_keybindings({
      awful.key({ modkey }, 's', function()
	    local hotkeys_popup = require('awful.hotkeys_popup')
	    require('awful.hotkeys_popup.keys').tmux.add_rules_for_terminal({ rule = { name = { "tmux" }}})
	    hotkeys_popup.show_help({}, nil, {show_awesome_keys=true})
      end, { description = 'show help', group = 'awesome' }),
    awful.key({ modkey, 'Control' }, 'r', awesome.restart, { description = 'reload awesome', group = 'awesome' }),
    awful.key({ modkey, 'Shift' }, 'q', awesome.quit, { description = 'quit awesome', group = 'awesome' }),
    awful.key({ modkey }, 'Return', function()
        awful.spawn(config.terminal)
    end, {
        description = 'open a terminal',
        group = 'launcher',
    }),
    awful.key({ modkey }, 'r', function()
        awful.spawn('rofiLauncher')
    end, {
        description = 'run Rofi',
        group = 'launcher',
    }),
    awful.key({ modkey }, 'e', function()
        awful.spawn('tfm')
    end, {
        description = 'run file explorer',
        group = 'launcher',
    }),
     awful.key({ modkey }, 'a', function()
        awful.spawn('visual')
    end, {
        description = 'run visual editor',
        group = 'launcher',
    }),
    awful.key({}, 'XF86PowerOff', function()
        local logoutw = require('widgets.logout.widget')
        logoutw.setup()
        logoutw.show()
    end, {
        description = 'Show logout screen',
        group = 'custom',
    }),
    awful.key({ modkey }, 'Left', awful.tag.viewprev, { description = 'view previous', group = 'tag' }),
    awful.key({ modkey }, 'Right', awful.tag.viewnext, { description = 'view next', group = 'tag' }),
    awful.key({ modkey }, 'Escape', awful.tag.history.restore, { description = 'go back', group = 'tag' }),
    awful.key({ modkey }, 'j', function()
        awful.client.focus.byidx(1)
    end, {
        description = 'focus next by index',
        group = 'client',
    }),
    awful.key({ modkey }, 'k', function()
        awful.client.focus.byidx(-1)
    end, {
        description = 'focus previous by index',
        group = 'client',
    }),
    awful.key({ modkey }, 'Tab', function()
	  local t = awful.screen.focused().selected_tag
	  if not t then return end
	  local clients = t:clients()
	  if not clients then return end
	  local f, ind = client.focus, 1
	  if f then
	     for i, cc in ipairs(clients) do
		if cc == f then
		   ind = i + 1
		   break
		end
	     end
	  end
	  if ind > #clients then ind = 1 end
	  local c = clients[ind]
	  if c then
	     c:activate({context = "unminimize",raise = true})
	  end
    end, {
        description = 'go back',
        group = 'client'
    }),
    awful.key({ modkey, 'Control' }, 'j', function()
        awful.screen.focus_relative(1)
    end, {
        description = 'focus the next screen',
        group = 'screen',
    }),
    awful.key({ modkey, 'Control' }, 'k', function()
        awful.screen.focus_relative(-1)
    end, {
        description = 'focus the previous screen',
        group = 'screen',
    }),
    awful.key({ modkey, 'Control' }, 'n', function()
        local c = awful.client.restore()
        -- Focus restored client
        if c then
            c:activate({ raise = true, context = 'key.unminimize' })
        end
    end, {
        description = 'restore minimized',
        group = 'client',
    }),
    awful.key({ modkey, 'Shift' }, 'j', function()
        awful.client.swap.byidx(1)
    end, {
        description = 'swap with next client by index',
        group = 'client',
    }),
    awful.key({ modkey, 'Shift' }, 'k', function()
        awful.client.swap.byidx(-1)
    end, {
        description = 'swap with previous client by index',
        group = 'client',
    }),
    awful.key({ modkey }, 'u', awful.client.urgent.jumpto, { description = 'jump to urgent client', group = 'client' }),
    awful.key({ modkey }, 'l', function()
        awful.tag.incmwfact(0.05)
    end, {
        description = 'increase master width factor',
        group = 'layout',
    }),
    awful.key({ modkey }, 'h', function()
        awful.tag.incmwfact(-0.05)
    end, {
        description = 'decrease master width factor',
        group = 'layout',
    }),
    awful.key({ modkey, 'Shift' }, 'h', function()
        awful.tag.incnmaster(1, nil, true)
    end, {
        description = 'increase the number of master clients',
        group = 'layout',
    }),
    awful.key({ modkey, 'Shift' }, 'l', function()
        awful.tag.incnmaster(-1, nil, true)
    end, {
        description = 'decrease the number of master clients',
        group = 'layout',
    }),
    awful.key({ modkey, 'Control' }, 'h', function()
        awful.tag.incncol(1, nil, true)
    end, {
        description = 'increase the number of columns',
        group = 'layout',
    }),
    awful.key({ modkey, 'Control' }, 'l', function()
        awful.tag.incncol(-1, nil, true)
    end, {
        description = 'decrease the number of columns',
        group = 'layout',
    }),
    awful.key({ modkey }, 'space', function()
        awful.layout.inc(1)
    end, {
        description = 'select next',
        group = 'layout',
    }),
    awful.key({ modkey, 'Shift' }, 'space', function()
        awful.layout.inc(-1)
    end, {
        description = 'select previous',
        group = 'layout',
    }),
    awful.key({
        modifiers = { modkey },
        keygroup = 'numrow',
        description = 'only view tag',
        group = 'tag',
        on_press = function(index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                tag:view_only()
            end
        end,
    }),
    awful.key({
        modifiers = { modkey, 'Control' },
        keygroup = 'numrow',
        description = 'toggle tag',
        group = 'tag',
        on_press = function(index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                awful.tag.viewtoggle(tag)
            end
        end,
    }),
    awful.key({
        modifiers = { modkey, 'Shift' },
        keygroup = 'numrow',
        description = 'move focused client to tag',
        group = 'tag',
        on_press = function(index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:move_to_tag(tag)
                end
            end
        end,
    }),
    awful.key({
        modifiers = { modkey, 'Control', 'Shift' },
        keygroup = 'numrow',
        description = 'toggle focused client on tag',
        group = 'tag',
        on_press = function(index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:toggle_tag(tag)
                end
            end
        end,
    }),
    awful.key({
        modifiers = { modkey },
        keygroup = 'numpad',
        description = 'select layout directly',
        group = 'layout',
        on_press = function(index)
            local t = awful.screen.focused().selected_tag
            if t then
                t.layout = t.layouts[index] or t.layout
            end
        end,
    }),
})

awful.mouse.append_global_mousebindings({
    awful.button({}, 4, awful.tag.viewprev),
    awful.button({}, 5, awful.tag.viewnext),
})
