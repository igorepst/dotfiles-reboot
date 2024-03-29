---------------------------------------------------------------------------
-- @author Aleksey Fedotov &lt;lexa@cfotr.com&gt;
-- @copyright 2015 Aleksey Fedotov
-- @widgetmod awful.widget.keyboardlayout

-- Modified by Igor Epstein, 10/08/21
---------------------------------------------------------------------------

local capi = { awesome = awesome }
local gdebug = require('gears.debug')

local keyboardlayout = { mt = {} }

local xkeyboard_country_code = {
    ['ad'] = true, -- Andorra
    ['af'] = true, -- Afganistan
    ['al'] = true, -- Albania
    ['am'] = true, -- Armenia
    ['ara'] = true, -- Arabic
    ['at'] = true, -- Austria
    ['az'] = true, -- Azerbaijan
    ['ba'] = true, -- Bosnia and Herzegovina
    ['bd'] = true, -- Bangladesh
    ['be'] = true, -- Belgium
    ['bg'] = true, -- Bulgaria
    ['br'] = true, -- Brazil
    ['bt'] = true, -- Bhutan
    ['bw'] = true, -- Botswana
    ['by'] = true, -- Belarus
    ['ca'] = true, -- Canada
    ['cd'] = true, -- Congo
    ['ch'] = true, -- Switzerland
    ['cm'] = true, -- Cameroon
    ['cn'] = true, -- China
    ['cz'] = true, -- Czechia
    ['de'] = true, -- Germany
    ['dk'] = true, -- Denmark
    ['ee'] = true, -- Estonia
    ['epo'] = true, -- Esperanto
    ['es'] = true, -- Spain
    ['et'] = true, -- Ethiopia
    ['eu'] = true, -- EurKey
    ['fi'] = true, -- Finland
    ['fo'] = true, -- Faroe Islands
    ['fr'] = true, -- France
    ['gb'] = true, -- United Kingdom
    ['ge'] = true, -- Georgia
    ['gh'] = true, -- Ghana
    ['gn'] = true, -- Guinea
    ['gr'] = true, -- Greece
    ['hr'] = true, -- Croatia
    ['hu'] = true, -- Hungary
    ['ie'] = true, -- Ireland
    ['il'] = true, -- Israel
    ['in'] = true, -- India
    ['iq'] = true, -- Iraq
    ['ir'] = true, -- Iran
    ['is'] = true, -- Iceland
    ['it'] = true, -- Italy
    ['jp'] = true, -- Japan
    ['ke'] = true, -- Kenya
    ['kg'] = true, -- Kyrgyzstan
    ['kh'] = true, -- Cambodia
    ['kr'] = true, -- Korea
    ['kz'] = true, -- Kazakhstan
    ['la'] = true, -- Laos
    ['latam'] = true, -- Latin America
    ['latin'] = true, -- Latin
    ['lk'] = true, -- Sri Lanka
    ['lt'] = true, -- Lithuania
    ['lv'] = true, -- Latvia
    ['ma'] = true, -- Morocco
    ['mao'] = true, -- Maori
    ['me'] = true, -- Montenegro
    ['mk'] = true, -- Macedonia
    ['ml'] = true, -- Mali
    ['mm'] = true, -- Myanmar
    ['mn'] = true, -- Mongolia
    ['mt'] = true, -- Malta
    ['mv'] = true, -- Maldives
    ['ng'] = true, -- Nigeria
    ['nl'] = true, -- Netherlands
    ['no'] = true, -- Norway
    ['np'] = true, -- Nepal
    ['ph'] = true, -- Philippines
    ['pk'] = true, -- Pakistan
    ['pl'] = true, -- Poland
    ['pt'] = true, -- Portugal
    ['ro'] = true, -- Romania
    ['rs'] = true, -- Serbia
    ['ru'] = true, -- Russia
    ['se'] = true, -- Sweden
    ['si'] = true, -- Slovenia
    ['sk'] = true, -- Slovakia
    ['sn'] = true, -- Senegal
    ['sy'] = true, -- Syria
    ['th'] = true, -- Thailand
    ['tj'] = true, -- Tajikistan
    ['tm'] = true, -- Turkmenistan
    ['tr'] = true, -- Turkey
    ['tw'] = true, -- Taiwan
    ['tz'] = true, -- Tanzania
    ['ua'] = true, -- Ukraine
    ['us'] = true, -- USA
    ['uz'] = true, -- Uzbekistan
    ['vn'] = true, -- Vietnam
    ['za'] = true, -- South Africa
}

local _current
local _layout

local function update_status()
    _current = awesome.xkb_get_layout_group()
    local layout
    if #_layout > 0 then
        -- Please note that the group number reported by xkb_get_layout_group
        -- is lower by one than the group numbers reported by xkb_get_group_names.
        layout = _layout[_current + 1]
    end
    capi.awesome.emit_signal('keyboardlayout::update', layout)
end

local function get_groups_from_group_names(group_names)
    if group_names == nil then
        return nil
    end

    -- Pattern elements to be captured.
    local word_pat = '([%w_]+)'
    local sec_pat = '(%b())'
    local idx_pat = ':(%d)'
    -- Pairs of a pattern and its callback.  In callbacks, set 'group_idx' to 1
    -- and return it if there's no specification on 'group_idx' in the given
    -- pattern.
    local pattern_and_callback_pairs = {
        -- vendor/file(section):group_idx
        ['^' .. word_pat .. '/' .. word_pat .. sec_pat .. idx_pat .. '$'] = function(token, pattern)
            local vendor, file, section, group_idx = string.match(token, pattern)
            return vendor, file, section, group_idx
        end,
        -- vendor/file(section)
        ['^' .. word_pat .. '/' .. word_pat .. sec_pat .. '$'] = function(token, pattern)
            local vendor, file, section = string.match(token, pattern)
            return vendor, file, section, 1
        end,
        -- vendor/file:group_idx
        ['^' .. word_pat .. '/' .. word_pat .. idx_pat .. '$'] = function(token, pattern)
            local vendor, file, group_idx = string.match(token, pattern)
            return vendor, file, nil, group_idx
        end,
        -- vendor/file
        ['^' .. word_pat .. '/' .. word_pat .. '$'] = function(token, pattern)
            local vendor, file = string.match(token, pattern)
            return vendor, file, nil, 1
        end,
        --  file(section):group_idx
        ['^' .. word_pat .. sec_pat .. idx_pat .. '$'] = function(token, pattern)
            local file, section, group_idx = string.match(token, pattern)
            return nil, file, section, group_idx
        end,
        -- file(section)
        ['^' .. word_pat .. sec_pat .. '$'] = function(token, pattern)
            local file, section = string.match(token, pattern)
            return nil, file, section, 1
        end,
        -- file:group_idx
        ['^' .. word_pat .. idx_pat .. '$'] = function(token, pattern)
            local file, group_idx = string.match(token, pattern)
            return nil, file, nil, group_idx
        end,
        -- file
        ['^' .. word_pat .. '$'] = function(token, pattern)
            local file = string.match(token, pattern)
            return nil, file, nil, 1
        end,
    }

    -- Split 'group_names' into 'tokens'.  The separator is "+".
    local tokens = {}
    local _ = string.gsub(group_names, '[^+]+', function(match)
        table.insert(tokens, match)
    end)

    -- For each token in 'tokens', check if it matches one of the patterns in
    -- the array 'pattern_and_callback_pairs', where the patterns are used as
    -- key.  If a match is found, extract captured strings using the
    -- corresponding callback function.  Check if those extracted is country
    -- specific part of a layout.  If so, add it to 'layout_groups'; otherwise,
    -- ignore it.
    local layout_groups = {}
    for i = 1, #tokens do
        for pattern, callback in pairs(pattern_and_callback_pairs) do
            local vendor, file, section, group_idx = callback(tokens[i], pattern)
            if file then
                if not xkeyboard_country_code[file] then
                    break
                end

                if section then
                    section = string.gsub(section, '%(([%w-_]+)%)', '%1')
                end

                table.insert(
                    layout_groups,
                    { vendor = vendor, file = file, section = section, group_idx = tonumber(group_idx) }
                )
                break
            end
        end
    end

    return layout_groups
end

-- Callback for updating list of layouts
local function update_layout()
    _layout = {}
    local layouts = get_groups_from_group_names(awesome.xkb_get_group_names())
    if layouts == nil or layouts[1] == nil then
        gdebug.print_error('Failed to get list of keyboard groups')
        return
    end
    if #layouts == 1 then
        layouts[1].group_idx = 1
    end
    for _, v in ipairs(layouts) do
        _layout[v.group_idx] = { name = v.file, section = v.section }
    end
    update_status()
end

local function set_layout(group_number)
    if (0 > group_number) or (group_number > #_layout) then
        error('Invalid group number: ' .. group_number .. 'expected number from 0 to ' .. #_layout)
        return
    end
    awesome.xkb_set_layout_group(group_number)
end

function keyboardlayout.next_layout()
    set_layout((_current + 1) % (#_layout + 1))
end

function keyboardlayout.setup()
    update_layout()

    capi.awesome.connect_signal('xkb::map_changed', function()
        update_layout()
    end)
    capi.awesome.connect_signal('xkb::group_changed', function()
        update_status()
    end)
end

return keyboardlayout
