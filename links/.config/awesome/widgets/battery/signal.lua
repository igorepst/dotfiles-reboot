local upower = require('lgi').require('UPowerGlib')
local awesome = require('awesome')

local _M = {}

local upower_tech = {
    [upower.DeviceTechnology.UNKNOWN] = 'Unknown',
    [upower.DeviceTechnology.LITHIUM_ION] = 'Lithium Ion',
    [upower.DeviceTechnology.LITHIUM_POLYMER] = 'Lithium Polymer',
    [upower.DeviceTechnology.LITHIUM_IRON_PHOSPHATE] = 'Lithium Iron Phosphate',
    [upower.DeviceTechnology.LEAD_ACID] = 'Lead Acid',
    [upower.DeviceTechnology.NICKEL_CADMIUM] = 'Nickel Cadmium',
    [upower.DeviceTechnology.NICKEL_METAL_HYDRIDE] = 'Nickel Metal Hydride',
    [upower.DeviceTechnology.LAST] = 'Last',
}

local upower_level = {
    [upower.DeviceLevel.UNKNOWN] = 'Unknown',
    [upower.DeviceLevel.NONE] = 'None',
    [upower.DeviceLevel.DISCHARGING] = 'Discharging',
    [upower.DeviceLevel.LOW] = 'Low',
    [upower.DeviceLevel.CRITICAL] = 'Critical',
    [upower.DeviceLevel.ACTION] = 'Action',
    [upower.DeviceLevel.NORMAL] = 'Normal',
    [upower.DeviceLevel.HIGH] = 'High',
    [upower.DeviceLevel.FULL] = 'Full',
    [upower.DeviceLevel.LAST] = 'Last',
}

local upower_status = {
    [upower.DeviceState.UNKNOWN] = 'Unknown',
    [upower.DeviceState.CHARGING] = 'Charging',
    [upower.DeviceState.DISCHARGING] = 'Discharging',
    [upower.DeviceState.EMPTY] = 'Empty',
    [upower.DeviceState.FULLY_CHARGED] = 'Full',
    [upower.DeviceState.PENDING_CHARGE] = 'Pending Charge',
    [upower.DeviceState.PENDING_DISCHARGE] = 'Pending Discharge',
    [upower.DeviceState.LAST] = 'Last',
}

local upower_kind = {
    [upower.DeviceKind.UNKNOWN] = 'Unknown',
    [upower.DeviceKind.LINE_POWER] = 'Line Power',
    [upower.DeviceKind.BATTERY] = 'Battery',
    [upower.DeviceKind.UPS] = 'UPS',
    [upower.DeviceKind.MONITOR] = 'Monitor',
    [upower.DeviceKind.MOUSE] = 'Mouse',
    [upower.DeviceKind.KEYBOARD] = 'Keyboard',
    [upower.DeviceKind.PDA] = 'PDA',
    [upower.DeviceKind.PHONE] = 'Phone',
    [upower.DeviceKind.MEDIA_PLAYER] = 'Media Player',
    [upower.DeviceKind.TABLET] = 'Tablet',
    [upower.DeviceKind.COMPUTER] = 'Computer',
    [upower.DeviceKind.GAMING_INPUT] = 'Gaming Input',
    [upower.DeviceKind.LAST] = 'Last',
}

local update = function(devices)
    local upd = {}
    for _, device in ipairs(devices) do
        local bn = {}
        bn.object_path = device:get_object_path()
        bn.battery_level = upower_level[device.battery_level]
        bn.kind = upower_kind[device.kind]
        bn.icon_name = device.icon_name
        bn.luminosity = device.luminosity
        bn.model = device.model
        bn.native_path = device.native_path
        if bn.kind == upower_kind[upower.DeviceKind.BATTERY] then
            bn.energy = device.energy
            bn.energy_empty = device.energy_empty
            bn.energy_full = device.energy_full
            bn.energy_full_design = device.energy_full_design
            bn.energy_rate = device.energy_rate
            bn.time_to_empty = device.time_to_empty
            bn.time_to_full = device.time_to_full
            bn.is_present = device.is_present
            bn.is_rechargeable = device.is_rechargeable
            bn.state = upower_status[device.state]
            bn.capacity = device.capacity
            bn.technology = upower_tech[device.technology]
            if bn.battery_level == upower_level[upower.DeviceLevel.NONE] then
                bn.percentage = device.percentage
            end
        elseif bn.kind == upower_kind[upower.DeviceKind.LINE_POWER] then
            bn.online = device.online
        end
        bn.power_supply = device.power_supply
        bn.serial = device.serial
        bn.temperature = device.temperature
        bn.update_time = device.update_time
        bn.vendor = device.vendor
        bn.voltage = device.voltage
        bn.warning_level = upower_level[device.warning_level]

        table.insert(upd, bn)
    end
    awesome.emit_signal('upower::update', upd)
end

_M.setup = function()
    local devTable = {}
    for _, device in ipairs(upower.Client():get_devices()) do
        if string.match(device:get_object_path(), '/org/freedesktop/UPower/devices/battery') then
            table.insert(devTable, device)
            device.on_notify = function()
                update({ device })
            end
        end
    end
    update(devTable)
end

return _M
