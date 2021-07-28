-- vim.opt.shadafile = "NONE"

local ig_modules = {
-- 	"options",
-- 	"pluginList",
-- 	"mappings",
-- 	"utils"
}
for i = 1, #ig_modules, 1 do
	local ok, res = xpcall(require, debug.traceback, ig_modules[i])
	if not (ok) then
		print("Error loading module : " .. ig_modules[i])
		print(res) -- print stack traceback of the error
	end
end

-- vim.opt.shadafile = ""
