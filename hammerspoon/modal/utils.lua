local fn = require "hs.fnutils"

local function strToKey(str)
  local mods = fn.split(str, "-")
  local key = table.remove(mods)
  return {mods, key}
end

return { strToKey = strToKey }
