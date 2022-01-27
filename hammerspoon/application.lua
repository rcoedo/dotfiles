local window = require "hs.window"
local eventtap = require "hs.eventtap"
local appfinder = require "hs.appfinder"

local function launchOrSwitch(name, realName)
    if (window.frontmostWindow() ~= null and window.frontmostWindow():application() == appfinder.appFromName(name)) then
        eventtap.keyStroke({"cmd"}, "`")
    else
        local app = appfinder.appFromName(realName or name)
        if (app == null) then
          hs.application.launchOrFocus(realName or name)
        else
          app:activate()
        end
    end
end

return {
   launchOrFocus = hs.application.launchOrFocus,
   launchOrSwitch = launchOrSwitch
}
