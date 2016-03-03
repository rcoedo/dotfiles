local styledtext = require "hs.styledtext"
local drawing = require "hs.drawing"
local geometry = require "hs.geometry"
local hotkey = require "hs.hotkey"

local function ansiMessage(message)
  return styledtext.ansi(message, {font={name="Fantasque Sans Mono",size=36}, backgroundColor={alpha=1}})
end

local function clearModeTooltip()
  if modeStatus then
    modeStatus:delete()
  end
end

local function drawModeTooltip(message)
  clearModeTooltip()
  modeStatus = drawing.text(geometry.rect(100,100,1000,1000), ansiMessage(message))
  modeStatus:show()
end

local function new(mod, key, message)
  local modal = hotkey.modal.new(mod, key)
  modal:bind({}, "escape", function() modal:exit() end)
  modal.exited = function(self) clearModeTooltip() end
  modal.entered = function(self) drawModeTooltip(message) end
  return modal
end

function hs.hotkey.modal:quit(message)
  self:exit()
  hs.alert(message, 1)
end

return { new = new }
