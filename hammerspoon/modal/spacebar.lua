local Node = require "modal/node"

-- local styledtext = require "hs.styledtext"
-- local drawing = require "hs.drawing"
-- local geometry = require "hs.geometry"
--
-- local function ansiMessage(message)
--   return styledtext.ansi(message, {font={name="Fantasque Sans Mono",size=36}, backgroundColor={alpha=1}})
-- end
--
-- local function clearModeTooltip()
--   if modeStatus then
--     modeStatus:delete()
--   end
-- end
--
-- local function drawModeTooltip(message)
--   clearModeTooltip()
--   modeStatus = drawing.text(geometry.rect(100,100,1000,1000), ansiMessage(message))
--   modeStatus:show()
-- end

local Spacebar = {}
function Spacebar.new(prefix)
  local _prefix = prefix == nil and "" or prefix
  local _root = Node.new()

  local self = {}
  function self.register(sequence, f)
    local prefixedSequence = prefix .. " " .. sequence
    _root.register(prefixedSequence)
    _root.listen(prefixedSequence, "sequence", f)
    _root.enter()
  end

  return self
end

return Spacebar
