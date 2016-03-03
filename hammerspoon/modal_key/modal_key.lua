local utils = require("utils")

local ModalKey = {}

local function parseKey(key)
  return utils.split(key, " ")
end

function ModalKey.new(key)
  local mode = nil
  local modeTooltip = nil

  local function text()
    return hs.styledtext.ansi(mode, {font={name="Fantasque Sans Mono",size=36}, backgroundColor={alpha=1}})
  end

  local function hideModeTooltip()
    if modeTooltip then
      modeTooltip:delete()
    end
  end

  local function showModeTooltip()
    clearModeTooltip()
    modeTooltip = hs.drawing.text(hs.geometry.rect(100,100,1000,1000), text())
    modeTooltip:show()
  end

  local self = {}

  function self.show()
    showModeTooltip()
  end

  function self.hide()
    hideModeTooltip()
  end

  self.parseKey = parseKey

  return self
end

return ModalKey
