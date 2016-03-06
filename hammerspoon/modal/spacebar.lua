local Node = require "modal/node"

local log = hs.logger.new("debug", "debug")
local i = hs.inspect.inspect

local styledtext = require "hs.styledtext"
local drawing = require "hs.drawing"
local geometry = require "hs.geometry"

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

local function getTag(node)
  local tag = node.getMetadata()["tag"]
  return tag == nil and "" or tag
end

local Spacebar = {}
function Spacebar.new()
  local _root = Node.new()

  _root.listen("transition", function(node) drawModeTooltip(getTag(node)) end)
  _root.listen("exit", function() clearModeTooltip() end)

  local self = {}
  function self.register(sequence, f, meta)
    meta = meta == nil and {} or meta
    _root.register(sequence)

    local node = _root.findNode(sequence)

    node.listen("sequence", function() f() end)
        .addMetadata("tag", meta["tag"])
    --
    -- if meta["modal"] == "stay" then
    --
    -- end

    _root.enter()
  end

  function self.addTag(sequence, tag)
    _root.findNode(sequence)
      .addMetadata("tag", tag)
  end

  return self
end

return Spacebar
