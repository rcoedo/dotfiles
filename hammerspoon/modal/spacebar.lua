local Node = require "modal/node"

local log = hs.logger.new("debug", "debug")
local i = hs.inspect.inspect

local fn = require "hs.fnutils"

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

local function drawModeTooltip(labels)
  clearModeTooltip()
  local message = ""
  for k,v in pairs(labels) do
     message = message .. k .. ": " .. v .. "\n"
  end
  modeStatus = drawing.text(geometry.rect(0,0,1000,1000), ansiMessage(message))
  modeStatus:show()
end

local function drawModeTooltip2(labels)
  clearModeTooltip()
  local message = ""
  for k,v in pairs(labels) do
     message = message .. k .. ": " .. v .. "\n"
  end
  modeStatus = hs.webview.new(geometry.rect(0,0,1000,1000)):url("http://www.google.es/"):asHSDrawing()
  modeStatus:show()

end

local function getLabels(node)
  local labels = node.getMetadata()["labels"]
  return labels == nil and {} or labels
end

local Spacebar = {}
function Spacebar.new(config)
  local _root = Node.new()
  _root.listen("transition", function(event) drawModeTooltip(getLabels(event.getData())) end)
  _root.listen("exit", function() clearModeTooltip() end)

  local self = {}
  function self.register(sequence, f, meta)
    meta = meta == nil and {} or meta
    _root.register(sequence)

    local node = _root.findNode(sequence)

    node.listen("sequence", function() f() end)

    if meta["modal"] == "stay" then
       node.listen("exit", function(e) e.stopPropagation() end)
    end

    _root.enter()
  end

  function self.addLabel(sequence, key, tag)
     local node = _root.findNode(sequence)
     if node.getMetadata()["labels"] == nil then
        node.addMetadata("labels", {})
     end
     node.getMetadata()["labels"][key] = tag
  end

  local function init(config)
    local prefix = config.prefix
    fn.map(config.bindings, function(binding)
      local key = binding[1]
      local tag = binding[2]
      local f = binding[3]
      local options = binding[4]
      local sequence = prefix .. " " .. key
      if type(f) == "table" then
         init({prefix = sequence, bindings = f})
      else
         self.register(sequence, f, options)
      end
      self.addLabel(prefix, key, tag)
    end)
  end

  if config ~= nil then
    init(config)
  end

  return self
end

return Spacebar
