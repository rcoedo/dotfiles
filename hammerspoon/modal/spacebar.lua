local Node = require "modal/node"

local log = hs.logger.new("debug", "debug")
local i = hs.inspect.inspect

local fn = require "hs.fnutils"

local styledtext = require "hs.styledtext"
local drawing = require "hs.drawing"
local geometry = require "hs.geometry"
local screen = require "hs.screen"
local template = require "resty.template"

local function buildStyledText(labels)
  local view = template.compile("modal/layout.html")
  return styledtext.getStyledTextFromData(view{labels = labels})
end

local function buildTextDrawing(labels)
  local styledText = buildStyledText(labels)
  local frame = screen.primaryScreen():frame()
  return drawing.text({ x = 15, y = 10, w = frame.w, h = frame.h }, styledText)
end

local function buildTooltip(labels)
  local text = buildTextDrawing(labels)
  local size = drawing.getTextDrawingSize(text:getStyledText())
  local frame = screen.primaryScreen():frame()
  local background = drawing.rectangle({ x = -2, y = 0, w = frame.w + 4, h = size.h })
  background:setFillColor({red = 0, green = 0, blue = 0, alpha=0.80})
  return {
    text = text,
    background = background
  }
end

local function getLabels(node)
  local labels = node.getMetadata()["labels"]
  return labels == nil and {} or labels
end

local Spacebar = {}
function Spacebar.new(config)
  local _tooltip = nil
  local _root = Node.new()

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

  function self.clearModeTooltip()
    if _tooltip then
      _tooltip.background:delete()
      _tooltip.text:delete()
    end
  end

  function self.drawModeTooltip(labels)
    _tooltip = buildTooltip(labels)
    _tooltip.background:show()
    _tooltip.text:show()
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

  _root.listen("transition", function(event)
    self.clearModeTooltip()
    self.drawModeTooltip(getLabels(event.getData()))
  end)

  _root.listen("exit", function() self.clearModeTooltip() end)

  if config ~= nil then
    init(config)
  end

  return self
end

return Spacebar
