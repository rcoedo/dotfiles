local hotkey = require("hs.hotkey")
local fn = require("hs.fnutils")

local log = hs.logger.new('mymodule','debug')
local i = hs.inspect.inspect
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

local function strToKey(str)
  local mods = fn.split(str, "-")
  local key = table.remove(mods)
  return {mods, key}
end

local Node = {}
function Node.new(parent)
  local children = {}
  local parent = parent
  local modal = hotkey.modal.new(nil, nil)

  local self = {}

  function self.enter()
    modal:enter()
  end

  function self.exit()
    modal:exit()
  end

  function self.propagate(event)
    if parent then
      parent.handle(event)
    end
  end

  function self.handle(event)
    hs.alert(event)
    self.propagate(event)
  end

  function self.bind(key, f)
    local keyTable = strToKey(key)
    modal:bind(keyTable[1], keyTable[2], f)
    children[key] = key
  end

  function self.register(sequence, f)
    if sequence[1] then
      local key = table.remove(sequence, 1)
      if not sequence[1] then
        self.bind(key, function() f() self.exit() end)
      else
        local childNode = children[key]
        if not childNode then
          local newNode = Node.new(self)
          self.bind(key, function() self.exit() newNode.enter() end)
          children[key] = newNode
        end
        children[key].register(sequence, f)
      end
    end
  end

  self.bind("escape", function() self.exit() end)

  return self
end

local Modal = {}
function Modal.new()
  local root = Node.new()

  local self = {}
  function self.register(sequence, f)
    root.register(fn.split(sequence, " "), f)
    return root
  end

  return self
end

return Modal
