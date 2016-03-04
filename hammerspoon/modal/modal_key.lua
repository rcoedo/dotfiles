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

-- local Node = {}
-- function Node.new(parent)
--   local node = {
--     modal = hotkey.modal.new(nil, nil),
--     children = {},
--     parent = parent,
--     bind
--   }
--   bindToNode(node, "escape", function() node.modal:exit() end)
--   return node
-- end

local function bindToNode(node, key, f)
  local keyTable = strToKey(key)
  node.modal:bind(keyTable[1], keyTable[2], f)
  node.children[key] = key
end

local function createNode(parent)
  local node = {modal = hotkey.modal.new(nil, nil), children = {}, parent = parent}
  bindToNode(node, "escape", function() node.modal:exit() end)
  -- node.modal.exited = function(self) clearModeTooltip() end
  -- node.modal.entered = function(self) drawModeTooltip(i(node.tags)) end
  return node
end

local function registerSequence(tree, sequence, f)
  if sequence[1] then
    local key = table.remove(sequence, 1)
    if not sequence[1] then
      bindToNode(tree, key, function() f() tree.modal:exit() end)
    else
      local childNode = tree.children[key]
      if not childNode then
        local newNode = createNode(tree)
        bindToNode(tree, key, function() tree.modal:exit() newNode.modal:enter() end)
        -- this is overriding the shorcut. sometimes we store a modal, sometimes a tag. that's wrong.
        tree.children[key] = newNode
      end
      registerSequence(tree.children[key], sequence, f)
    end
  end
end

local Modal = {}
function Modal.new()
  local tree = createNode()

  local self = {}
  function self.register(sequence, f)
    registerSequence(tree, fn.split(sequence, " "), f)
    return tree
  end

  return self
end

return Modal
