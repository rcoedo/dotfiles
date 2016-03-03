local hotkey = require("hs.hotkey")
local fn = require("hs.fnutils")
local log = hs.logger.new('mymodule','debug')
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

local function buildModString(mod)
  -- mods = fn.reduce(mod, function(acc, curr)
  --   return acc == nil and curr or acc .. "+" .. curr
  -- end)
  -- return mods == nil and "" or mods .. "+"
  return ""
end

local function bindToNode(node, shortcut, message, f)
  node.modal:bind(shortcut[1], shortcut[2], function() f() node.modal:exit() end)
  node.children[shortcut] = {shortcut = buildModString(shortcut[1]), key = shortcut[2]}
end

local function createNode(mod, key)
  local node = {modal = hotkey.modal.new(mod, key), children = {}}
  bindToNode(node, {nil, "escape"}, "exit", function() end)
  node.modal.exited = function(self) clearModeTooltip() end
  node.modal.entered = function(self) drawModeTooltip(i(node.tags)) end
  return node
end

local function registerSequence(tree, sequence, f)
    local shortcut = table.remove(sequence, 1)
    if not sequence[1] then
      bindToNode(tree, shortcut, "", f)
    else
      local childNode = tree.children[shortcut]
      if not childNode then
        newNode = createNode()
        bindToNode(tree, shortcut, "", function() newNode.modal:enter() end)
        -- this is overriding the shorcut. sometimes we store a modal, sometimes a tag. that's wrong.
        tree.children[shortcut] = newNode
      end
      registerSequence(tree.children[shortcut], sequence, f)
    end
end

local Modal = {}
function Modal.new(mod, key)
  local tree = createNode(mod, key)

  local self = {}
  function self.register(sequence, f, message)
    registerSequence(tree, sequence, f)
  end

  return self
end

return Modal

  -- local function text()
  --   return hs.styledtext.ansi(mode, {font={name="Fantasque Sans Mono",size=36}, backgroundColor={alpha=1}})
  -- end
  --
  -- local function hideModeTooltip()
  --   if modeTooltip then
  --     modeTooltip:delete()
  --   end
  -- end
  --
  -- local function showModeTooltip()
  --   clearModeTooltip()
  --   modeTooltip = hs.drawing.text(hs.geometry.rect(100,100,1000,1000), text())
  --   modeTooltip:show()
  -- end
  --
  -- local self = {}
  --
  -- function self.show()
  --   showModeTooltip()
  -- end
  --
  -- function self.hide()
  --   hideModeTooltip()
  -- end
  --
  -- self.parseKey = parseKey

-- local function new(combination)
--   return {["__modal"] = hotkey.modal.new(combination[1], combination[2])}
-- end
--
-- local function assign(element, combination, f)
--   if not element["__modal"] then
--     element["__modal"] = hotkey.modal.new(nil, nil)
--   end
--   element["__modal"]:bind(mod, combination[1], combination[2], f)
-- end
--
-- local function set(mappings, sequence, f)
--   log:d(i(mappings))
--   log:d(i(sequence))
--   local lastCombination = table.remove(sequence)
--
--   local currentMapping = mappings
--   while sequence[1] do
--     local currentCombination = table.remove(sequence, 1)
--     if not currentMapping[currentCombination] then
--       currentMapping[currentCombination] = new(currentCombination)
--     end
--     currentMapping = currentMapping[currentCombination]
--   end
--   assign(currentMapping, lastCombination, f)
-- end
