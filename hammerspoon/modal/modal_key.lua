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

local function subsequenceEvent(subsequence)
  return {"subsequenceEvent", subsequence}
end

local function sequence(sequence)
  return {"sequence", sequence}
end

local Node = {}
function Node.new(parent, key)
  local _key = key
  local _sequence = parent == nil and "" or parent.getSequence() .. " " .. key
  local _children = {}
  local _listeners = {["sequence"] = {}}
  local _parent = parent
  local _modal = hotkey.modal.new(nil, nil)

  local self = {}

  --
  -- Private functions
  --
  local function bindKey(key, f)
    local keyTable = strToKey(key)
    _modal:bind(keyTable[1], keyTable[2], f)
  end

  -- Bind a sequence to this key
  local function bindSequence(key)
    bindKey(key, function()
      self.dispatch(sequence(self.getSequence() .. " " .. key)) self.exit()
    end)
    _children[key] = key
  end

  -- Bind a new branch under this key
  local function bindBranch(key)
    local newNode = Node.new(self, key)
    bindKey(key, function() self.exit() newNode.enter() end)
    _children[key] = newNode
  end

  --
  -- Public functions
  --
  function self.getKey()
    return _key
  end

  function self.getSequence()
    return _sequence
  end

  function self.handle(event)
    fn.map(_listeners[event[1]], function(listener) listener(event) end)
    self.propagate(event)
  end

  function self.propagate(event)
    if _parent then
      _parent.handle(event)
    end
  end

  function self.dispatch(event)
    self.handle(event)
  end

  function self.listen(id, action, f)
    local sequence = type(id) == "string" and fn.split(id, " ")  or id
    if sequence[1] then
      local key = table.remove(sequence, 1)
      if type(_children[key]) == "table" then
        _children[key].listen(sequence, action, f)
      end
    else
      table.insert(_listeners[action], f)
    end
  end

  function self.enter()
    log.d("transitioning")
    _modal:enter()
  end

  function self.exit()
    log.d("exiting")
    _modal:exit()
  end

  -- Register a new sequence under this node
  function self.register(seq)
    local sequence = type(seq) == "string" and fn.split(seq, " ") or seq
    if sequence[1] then
      local key = table.remove(sequence, 1)
      if not sequence[1] then
        bindSequence(key)
      else
        if not _children[key] then
          bindBranch(key)
        end
        _children[key].register(sequence, f)
      end
    end
  end

  if (parent) then
    bindKey("escape", function() self.exit() end)
  else
    self.listen({}, "sequence", function() self.enter() end)
  end

  return self
end

function onSequence(sequence, f)
  return function(event)
    log.d(i(event) .. " " .. sequence)
    f()
  end
end

local function last(sequence)
  return table.remove(fn.split(sequence, " "))
end

local function init(sequence)
  local otherSeq = fn.split(sequence, " ")
  table.remove(otherSeq)
  return otherSeq
end

local Modal = {}
function Modal.new(prefix)
  local _prefix = prefix == nil and "" or prefix
  local _root = Node.new()

  local self = {}
  function self.register(sequence, f)
    local prefixedSequence = prefix .. " " .. sequence
    _root.register(prefixedSequence)
    _root.listen(init(prefixedSequence), "sequence", function(e)
      if last(prefixedSequence) == last(e[2]) then
        f()
      end
    end)
    _root.enter()
  end

  return self
end

return Modal
