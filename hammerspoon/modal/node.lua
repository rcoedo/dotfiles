local hotkey = require "hs.hotkey"
local fn = require "hs.fnutils"

local Leaf = require "modal/leaf"
local utils = require "modal/utils"
local e = require "modal/events"

local Node = {}
function Node.new(parent, key)
  local _key = key
  local _sequence = parent == nil and "" or parent.getSequence() .. " " .. key
  local _children = {}
  local _listeners = {}
  local _parent = parent
  local _modal = hotkey.modal.new(nil, nil)

  local self = {}

  --
  -- Private functions
  --
  local function bindKey(key, f)
    local keyTable = utils.strToKey(key)
    _modal:bind(keyTable[1], keyTable[2], f)
  end

  -- Bind a sequence to this key
  local function bindSequence(key)
    local leaf = Leaf.new(self, key)
    bindKey(key, function() leaf.run() end)
    _children[key] = leaf
  end

  -- Bind a new branch under this key
  local function bindBranch(key)
    local newNode = Node.new(self, key)
    bindKey(key, function() self.transition(key) end)
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
    if _listeners[event[1]] then
      fn.map(_listeners[event[1]], function(listener) listener(event) end)
    end
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
      if _children[key] then
        _children[key].listen(sequence, action, f)
      end
    else
      if not _listeners[action] then
        _listeners[action] = {}
      end
      table.insert(_listeners[action], f)
    end
  end

  function self.enter()
    _modal:enter()
  end

  -- Dispatches an exit event and quits the modal
  function self.exit()
    _modal:exit()
    self.dispatch(e.exit())
  end

  -- Dispatches a transition event, exits this modal and enters next one
  function self.transition(child)
    _modal:exit()
    _children[child].enter()
    self.dispatch(e.transition())
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

  -- Escape cancels a sequence, root re-enters onExit event
  if (parent) then
    bindKey("escape", function() self.exit() end)
  else
    self.listen({}, "exit", function() self.enter() end)
  end

  return self
end
return Node
