local hotkey = require "hs.hotkey"
local fn = require "hs.fnutils"

local utils = require "modal/utils"
local e = require "modal/events"

local Node = {}
function Node.new(parent, key)
  local _key = key
  local _sequence = parent == nil and "" or parent.getSequence() .. " " .. key
  local _children = {}
  local _listeners = {}
  local _parent = parent
  local _metadata = {}
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
    local leaf = Node.new(self, key)
    bindKey(key, function() leaf.run() end)
    self.listen("exit", function() _modal:exit() end)
    _children[key] = leaf
  end

  -- Bind a new branch under this key
  local function bindBranch(key)
    local newNode = Node.new(self, key)
    bindKey(key, function() self.transition(newNode) end)
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
     if _listeners[event.getKey()] then
        fn.map(_listeners[event.getKey()], function(listener) listener(event) end)
     end
     if event.propagate() then
        self.propagate(event)
     end
  end

  function self.getMetadata()
    return _metadata
  end

  function self.addMetadata(key, value)
    _metadata[key] = value
    return self
  end

  function self.propagate(event)
    if _parent then
      _parent.handle(event)
    end
  end

  function self.dispatch(event)
    self.handle(event)
  end

  function self.findNode(id)
    local sequence = type(id) == "string" and fn.split(id, " ")  or id
    if sequence[1] then
      local key = table.remove(sequence, 1)
      if _children[key] then
        return _children[key].findNode(sequence)
      end
    else
      return self
    end
  end

  function self.listen(action, f)
    if not _listeners[action] then
      _listeners[action] = {}
    end
    table.insert(_listeners[action], f)
    return self
  end

  function self.enter()
    _modal:enter()
  end

  -- Dispatches an exit event and quits the modal
  function self.exit()
    _modal:exit()
    self.dispatch(e.exit())
  end

  function self.run()
    self.dispatch(e.sequence(self.getSequence()))
    self.dispatch(e.exit())
  end

  -- Dispatches a transition event, exits this modal and enters next one
  function self.transition(newNode)
    _modal:exit()
    newNode.enter()
    self.dispatch(e.transition(newNode))
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
    self.listen("exit", function() self.enter() end)
  end

  return self
end
return Node
