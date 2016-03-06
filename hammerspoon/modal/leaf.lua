local fn = require("hs.fnutils")
local e = require "modal/events"

local Leaf = {}
function Leaf.new(parent, key)
  local _key = key
  local _sequence = parent == nil and "" or parent.getSequence() .. " " .. key
  local _listeners = {}
  local _parent = parent

  local self = {}

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
    if not id[1] then
      if not _listeners[action] then
        _listeners[action] = {}
      end
      table.insert(_listeners[action], f)
    end
  end

  function self.run()
    self.dispatch(e.sequence(self.getSequence()))
    self.dispatch(e.exit())
    _parent.exit()
  end

  return self
end

return Leaf
