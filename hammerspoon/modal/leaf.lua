local fn = require("hs.fnutils")
local e = require "modal/events"

local Leaf = {}
function Leaf.new(parent, key)
  local _key = key
  local _sequence = parent == nil and "" or parent.getSequence() .. " " .. key
  local _listeners = {}
  local _metadata = {}
  local _parent = parent

  local self = {}

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
    if (event.propagate()) then
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
    if not sequence[1] then
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

  function self.run()
    self.dispatch(e.sequence(self.getSequence()))
    self.dispatch(e.exit())
  end

  return self
end

return Leaf
