local Event = {}
function Event.new(key, data)
   local _key = key
   local _data = data
   local _propagate = true

   local self = {}

   function self.stopPropagation()
      _propagate = false
   end

   function self.getKey()
      return _key
   end

   function self.getData()
      return _data
   end

   function self.propagate()
      return _propagate
   end
   
   return self
end

local function transition(newNode)
   return Event.new("transition", newNode)
end

local function exit()
   return Event.new("exit", nil)
end

local function sequence(sequence)
   return Event.new("sequence", sequence)
end

return { transition = transition, exit = exit, sequence = sequence}
