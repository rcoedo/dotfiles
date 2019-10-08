local Node = require "modal/node"
local Tooltip = require "modal/tooltip"
local fn = require "hs.fnutils"

local function getLabels(node)
  local labels = node.getMetadata()["labels"]
  return labels == nil and {} or labels
end

local Spacebar = {}
function Spacebar.new(config)
   local _tooltip = Tooltip.new()
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
                   _tooltip.setLabels(getLabels(event.getData()))
                   if (not _tooltip:isShowing()) then
                      _tooltip.show()
                   end
   end)

   _root.listen("exit", function() _tooltip.hide() end)

   if config ~= nil then
      init(config)
   end

   return self
end

return Spacebar
