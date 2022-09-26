local screen = require "hs.screen"
local canvas = require "hs.canvas"
local styledtext = require "hs.styledtext"
local template = require "resty.template"

local function buildStyledText(labels)
  local view = template.compile("modal/layout.html")
  return styledtext.getStyledTextFromData(view{labels = labels})
end

local Tooltip = {}
function Tooltip.new()
   local self = {}
   local _frame = screen.primaryScreen():frame()
   local _background = {
      id = "background",
      action = "fill",
      fillColor = {red = 0, green = 0, blue = 0, alpha=0.80},
      frame = { x = "0", y = "0", h = "1", w = "1", },
      type = "rectangle"
   }
   local _canvas = canvas.new({ x = 0, y = 0, w = _frame.w, h = _frame.h }):appendElements(_background)

   function self.show()
      _canvas:show()
   end

   function self.isShowing()
      return _canvas:isShowing()
   end

   function self.setLabels(labels)
      _canvas[2] = {
         id = "text",
         action = "fill",
         text = buildStyledText(labels),
         type = "text"
      }
   end

   function self.hide()
      _canvas:hide()
   end

   return self
end
return Tooltip
