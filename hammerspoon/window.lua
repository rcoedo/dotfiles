local window = require "hs.window"
local grid = require "hs.grid"
local eventtap = require "hs.eventtap"
local application = require "hs.application"
local appfinder = require "hs.appfinder"

window.animationDuration = 0
grid.MARGINX = 0
grid.MARGINY = 0
grid.GRIDWIDTH = 2
grid.GRIDHEIGHT = 2

local function gridOp(op, cell)
    if (op.x ~= null) then
        cell.x = cell.x + op.x
    end
    if (op.y ~= null) then
        cell.y = cell.y + op.y
    end
    if (op.w ~= null) then
        cell.w = cell.w + op.w
    end
    if (op.h ~= null) then
        cell.h = cell.h + op.h
    end
    return cell
end

local function expandToRight(cell)  return gridOp({w = 1},         cell) end
local function expandToLeft(cell)   return gridOp({x = -1, w = 1}, cell) end
local function expandToBottom(cell) return gridOp({h = 1},         cell) end
local function expandToTop(cell)    return gridOp({y = -1, h = 1}, cell) end

local function shrinkToRight(cell)  return gridOp({x = 1, w = -1}, cell) end
local function shrinkToLeft(cell)   return gridOp({w = -1},        cell) end
local function shrinkToBottom(cell) return gridOp({y = 1, h = -1}, cell) end
local function shrinkToTop(cell)    return gridOp({h = -1}       , cell) end

local function smartResizeRight(cell)
    if ((cell.x + cell.w >= grid.GRIDWIDTH) and (cell.w > 1)) then
        return shrinkToRight(cell)
    elseif ((cell.x + cell.w < grid.GRIDWIDTH) and (cell.w < grid.GRIDWIDTH)) then
        return expandToRight(cell)
    end
end

local function smartResizeLeft(cell)
    if (cell.x + cell.w >= grid.GRIDWIDTH) then
        if (cell.x > 0) then
            return expandToLeft(cell)
        else
            return shrinkToLeft(cell)
        end
    else
        if (cell.w > 1) then
            return shrinkToLeft(cell)
        elseif (cell.x > 0) then
            return expandToLeft(cell)
        end
    end
end

local function smartResizeDown(cell)
    if ((cell.y + cell.h >= grid.GRIDHEIGHT) and (cell.h > 1)) then
        return shrinkToBottom(cell)
    elseif ((cell.y + cell.h < grid.GRIDHEIGHT) and (cell.h < grid.GRIDHEIGHT)) then
        return expandToBottom(cell)
    end
end

local function smartResizeUp(cell)
    if (cell.y + cell.h >= grid.GRIDHEIGHT) then
        if (cell.y > 0) then
            return expandToTop(cell)
        else
            return shrinkToTop(cell)
        end
    else
        if (cell.h > 1) then
            return shrinkToTop(cell)
        elseif (cell.y > 0) then
            return expandToTop(cell)
        end
    end
end

local function smartResizeWindowRight() grid.adjustFocusedWindow(smartResizeRight) end
local function smartResizeWindowLeft() grid.adjustFocusedWindow(smartResizeLeft) end
local function smartResizeWindowUp() grid.adjustFocusedWindow(smartResizeUp) end
local function smartResizeWindowDown() grid.adjustFocusedWindow(smartResizeDown) end

local function adjustGrid(h, w) grid.adjustHeight(h) grid.adjustWidth(w) end
local function increaseGrid() adjustGrid(1, 1) end
local function decreaseGrid() adjustGrid(-1, -1) end

local function launchOrSwitch(name, realName)
    if (window.frontmostWindow() ~= null and window.frontmostWindow():application() == appfinder.appFromName(name)) then
        eventtap.keyStroke({"cmd"}, "`")
    else
        local app = appfinder.appFromName(realName or name)
        if (app == null) then
          application.launchOrFocus(realName or name)
        else
          app:activate()
        end
    end
end

local function focus(name)
  local win = hs.window.find(name)
  if (win ~= null) then
    win:focus()
  end
end

local function pushToNextScreen()
   win = window.frontmostWindow()
   win:moveToScreen(win:screen():next())
end

local function fullscreen()
  window.frontmostWindow():toggleFullScreen()
end

local function close()
  window.frontmostWindow():close()
end

local function kill()
  window.frontmostWindow():application():kill()
end

return {
  smartResizeWindowRight = smartResizeWindowRight,
  smartResizeWindowLeft = smartResizeWindowLeft,
  smartResizeWindowDown = smartResizeWindowDown,
  smartResizeWindowUp = smartResizeWindowUp,
  adjustGrid = adjustGrid,
  increaseGrid = increaseGrid,
  decreaseGrid = decreaseGrid,
  launchOrSwitch = launchOrSwitch,
  launchOrFocus = application.launchOrFocus,
  pushToNextScreen = pushToNextScreen,
  pushWindowUp = grid.pushWindowUp,
  pushWindowDown = grid.pushWindowDown,
  pushWindowRight = grid.pushWindowRight,
  pushWindowLeft = grid.pushWindowLeft,
  maximizeWindow = grid.maximizeWindow,
  focus = focus,
  fullscreen = fullscreen,
  close = close,
  kill = kill
}
