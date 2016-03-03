hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.alert("Config loaded")

function gridOp(op, cell)
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

function expandToRight(cell)  return gridOp({w = 1},         cell) end
function expandToLeft(cell)   return gridOp({x = -1, w = 1}, cell) end
function expandToBottom(cell) return gridOp({h = 1},         cell) end
function expandToTop(cell)    return gridOp({y = -1, h = 1}, cell) end

function shrinkToRight(cell)  return gridOp({x = 1, w = -1}, cell) end
function shrinkToLeft(cell)   return gridOp({w = -1},        cell) end
function shrinkToBottom(cell) return gridOp({y = 1, h = -1}, cell) end
function shrinkToTop(cell)    return gridOp({h = -1}       , cell) end

function smartResizeRight(cell)
    if ((cell.x + cell.w >= hs.grid.GRIDWIDTH) and (cell.w > 1)) then
        return shrinkToRight(cell)
    elseif ((cell.x + cell.w < hs.grid.GRIDWIDTH) and (cell.w < hs.grid.GRIDWIDTH)) then
        return expandToRight(cell)
    end
end

function smartResizeLeft(cell)
    if (cell.x + cell.w >= hs.grid.GRIDWIDTH) then
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

function smartResizeDown(cell)
    if ((cell.y + cell.h >= hs.grid.GRIDHEIGHT) and (cell.h > 1)) then
        return shrinkToBottom(cell)
    elseif ((cell.y + cell.h < hs.grid.GRIDHEIGHT) and (cell.h < hs.grid.GRIDHEIGHT)) then
        return expandToBottom(cell)
    end
end

function smartResizeUp(cell)
    if (cell.y + cell.h >= hs.grid.GRIDHEIGHT) then
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

function smartResizeWindowRight() hs.grid.adjustFocusedWindow(smartResizeRight) end
function smartResizeWindowLeft() hs.grid.adjustFocusedWindow(smartResizeLeft) end
function smartResizeWindowUp() hs.grid.adjustFocusedWindow(smartResizeUp) end
function smartResizeWindowDown() hs.grid.adjustFocusedWindow(smartResizeDown) end

function adjustGrid(h, w) hs.grid.adjustHeight(h) hs.grid.adjustWidth(w) end
function increaseGrid() adjustGrid(1, 1) end
function decreaseGrid() adjustGrid(-1, -1) end

function launchOrSwitch(name, realName)
    if (hs.window.frontmostWindow() ~= null and hs.window.frontmostWindow():application() == hs.appfinder.appFromName(name)) then
        hs.eventtap.keyStroke({"cmd"}, "`")
    else
        hs.application.launchOrFocus(realName or name)
    end
end

function pushToNextScreen()
   win = hs.window.frontmostWindow()
   win:moveToScreen(win:screen():next())
end
hs.window.animationDuration = 0
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0
hs.grid.GRIDWIDTH = 2
hs.grid.GRIDHEIGHT = 2

-- Bindings

local cmd = {"cmd"}
local alt = {"alt"}
local cmdalt = {"cmd", "alt"}

ModalKey = {}
function ModalKey.new()
  local mode = nil
  local modeTooltip = nil

  local function text()
    return hs.styledtext.ansi(mode, {font={name="Fantasque Sans Mono",size=36}, backgroundColor={alpha=1}})
  end

  local function hideModeTooltip()
    if modeTooltip then
      modeTooltip:delete()
    end
  end

  local function showModeTooltip()
    clearModeTooltip()
    modeTooltip = hs.drawing.text(hs.geometry.rect(100,100,1000,1000), text())
    modeTooltip:show()
  end

  local self = {}
  function self.show()
    showModeTooltip()
  end
  function self.hide()
    hideModeTooltip()
  end
  return self
end


function text(message)
  return hs.styledtext.ansi(message, {font={name="Fantasque Sans Mono",size=36}, backgroundColor={alpha=1}})
end

function clearModeTooltip()
  if modeStatus then
    modeStatus:delete()
  end
end

function drawModeTooltip(message)
  clearModeTooltip()
  modeStatus = hs.drawing.text(hs.geometry.rect(100,100,1000,1000), text(message))
  modeStatus:show()
end

function modal(mod, key, message)
  local modal = hs.hotkey.modal.new(mod, key)
  modal:bind({}, "escape", function() modal:exit() end)
  modal.exited = function(self) clearModeTooltip() end
  modal.entered = function(self) drawModeTooltip(message) end
  return modal
end

function hs.hotkey.modal:quit(message)
  self:exit()
  hs.alert(message, 1)
end

function spotifyInfo()
  hs.notify.new("", {title=hs.spotify.getCurrentTrack(), subTitle=hs.spotify.getCurrentArtist()}):send()
end

-- Spotify bindings
s = modal(nil, nil, "Spotify mode")
s:bind({}, "p", function() hs.spotify.previous() spotifyInfo() end)
s:bind({}, "n", function() hs.spotify.next() spotifyInfo() end)
s:bind({}, "i", function() s:quit("Current track") spotifyInfo() end)
s:bind({}, "s", function() s:quit("Spotify") hs.application.launchOrFocus("Spotify") end)
s:bind({}, "space", function() s:quit("Pause/Unpause Spotify") hs.spotify.playpause()  end)

w = modal(nil, nil, "Window mode")
w:bind({}, "h", hs.grid.pushWindowLeft)
w:bind({}, "j", hs.grid.pushWindowDown)
w:bind({}, "k", hs.grid.pushWindowUp)
w:bind({}, "l", hs.grid.pushWindowRight)
w:bind({}, "u", smartResizeWindowDown)
w:bind({}, "i", smartResizeWindowUp)
w:bind({}, "o", smartResizeWindowRight)
w:bind({}, "y", smartResizeWindowLeft)
w:bind({}, "[", decreaseGrid)
w:bind({}, "]", increaseGrid)
w:bind({}, "n", pushToNextScreen)
w:bind({}, "m", hs.grid.maximizeWindow)
w:bind({}, "f", function() hs.window.frontmostWindow():toggleFullScreen() end)
w:bind({}, "delete", function() hs.window.frontmostWindow():close() w:quit("Close window") end)
w:bind(cmd, "delete", function() hs.window.frontmostWindow():application():kill() w:quit("Kill window") end)

-- Launcher bindings
l = modal(cmd, "return", "Launcher mode")
l:bind({}, "i", function() l:quit("Focus IntelliJ") hs.application.launchOrFocus("IntelliJ IDEA 15") end)
l:bind({}, "a", function() l:quit("Focus Emacs") hs.application.launchOrFocus("Emacs") end)
l:bind({}, "e", function() l:quit("Focus Atom") hs.application.launchOrFocus("Atom") end)
l:bind({}, "t", function() l:quit("Focus Terminal") hs.application.launchOrFocus("iTerm") end)
l:bind({}, "l", function() l:quit("Focus Slack") hs.application.launchOrFocus("Slack") end)
l:bind({}, "c", function() l:quit("Focus Chrome") hs.application.launchOrFocus("Google Chrome") end)
l:bind({}, "s", function() l:exit() hs.alert.closeAll() s:enter() end)
l:bind({}, "w", function() l:exit() hs.alert.closeAll() w:enter() end)

hs.ipc.cliInstall()
