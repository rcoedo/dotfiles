hs.window.animationDuration = 0
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0
hs.grid.GRIDWIDTH = 2
hs.grid.GRIDHEIGHT = 2

local super = {"cmd"}
local superalt = {"cmd", "alt"}

hs.hotkey.bind(super, "h", function() hs.window.focusedWindow():focusWindowWest() end)
hs.hotkey.bind(super, "l", function() hs.window.focusedWindow():focusWindowEast() end)
hs.hotkey.bind(super, "k", function() hs.window.focusedWindow():focusWindowNorth() end)
hs.hotkey.bind(super, "j", function() hs.window.focusedWindow():focusWindowSouth() end)

hs.hotkey.bind(superalt, "h", hs.grid.pushWindowLeft)
hs.hotkey.bind(superalt, "j", hs.grid.pushWindowDown)
hs.hotkey.bind(superalt, "k", hs.grid.pushWindowUp)
hs.hotkey.bind(superalt, "l", hs.grid.pushWindowRight)

hs.hotkey.bind(superalt, "u", hs.grid.resizeWindowTaller)
hs.hotkey.bind(superalt, "i", hs.grid.resizeWindowShorter)
hs.hotkey.bind(superalt, "o", hs.grid.resizeWindowWider)
hs.hotkey.bind(superalt, "y", hs.grid.resizeWindowThinner)

hs.hotkey.bind(superalt, "n", hs.grid.pushWindowNextScreen)
hs.hotkey.bind(superalt, "p", hs.grid.pushWindowPreviousScreen)

hs.hotkey.bind(superalt, "m", hs.grid.maximizeWindow)

hs.hotkey.bind(super, "delete", function() hs.window.focusedWindow():close() end)
hs.hotkey.bind(superalt, "delete", function() hs.window.focusedWindow():application():kill() end)

--local hint = hs.hints
--hint.hintChars = {"a","s","d","f","j","k","l",";","g","h"}
--hs.hotkey.bind({"ctrl"}, "return", hint.windowHints)

--local super = {"cmd"}
--local superalt = {"cmd", "alt"}
--local alt = {"alt"}

--hs.window.animationDuration = 0

--function floating()
    --local win = hs.window.focusedWindow()
    --local s = win:screen():frame()
    --win:setFrame({x=s.w/2-640 + s.x, y=s.h/2-360 + s.y, w=1280., h=720})
--end

--function fullscreen() hs.window.focusedWindow():maximize() end
--function resizeRight() hs.window.focusedWindow():moveToUnit({x=0.5, y=0, w=0.5, h=1}) end
--function resizeLeft() hs.window.focusedWindow():moveToUnit({x=0, y=0, w=0.5, h=1}) end
--function resizeTopLeft() hs.window.focusedWindow():moveToUnit({x=0, y=0, w=0.5, h=0.5}) end
--function resizeTopRight() hs.window.focusedWindow():moveToUnit({x=0.5, y=0, w=0.5, h=0.5}) end
--function resizeBottomLeft() hs.window.focusedWindow():moveToUnit({x=0, y=0.5, w=0.5, h=0.5}) end
--function resizeBottomRight() hs.window.focusedWindow():moveToUnit({x=0.5, y=0.5, w=0.5, h=0.5}) end

--function moveToLeftMonitor() hs.window.focusedWindow():setFrame(hs.window.focusedWindow():screen():toWest():frame()) end
--function moveToRightMonitor() hs.window.focusedWindow():setFrame(hs.window.focusedWindow():screen():toEast():frame()) end

--function moveRight() hs.window.focusedWindow():focusWindowEast() end
--function moveLeft() hs.window.focusedWindow():focusWindowWest() end
--function moveUp() hs.window.focusedWindow():focusWindowNorth() end
--function moveDown() hs.window.focusedWindow():focusWindowSouth() end

--function killWindow() hs.window.focusedWindow():close() end
--function killApplication() hs.window.focusedWindow():application():kill() end

--function resizeOrMoveToMonitorLeft()
    --local win = hs.window.focusedWindow()
    --local frame  = win:frame()
    --local screen = win:screen():frame()
    --if frame.x == screen.x and frame.y == screen.y
        --and (math.abs(screen.w/2 - frame.w) < 5)
        --and (math.abs(screen.h - frame.h) < 30) then
        --westWindow = win:screen():toWest()
        --if westWindow ~= nil then
            --win:setFrame(westWindow:frame())
        --end
    --else
        --resizeLeft()
    --end
--end

--function resizeOrMoveToMonitorRight()
    --local win = hs.window.focusedWindow()
    --local frame  = win:frame()
    --local screen = win:screen():frame()
    --if (math.abs(frame.x - screen.w/2) < 5) and frame.y == screen.y
        --and (math.abs(screen.w/2 - frame.w) < 5)
        --and (math.abs(screen.h - frame.h) < 30) then
        --eastWindow = win:screen():toEast()
        --if eastWindow ~= nil then
            --win:setFrame(eastWindow:frame())
        --end
    --else
        --resizeRight()
    --end
--end

--hs.hotkey.bind(superalt, "h", resizeOrMoveToMonitorLeft)
--hs.hotkey.bind(superalt, "j", floating)
--hs.hotkey.bind(superalt, "k", fullscreen)
--hs.hotkey.bind(superalt, "l", resizeOrMoveToMonitorRight)

--hs.hotkey.bind(superalt, ".", moveToRightMonitor)
--hs.hotkey.bind(superalt, ",", moveToLeftMonitor)

--hs.hotkey.bind(super, "j", moveDown)
--hs.hotkey.bind(super, "k", moveUp)
--hs.hotkey.bind(super, "h", moveLeft)
--hs.hotkey.bind(super, "l", moveRight)

--hs.hotkey.bind(super, "delete", killWindow)
--hs.hotkey.bind(superalt, "delete", killApplication)
