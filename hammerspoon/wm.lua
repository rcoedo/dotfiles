require "smartgrid"

hs.window.animationDuration = 0
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0
hs.grid.GRIDWIDTH = 3
hs.grid.GRIDHEIGHT = 3

local cmd = {"cmd"}
local cmdalt = {"cmd", "alt"}

hs.hotkey.bind(cmd, "h", function() hs.window.focusedWindow():focusWindowWest() end)
hs.hotkey.bind(cmd, "l", function() hs.window.focusedWindow():focusWindowEast() end)
hs.hotkey.bind(cmd, "k", function() hs.window.focusedWindow():focusWindowNorth() end)
hs.hotkey.bind(cmd, "j", function() hs.window.focusedWindow():focusWindowSouth() end)

hs.hotkey.bind(cmdalt, "h", hs.grid.pushWindowLeft)
hs.hotkey.bind(cmdalt, "j", hs.grid.pushWindowDown)
hs.hotkey.bind(cmdalt, "k", hs.grid.pushWindowUp)
hs.hotkey.bind(cmdalt, "l", hs.grid.pushWindowRight)

hs.hotkey.bind(cmdalt, "u", smartResizeWindowDown)
hs.hotkey.bind(cmdalt, "i", smartResizeWindowUp)
hs.hotkey.bind(cmdalt, "o", smartResizeWindowRight)
hs.hotkey.bind(cmdalt, "y", smartResizeWindowLeft)

hs.hotkey.bind(cmdalt, "n", hs.grid.pushWindowNextScreen)
hs.hotkey.bind(cmdalt, "p", hs.grid.pushWindowPreviousScreen)

hs.hotkey.bind(cmdalt, "m", hs.grid.maximizeWindow)

hs.hotkey.bind(cmd, "delete", function() hs.window.focusedWindow():close() end)
hs.hotkey.bind(cmdalt, "delete", function() hs.window.focusedWindow():application():kill() end)
