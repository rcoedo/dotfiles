local window = require "window"
local spotify = require "spotify"
local modal = require "modal"

-- Reload configuration
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.alert("Config loaded")

-- Bindings
local cmd = {"cmd"}
local alt = {"alt"}
local cmdalt = {"cmd", "alt"}

-- Spotify mode
s = modal.new(nil, nil, "Spotify mode")
s:bind({}, "p", spotify.previous)
s:bind({}, "n", spotify.next)
s:bind({}, "i", function() s:quit("Current track") spotify.info() end)
s:bind({}, "s", function() s:quit("Spotify") spotify.focus() end)
s:bind({}, "space", function() s:quit("Pause/Unpause Spotify") spotify.playpause()  end)

-- Window mode
w = modal.new(nil, nil, "Window mode")
w:bind({}, "h", window.pushWindowLeft)
w:bind({}, "j", window.pushWindowDown)
w:bind({}, "k", window.pushWindowUp)
w:bind({}, "l", window.pushWindowRight)
w:bind({}, "u", window.smartResizeWindowDown)
w:bind({}, "i", window.smartResizeWindowUp)
w:bind({}, "o", window.smartResizeWindowRight)
w:bind({}, "y", window.smartResizeWindowLeft)
w:bind({}, "[", window.decreaseGrid)
w:bind({}, "]", window.increaseGrid)
w:bind({}, "n", window.pushToNextScreen)
w:bind({}, "m", window.maximizeWindow)
w:bind({}, "f", window.fullscreen)
w:bind({}, "delete", function() window.close() w:quit("Close window") end)
w:bind(cmd, "delete", function() window.kill() w:quit("Kill window") end)

-- Launcher mode
l = modal.new(cmd, "return", "Launcher mode")
l:bind({}, "i", function() l:quit("Focus IntelliJ") window.launchOrFocus("IntelliJ IDEA 15") end)
l:bind({}, "a", function() l:quit("Focus Emacs") window.launchOrFocus("Emacs") end)
l:bind({}, "e", function() l:quit("Focus Atom") window.launchOrFocus("Atom") end)
l:bind({}, "t", function() l:quit("Focus Terminal") window.launchOrFocus("iTerm") end)
l:bind({}, "l", function() l:quit("Focus Slack") window.launchOrFocus("Slack") end)
l:bind({}, "c", function() l:quit("Focus Chrome") window.launchOrFocus("Google Chrome") end)
l:bind({}, "s", function() l:exit() s:enter() end)
l:bind({}, "w", function() l:exit() w:enter() end)

-- Install CLI
hs.ipc.cliInstall()
