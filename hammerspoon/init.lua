local window = require "window"
local spotify = require "spotify"
local Spacebar = require "modal/spacebar"

-- Reload configuration
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.alert("Config loaded")

local key = Spacebar.new()
local function spotifyMode(prefix)
  key.register(prefix .. " p", spotify.previous, {modal = "stay"})
  key.register(prefix .. " n", spotify.next, {modal = "stay"})
  key.register(prefix .. " i", spotify.info)
  key.register(prefix .. " s", spotify.focus)
  key.register(prefix .. " space", spotify.playpause)
  key.addTag(prefix, "Spotify mode")
end

local function windowMode(prefix)
  key.register(prefix .. " h", window.pushWindowLeft, {modal = "stay"})
  key.register(prefix .. " j", window.pushWindowDown, {modal = "stay"})
  key.register(prefix .. " k", window.pushWindowUp, {modal = "stay"})
  key.register(prefix .. " l", window.pushWindowRight, {modal = "stay"})
  key.register(prefix .. " u", window.smartResizeWindowDown, {modal = "stay"})
  key.register(prefix .. " i", window.smartResizeWindowUp, {modal = "stay"})
  key.register(prefix .. " o", window.smartResizeWindowRight, {modal = "stay"})
  key.register(prefix .. " y", window.smartResizeWindowLeft, {modal = "stay"})
  key.register(prefix .. " [", window.decreaseGrid, {modal = "stay"})
  key.register(prefix .. " ]", window.increaseGrid, {modal = "stay"})
  key.register(prefix .. " n", window.pushToNextScreen, {modal = "stay"})
  key.register(prefix .. " m", window.maximizeWindow)
  key.register(prefix .. " f", window.fullscreen)
  key.register(prefix .. " delete", window.close)
  key.register(prefix .. " cmd-delete", window.kill)
  key.addTag(prefix, "Window mode")
end

local function launcherMode(prefix)
  key.register(prefix .. " i", function() window.launchOrFocus("IntelliJ IDEA 15") end)
  key.register(prefix .. " a", function() window.launchOrFocus("Atom") end)
  key.register(prefix .. " e", function() window.launchOrFocus("Emacs") end)
  key.register(prefix .. " t", function() window.launchOrFocus("iTerm") end)
  key.register(prefix .. " l", function() window.launchOrFocus("Slack") end)
  key.register(prefix .. " c", function() window.launchOrFocus("Google Chrome") end)
  key.addTag(prefix, "Launcher mode")

  windowMode(prefix .. " w")
  spotifyMode(prefix .. " s")
end

local key = Spacebar.new()
launcherMode("cmd-return")

-- TODO: Merge Leaf and Node.
-- TODO: Add a nice syntax to define a tree with tags
-- Spacebar.new {
--   prefix = "alt-space",
--   bindings = {
--     {"d", "dAlert"} = function() hs.alert("d!") end,
--     {"a", "moreAlerts"} = {
--       {"b", "bAlert"} = function() hs.alert("b!") end,
--       "c" = function() hs.alert("c!") end
--     }
--   }
-- }
-- TODO: Draw a pretty tooltip to navigate hotkeys

-- Install CLI
hs.ipc.cliInstall()
