local window = require "window"
local spotify = require "spotify"
local amphetamine = require "amphetamine"
local Spacebar = require "modal/spacebar"

-- Reload configuration
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.alert("Config loaded")

local key = Spacebar.new {
   prefix = "cmd-return",
   bindings = {
      {"i", "intellij", function() window.launchOrFocus("IntelliJ IDEA") end},
      {"u", "android studio", function() window.launchOrFocus("Android Studio") end},
      {"d", "ndb", function() window.focus("ndb") end},
      {"o", "code", function() window.launchOrFocus("Visual Studio Code") end},
      {"e", "emacs", function() window.launchOrFocus("Emacs.app") end},
      {"b", "brave", function() window.launchOrFocus("Brave Browser") end},
      {"r", "trello", function() window.launchOrFocus("Trello") end},
      {"h", "hyper", function() window.launchOrFocus("Hyper") end},
      {"t", "terminal", function() window.launchOrFocus("iTerm") end},
      {"p", "postman", function() window.launchOrFocus("Postman") end},
      {"c", "chrome", function() window.launchOrSwitch("Google Chrome") end},
      {"m", "messaging", {
          {"w", "whatsapp", function() window.launchOrSwitch("WhatsApp") end},
          {"d", "discord", function() window.launchOrSwitch("Discord") end},
          {"t", "telegram", function() window.launchOrFocus("Telegram") end},
          {"l", "slack", function() window.launchOrFocus("Slack") end},
          {"r", "wire", function() window.launchOrFocus("Wire") end}
      }},
      {"s", "spotify mode", {
          {"p", "previous", spotify.previous, {modal = "stay"}},
          {"n", "next", spotify.next, {modal = "stay"}},
          {"i", "info", spotify.info},
          {"s", "open", spotify.focus},
          {"space", "play/pause", spotify.playpause}
      }},
      {"w", "window mode", {
          {"h", "push left", window.pushWindowLeft, {modal = "stay"}},
          {"j", "push down", window.pushWindowDown, {modal = "stay"}},
          {"k", "push up", window.pushWindowUp, {modal = "stay"}},
          {"l", "push right", window.pushWindowRight, {modal = "stay"}},
          {"u", "resize down", window.smartResizeWindowDown, {modal = "stay"}},
          {"i", "resize up", window.smartResizeWindowUp, {modal = "stay"}},
          {"o", "resize right", window.smartResizeWindowRight, {modal = "stay"}},
          {"y", "resize left", window.smartResizeWindowLeft, {modal = "stay"}},
          {"[", "decrease grid", window.decreaseGrid, {modal = "stay"}},
          {"]", "increase grid", window.increaseGrid, {modal = "stay"}},
          {"n", "next screen", window.pushToNextScreen, {modal = "stay"}},
          {"m", "maximize", window.maximizeWindow},
          {"f", "fullscreen", window.fullscreen},
          {"delete", "close", window.close},
          {"cmd-delete", "kill", window.kill}
      }}
   }
}
-- TODO: Draw a pretty tooltip to navigate hotkeys
-- Export the tree to json?

-- Install CLI
hs.ipc.cliInstall()
