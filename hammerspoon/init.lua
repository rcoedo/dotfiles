package.path = package.path .. ";/Users/rcoedo/.local/share/mise/installs/lua/5.4/luarocks/share/lua/5.4/?.lua"

local amphetamine = require("amphetamine")
local window = require("window")
local app = require("application")
local spotify = require("spotify")
local Spacebar = require("modal/spacebar")

-- Reload configuration
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.application.enableSpotlightForNameSearches(true)
hs.alert("Config loaded")

local key = Spacebar.new({
  prefix = "ctrl-space",
  bindings = {
    {
      "ctrl-space",
      "hints",
      function()
        hs.hints.windowHints()
      end,
    },
    {
      "b",
      "browser",
      function()
        app.launchOrFocus("Brave Browser")
      end,
    },
    {
      "t",
      "terminal",
      function()
        app.launchOrFocus("WezTerm")
      end,
    },
    {
      "m",
      "messaging",
      {
        {
          "w",
          "whatsapp",
          function()
            app.launchOrSwitch("WhatsApp")
          end,
        },
        {
          "l",
          "slack",
          function()
            app.launchOrFocus("Slack")
          end,
        },
      },
    },
    {
      "s",
      "spotify mode",
      {
        { "p",     "previous",   spotify.previous, { modal = "stay" } },
        { "n",     "next",       spotify.next,     { modal = "stay" } },
        { "i",     "info",       spotify.info },
        { "s",     "open",       spotify.focus },
        { "space", "play/pause", spotify.playpause },
      },
    },
    {
      "w",
      "window mode",
      {
        { "h",          "push left",     window.pushWindowLeft,         { modal = "stay" } },
        { "j",          "push down",     window.pushWindowDown,         { modal = "stay" } },
        { "k",          "push up",       window.pushWindowUp,           { modal = "stay" } },
        { "l",          "push right",    window.pushWindowRight,        { modal = "stay" } },
        { "u",          "resize down",   window.smartResizeWindowDown,  { modal = "stay" } },
        { "i",          "resize up",     window.smartResizeWindowUp,    { modal = "stay" } },
        { "o",          "resize right",  window.smartResizeWindowRight, { modal = "stay" } },
        { "y",          "resize left",   window.smartResizeWindowLeft,  { modal = "stay" } },
        { "[",          "decrease grid", window.decreaseGrid,           { modal = "stay" } },
        { "]",          "increase grid", window.increaseGrid,           { modal = "stay" } },
        { "n",          "next screen",   window.pushToNextScreen,       { modal = "stay" } },
        { "m",          "maximize",      window.maximizeWindow },
        { "f",          "fullscreen",    window.fullscreen },
        { "delete",     "close",         window.close },
        { "cmd-delete", "kill",          window.kill },
      },
    },
  },
})
-- TODO: Draw a pretty tooltip to navigate hotkeys
-- Export the tree to json?

-- Install CLI
hs.ipc.cliInstall()
