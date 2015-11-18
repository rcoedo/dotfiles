require "launcher"
require "wm"
require "spotify"
require "cleaner"

-- Ensure the IPC command line client is available
hs.ipc.cliInstall()

-- Watch the config files
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.alert("Config loaded")
