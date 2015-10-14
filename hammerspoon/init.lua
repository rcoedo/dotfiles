require "launcher"
require "wm"
require "spotify"

-- Watch the config files
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.alert("Config loaded")
