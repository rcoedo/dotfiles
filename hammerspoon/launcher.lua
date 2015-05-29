k = hs.hotkey.modal.new({"cmd"}, "return")

k:bind({"cmd"}, "return", function() end)
k:bind({}, "escape", function() k:exit() end)

k:bind({}, "c", function() hs.application.launchOrFocus("Google Chrome") k:exit() end)
k:bind({}, "t", function() hs.application.launchOrFocus("iTerm") k:exit() end)
k:bind({}, "s", function() hs.application.launchOrFocus("Spotify") k:exit() end)
k:bind({}, "i", function() hs.application.launchOrFocus("IntelliJ IDEA 14") k:exit() end)
k:bind({}, "p", function() hs.application.launchOrFocus("Postman - REST Client (Packaged App)") k:exit() end)
k:bind({}, ";", function() hs.application.launchOrFocus("Launchpad") k:exit() end)
