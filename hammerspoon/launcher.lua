b = hs.hotkey.modal.new({"cmd"}, "return")

b:bind({"cmd"}, "return", function() end)
b:bind({}, "escape", function() b:exit() end)

b:bind({}, "c", function() hs.application.launchOrFocus("Google Chrome") b:exit() end)
b:bind({}, "t", function() hs.application.launchOrFocus("iTerm") b:exit() end)
b:bind({}, "s", function() hs.application.launchOrFocus("Spotify") b:exit() end)
b:bind({}, "n", function() hs.application.launchOrFocus("Evernote") b:exit() end)
b:bind({}, "e", function() hs.application.launchOrFocus("Emacs") b:exit() end)
b:bind({}, "i", function() hs.application.launchOrFocus("IntelliJ IDEA 15") b:exit() end)
