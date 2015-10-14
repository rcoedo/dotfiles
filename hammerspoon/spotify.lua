s = hs.hotkey.modal.new({"alt"}, "space")

s:bind({}, "escape", function() s:exit() end)

s:bind({"alt"}, "space", function() hs.spotify.play() s:exit() end)
s:bind({}, "p", function() hs.spotify.previous() s:exit() end)
s:bind({}, "n", function() hs.spotify.next() s:exit() end)
s:bind({}, "c", function() hs.spotify.displayCurrentTrack() s:exit() end)
