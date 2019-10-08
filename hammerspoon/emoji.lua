local choices = {}
for _, emoji in ipairs(hs.json.decode(io.open("emoji_data/emojis.json"):read())) do
    table.insert(choices,
        {text=emoji['name'],
            subText=table.concat(emoji['kwds'], ", "),
            image=hs.image.imageFromPath("emoji_data/" .. emoji['id'] .. ".png"),
            chars=emoji['chars']
        })
end

-- Focus the last used window.
local function focusLastFocused()
    local wf = hs.window.filter
    local lastFocused = wf.defaultCurrentSpace:getWindows(wf.sortByFocusedLast)
    if #lastFocused > 0 then lastFocused[1]:focus() end
end

-- Create the chooser.
-- On selection, copy the emoji and type it into the focused application.
local emoji_chooser = hs.chooser.new(function(choice)
    if not choice then focusLastFocused(); return end
    hs.pasteboard.setContents(choice["chars"])
    focusLastFocused()
    hs.eventtap.keyStrokes(hs.pasteboard.getContents())
end)

emoji_chooser:searchSubText(true)
emoji_chooser:choices(choices)

return {
  emoji_chooser = emoji_chooser
}
