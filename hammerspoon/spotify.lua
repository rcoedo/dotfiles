local application = require "hs.application"
local spotify = require "hs.spotify"
local notify = require "hs.notify"

local function info()
  notify.new("", {title=spotify.getCurrentTrack(), subTitle=spotify.getCurrentArtist()}):send()
end

local function previous()
  spotify.previous()
  info()
end

local function next()
   spotify.next()
   info()
end

local function focus()
  application.launchOrFocus("Spotify")
end

local function playpause()
  spotify.playpause()
end

return {
  info = info,
  previous = previous,
  next = next,
  focus = focus,
  playpause = playpause
}
