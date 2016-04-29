local application = require "hs.application"
local spotify = require "hs.spotify"
local notify = require "hs.notify"
local image = require "hs.image"

local notification = notify.new("")

local function info()
  notification:withdraw()
  notification = notify.new("")
    :subTitle(spotify.getCurrentArtist())
    :title(spotify.getCurrentTrack())
    :setIdImage("resources/spotify.png")
    :send()
end

local function previous()
  spotify.previous()
  os.execute("sleep " .. tonumber(0.1))
  info()
end

local function next()
   spotify.next()
   os.execute("sleep " .. tonumber(0.1))
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
