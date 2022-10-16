local wezterm = require("wezterm")

local config = {
  --color_scheme = "MonokaiPro (Gogh)",
  color_scheme = "tokyonight",
  font = wezterm.font('FiraCode NF', { weight = "Regular" }),
  window_decorations = "RESIZE",
  hide_tab_bar_if_only_one_tab = true,
  --window_padding = {
    --left = 0,
    --right = 0,
    --top = 0,
    --bottom = 0,
    --},
  }

return config
