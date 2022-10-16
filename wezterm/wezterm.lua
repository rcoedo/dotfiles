local wezterm = require("wezterm")

local config = {
  --color_scheme = "MonokaiPro (Gogh)",
  color_scheme = "tokyonight",
  font = wezterm.font('JetBrains Mono', { weight = "Medium" }),
  font_size = 14,
  window_decorations = "RESIZE",
  hide_tab_bar_if_only_one_tab = true,
  send_composed_key_when_right_alt_is_pressed = false,
  --window_padding = {
    --left = 0,
    --right = 0,
    --top = 0,
    --bottom = 0,
    --},
  }

return config
