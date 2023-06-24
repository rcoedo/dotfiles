local wezterm = require("wezterm")
local act = wezterm.action

local config = {
  color_scheme = "tokyonight",
  font = wezterm.font("JetBrainsMono NF", { weight = "Regular" }),
  font_size = 14,
  audible_bell = "Disabled",
  window_decorations = "RESIZE",
  enable_kitty_keyboard = true,
  check_for_updates = false,
  use_fancy_tab_bar = false,
  hide_tab_bar_if_only_one_tab = true,
  enable_tab_bar = true,
  show_tab_index_in_tab_bar = true,
  send_composed_key_when_right_alt_is_pressed = false,
  leader = { key = ",", mods = "CTRL", timeout_milliseconds = 1000 },
  keys = {
    -- Regular bindings
    { key = "h",         mods = "SUPER",       action = act.ActivatePaneDirection("Left") },
    { key = "j",         mods = "SUPER",       action = act.ActivatePaneDirection("Down") },
    { key = "k",         mods = "SUPER",       action = act.ActivatePaneDirection("Up") },
    { key = "l",         mods = "SUPER",       action = act.ActivatePaneDirection("Right") },
    { key = "[",         mods = "SUPER",       action = act.ActivateTabRelative(-1) },
    { key = "]",         mods = "SUPER",       action = act.ActivateTabRelative(1) },
    { key = "u",         mods = "SUPER",       action = act.ScrollByPage(-0.5) },
    { key = "d",         mods = "SUPER",       action = act.ScrollByPage(0.5) },
    { key = "/",         mods = "SUPER",       action = act.Search({ CaseSensitiveString = "" }) },
    { key = ".",         mods = "SUPER",       action = act.TogglePaneZoomState },

    -- Leader bindings
    { key = ",",         mods = "CTRL|LEADER", action = act.ActivatePaneDirection("Next") },
    { key = "0",         mods = "LEADER",      action = act.CloseCurrentPane({ confirm = false }) },
    { key = "2",         mods = "LEADER",      action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "3",         mods = "LEADER",      action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "l",         mods = "LEADER",      action = act.ShowLauncher },
    { key = "t",         mods = "LEADER",      action = act.ShowTabNavigator },
    { key = "v",         mods = "LEADER",      action = act.ActivateCopyMode },
    { key = "Backspace", mods = "LEADER",      action = act.ClearSelection },
  },
}

return config
