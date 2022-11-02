local wezterm = require("wezterm")
local act = wezterm.action

local config = {
	color_scheme = "tokyonight",
	font = wezterm.font("JetBrainsMono NF", { weight = "Medium" }),
	font_size = 14,
	window_decorations = "RESIZE",
	use_fancy_tab_bar = false,
	hide_tab_bar_if_only_one_tab = true,
	enable_tab_bar = true,
	show_tab_index_in_tab_bar = false,
	send_composed_key_when_right_alt_is_pressed = false,
	leader = { key = ",", mods = "SUPER", timeout_milliseconds = 1000 },
	keys = {
		{
			key = ".",
			mods = "LEADER|SUPER",
			action = act.ActivateKeyTable({
				name = "pane_mode",
				one_shot = false,
			}),
		},
    { key = 'l', mods = 'LEADER', action = act.ShowLauncher },
    { key = 't', mods = 'LEADER', action = act.ShowTabNavigator },
		{ key = ",", mods = "LEADER|SUPER", action = act.ActivatePaneDirection("Next") },
		{ key = "0", mods = "LEADER", action = act.CloseCurrentPane({ confirm = false }) },
		{ key = "3", mods = "LEADER", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
		{ key = "2", mods = "LEADER", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
		{ key = "v", mods = "LEADER", action = act.ActivateCopyMode },
  	{ key = "Backspace", mods = "LEADER", action = act.ClearSelection },
		{ key = "h", mods = "SUPER", action = act.ActivatePaneDirection("Left") },
		{ key = "j", mods = "SUPER", action = act.ActivatePaneDirection("Down") },
		{ key = "k", mods = "SUPER", action = act.ActivatePaneDirection("Up") },
		{ key = "l", mods = "SUPER", action = act.ActivatePaneDirection("Right") },
		{ key = "[", mods = "SUPER", action = act.ActivateTabRelative(-1) },
		{ key = "]", mods = "SUPER", action = act.ActivateTabRelative(1) },
		{ key = "/", mods = "SUPER", action = act.Search({ CaseSensitiveString = "" }) },
	},
	key_tables = {
		pane_mode = {
			{ key = "Escape", action = "ClearKeyTableStack" },
			{ key = "0", action = act.CloseCurrentPane({ confirm = false }) },
			{ key = "3", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
			{ key = "2", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
			{ key = "h", action = act.ActivatePaneDirection("Left") },
			{ key = "j", action = act.ActivatePaneDirection("Down") },
			{ key = "k", action = act.ActivatePaneDirection("Up") },
			{ key = "l", action = act.ActivatePaneDirection("Right") },
		},
	},
}

return config
