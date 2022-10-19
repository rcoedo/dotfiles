local wezterm = require("wezterm")
local act = wezterm.action

local config = {
	color_scheme = "tokyonight",
	font = wezterm.font("JetBrainsMono NF", { weight = "Medium" }),
	font_size = 14,
	window_decorations = "RESIZE",
	hide_tab_bar_if_only_one_tab = true,
	enable_tab_bar = false,
	--tab_bar_at_bottom = true,
	send_composed_key_when_right_alt_is_pressed = false,
	leader = { key = ",", mods = "CTRL", timeout_milliseconds = 1000 },
	keys = {
		{
			key = ".",
			mods = "CTRL|LEADER",
			action = act.ActivateKeyTable({
				name = "pane_mode",
				one_shot = false,
			}),
		},
		{ key = ",", mods = "LEADER|CTRL", action = act.ActivatePaneDirection("Next") },
		{ key = "0", mods = "LEADER", action = act.CloseCurrentPane({ confirm = false }) },
		{ key = "3", mods = "LEADER", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
		{ key = "2", mods = "LEADER", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
		{ key = "LeftArrow", mods = "SUPER|ALT", action = act.ActivatePaneDirection("Left") },
		{ key = "DownArrow", mods = "SUPER|ALT", action = act.ActivatePaneDirection("Down") },
		{ key = "UpArrow", mods = "SUPER|ALT", action = act.ActivatePaneDirection("Up") },
		{ key = "RightArrow", mods = "SUPER|ALT", action = act.ActivatePaneDirection("Right") },
		{ key = "[", mods = "SUPER", action = act.ActivateTabRelative(-1) },
		{ key = "]", mods = "SUPER", action = act.ActivateTabRelative(1) },
		{ key = "v", mods = "LEADER", action = act.ActivateCopyMode },
		{ key = "f", mods = "LEADER", action = act.Search({ CaseSensitiveString = "" }) },
		{ key = "Backspace", mods = "LEADER", action = act.ClearSelection },
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
