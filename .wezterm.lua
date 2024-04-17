-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

config.font = wezterm.font 'Iosevka Term'
config.font_size = 16
config.hide_tab_bar_if_only_one_tab = true

-- and finally, return the configuration to wezterm
return config
