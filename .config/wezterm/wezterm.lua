local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.hide_tab_bar_if_only_one_tab = true
config.initial_cols = 100
config.initial_rows = 64

config.font_size = 20
config.font = wezterm.font_with_fallback({
    { family = "Ricty Diminished Discord" },
    { family = "Emoji One", assume_emoji_presentation = true },
})

config.treat_east_asian_ambiguous_width_as_wide = true
config.bold_brightens_ansi_colors = true

return config
