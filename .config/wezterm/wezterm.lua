local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.font_size = 17
config.font = wezterm.font_with_fallback({
    { family = "Ricty Diminished Discord" },
    { family = "Emoji One", assume_emoji_presentation = true },
})

config.hide_tab_bar_if_only_one_tab = true
config.treat_east_asian_ambiguous_width_as_wide = true

return config
