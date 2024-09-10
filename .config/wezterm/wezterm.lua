local wezterm = require 'wezterm'
local config = {}

config.font_size = 17
config.font = wezterm.font_with_fallback({
    { family = "Ricty Diminished Discord" },
    { family = "Emoji One", assume_emoji_presentation = true },
})

return config
