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

config.default_cursor_style = 'BlinkingBlock'
config.cursor_blink_rate = 400
config.cursor_blink_ease_in = "Linear"
config.cursor_blink_ease_out = "Linear"

config.treat_east_asian_ambiguous_width_as_wide = true
config.bold_brightens_ansi_colors = false
config.colors = {
  foreground = 'white',
  background = 'black',
  cursor_fg = 'olive',
  cursor_bg = 'orange',
  cursor_border = 'orange',
  ansi = {
    'black',
    'red',
    'lime',
    'yellow',
    '#00A0FF', --'blue',
    'fuchsia',
    'aqua',
    'white',
  },
  brights = {
    'grey',
    'maroon',
    'green',
    'olive',
    '#00C0FF', --'navy',
    'purple',
    'teal',
    'silver',
  },
}

return config
