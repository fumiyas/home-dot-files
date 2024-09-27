local wezterm = require "wezterm"

local config = wezterm.config_builder()

--config.default_prog = { os.getenv("SHELL"), "-l" }
config.default_cwd = os.getenv("PWD")

config.hide_tab_bar_if_only_one_tab = true
config.enable_scroll_bar = false
config.scrollback_lines = 100000

config.initial_rows = 64
config.initial_cols = 100
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.font_size = 20
config.cell_width = 0.95
config.line_height = 0.90

config.treat_east_asian_ambiguous_width_as_wide = true
config.bold_brightens_ansi_colors = false

config.default_cursor_style = "BlinkingBlock"
config.cursor_blink_rate = 250
config.cursor_blink_ease_in = "Linear"
config.cursor_blink_ease_out = "Linear"

config.font = wezterm.font_with_fallback({
    { family = "Ricty Diminished Discord" },
    { family = "Emoji One", assume_emoji_presentation = true },
})

config.colors = {
  foreground = "white",
  background = "black",
  cursor_fg = "olive",
  cursor_bg = "orange",
  cursor_border = "orange",
  compose_cursor = "blue",
  ansi = {
    "black",
    "red",
    "lime",
    "yellow",
    "#00A0FF", --"blue",
    "fuchsia",
    "aqua",
    "white",
  },
  brights = {
    "grey",
    "maroon",
    "green",
    "olive",
    "#00C0FF", --"navy",
    "purple",
    "teal",
    "silver",
  },
}

return config
