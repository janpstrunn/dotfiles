local config = function()
  local theme = require("lualine.themes.catppuccin")

  -- Elegant Vagrant

  -- Normal
  theme.normal.a.bg = '#7C5CFF'
  theme.normal.b.fg = '#7C5CFF'
  -- theme.normal.c.bg = nil

  -- Insert
  theme.insert.a.bg = '#f067fc'
  theme.insert.b.fg = '#f067fc'
  -- theme.insert.c.bg = nil

  -- Visual
  --
  theme.visual.a.bg = '#02f789'
  theme.visual.b.fg = '#02f789'
  -- theme.visual.c.bg = nil

  -- Replace
  --
  theme.replace.a.bg = '#f4f113'
  theme.replace.b.fg = '#f4f113'
  -- theme.replace.c.bg = nil

  -- Command
  --
  theme.command.a.bg = '#46d9ff'
  theme.command.b.fg = '#46d9ff'
  -- theme.command.c.bg = nil

  require('lualine').setup {
    options = {
      icons_enabled = true,
      theme = theme,
      component_separators = { left = '|', right = '|' },
      section_separators = { left = '', right = '' },
      disabled_filetypes = {
        statusline = {},
        winbar = {},
      },
      ignore_focus = {},
      always_divide_middle = true,
      always_show_tabline = true,
      globalstatus = false,
      refresh = {
        statusline = 100,
        tabline = 100,
        winbar = 100,
      }
    },
    sections = {
      lualine_a = { 'mode' },
      lualine_b = { 'branch', 'diff', 'diagnostics' },
      lualine_c = { 'filename' },
      lualine_x = { 'encoding', 'fileformat', 'filetype' },
      lualine_y = { 'progress' },
      lualine_z = { 'location' }
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = { 'filename' },
      lualine_x = { 'location' },
      lualine_y = {},
      lualine_z = {}
    },
    tabline = {},
    winbar = {},
    inactive_winbar = {},
    extensions = {}
  }
end

return {
  "nvim-lualine/lualine.nvim",
  lazy = false,
  config = config,
}
