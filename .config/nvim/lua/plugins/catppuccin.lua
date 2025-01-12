local config = function()
  require("catppuccin").setup({
    flavour = "mocha", -- latte, frappe, macchiato, mocha
    background = { -- :h background
        light = "latte",
        dark = "mocha",
    },
    transparent_background = false, -- disables setting the background color.
    show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
    term_colors = false, -- sets terminal colors (e.g. `g:terminal_color_0`)
    dim_inactive = {
        enabled = false, -- dims the background color of inactive window
        shade = "dark",
        percentage = 0.15, -- percentage of the shade to apply to the inactive window
    },
    no_italic = false, -- Force no italic
    no_bold = false, -- Force no bold
    no_underline = false, -- Force no underline
    styles = { -- Handles the styles of general hi groups (see `:h highlight-args`):
        comments = { "italic" }, -- Change the style of comments
        conditionals = { "italic" },
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
        -- miscs = {}, -- Uncomment to turn off hard-coded styles
    },
    color_overrides = {
        all = {
            text = "#d9d9d9",
        },
        mocha = {
            rosewater = "#f5e0dc",
            flamingo = "#f2cdcd",
            pink = "#f067fc",
            mauve = "#cba6f7",
            red = "#f38ba8",
            maroon = "#eba0ac",
            peach = "#fab387",
            yellow = "#f9e2af",
            green = "#a6e3a1",
            teal = "#94e2d5",
            sky = "#20dbfc",
            sapphire = "#74c7ec",
            blue = "#5ffcfc",
            lavender = "#b4befe",
            text = "#d9d9d9",
            subtext1 = "#bac2de",
            subtext0 = "#a6adc8",
            overlay2 = "#9399b2",
            overlay1 = "#7f849c",
            overlay0 = "#6c7086",
            surface2 = "#585b70",
            surface1 = "#2a2a2a",
            surface0 = "#212121",
            base = "#161616",
            mantle = "#19191C",
            crust = "#1c1c1f",
        },
        frappe = {},
        macchiato = {},
    },
    custom_highlights = {},
    default_integrations = true,
    integrations = {
        cmp = true,
        gitsigns = true,
        nvimtree = false,
        treesitter = true,
        notify = false,
        dashboard = true,
        mason = true,
        nvim_surround = true,
        markdown = true,
        native_lsp = {
            enabled = true,
            virtual_text = {
                errors = { "italic" },
                hints = { "italic" },
                warnings = { "italic" },
                information = { "italic" },
                ok = { "italic" },
            },
            underlines = {
                errors = { "underline" },
                hints = { "underline" },
                warnings = { "underline" },
                information = { "underline" },
                ok = { "underline" },
            },
            inlay_hints = {
                background = true,
            },
        },
        treesitter_context = true,
        render_markdown = true,
        telescope = {
            enabled = true,
        },
        which_key = true,
        indent_blankline = {
            enabled = true,
            scope_color = "", -- catppuccin color (eg. `lavender`) Default: text
            colored_indent_levels = false,
        },
        mini = {
            enabled = false,
            indentscope_color = "",
        },
    },
})
end

return {
  "catppuccin/nvim", name = "catppuccin",
  priority = 1000,
  config = config,
}
