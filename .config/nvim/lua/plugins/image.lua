-- https://github.com/3rd/image.nvim
-- Config Template From
-- https://github.com/linkarzu/github/dotfiles-latest/neovim/neobean/lua/plugins/image-nvim.lua

return {
  {
    "vhyrro/luarocks.nvim",
    priority = 1001,
    opts = {
      rocks = { "magick" },
    },
  },
  {
    "3rd/image.nvim",
    ft = { "markdown" },
    lazy = true,
    enabled = true,
    dependencies = { "luarocks.nvim" },
    config = function()
      require("image").setup({
        backend = "kitty",
        kitty_method = "normal",
        integrations = {
          markdown = {
            enabled = true,
            clear_in_insert_mode = true,
            download_remote_images = true,
            only_render_image_at_cursor = false,
            filetypes = { "markdown", "vimwiki", "html" },
          },
          neorg = {
            enabled = true,
            clear_in_insert_mode = false,
            download_remote_images = true,
            only_render_image_at_cursor = false,
            filetypes = { "norg" },
          },
          html = {
            enabled = true,
            only_render_image_at_cursor = true,
            filetypes = { "html", "xhtml", "htm", "markdown" },
          },
          css = {
            enabled = true,
          },
        },
        max_width = nil,
        max_height = nil,
        max_width_window_percentage = nil,

        max_height_window_percentage = 20,

        window_overlap_clear_enabled = false,
        window_overlap_clear_ft_ignore = { "cmp_menu", "cmp_docs", "" },

        editor_only_render_when_focused = true,

        tmux_show_only_in_active_window = true,

        hijack_file_patterns = { "*.png", "*.jpg", "*.jpeg", "*.gif", "*.webp", "*.avif" },
      })
    end,
  },
}
