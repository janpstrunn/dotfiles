local keymap = vim.keymap

local config = function()
  local telescope = require("telescope")
  telescope.setup({
    defaults = {
      layout_strategy = "horizontal",
      layout_config = {
        preview_width = 0.5,
        horizontal = {
          size = {
            width = "95%",
            height = "95%",
          },
        },
      },
    },
    pickers = {
      find_files = {
        previewer = true,
        hidden = true,
      },
      live_grep = {
        previewer = true,
      },
      buffers = {
        previewer = true,
      },
    },
  })
end

return {
	"nvim-telescope/telescope.nvim",
	version = "*",
	lazy = false,
	dependencies = { "nvim-lua/plenary.nvim" },
  opts = {
  },
	config = config,
	keys = {
		keymap.set("n", "<leader>fk", ":Telescope keymaps<CR>", opts),
		keymap.set("n", "<leader>fh", ":Telescope help_tags<CR>", opts),
		keymap.set("n", "<leader>ff", ":Telescope find_files<CR>", opts),
		keymap.set("n", "<leader>fg", ":Telescope live_grep<CR>", opts),
		keymap.set("n", "<leader>fb", ":Telescope buffers<CR>", opts),
	},
}
