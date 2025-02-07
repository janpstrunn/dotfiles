return {
	"folke/zen-mode.nvim",
	cmd = { "ZenMode" },
	keys = {
		{
			"<leader>cz",
			"<cmd>ZenMode<cr>",
			desc = "[Z]en Mode",
		},
	},
	opts = {},
	config = function()
		vim.api.nvim_create_autocmd("User", {
			pattern = "ZenMode",
			callback = function()
				require("lazy").load({ plugins = { "folke/twilight.nvim" } })
			end,
		})
	end,
}
