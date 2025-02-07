return {
	"kylechui/nvim-surround",
	version = "*",
	enabled = true,
	event = "VeryLazy",
	config = function()
		require("nvim-surround").setup({})
	end,
}
