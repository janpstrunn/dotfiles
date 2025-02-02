return {
	"chenxin-yan/footnote.nvim",
	ft = "markdown",
	config = function()
		require("footnote").setup({
			keys = {
				new_footnote = "<C-f>",
				organize_footnotes = "",
				next_footnote = "]f",
				prev_footnote = "[f",
			},
			organize_on_new = false,
		})
	end,
}
