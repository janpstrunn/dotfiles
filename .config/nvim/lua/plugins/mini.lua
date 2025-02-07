local config = function()
	require("mini.align").setup()
	require("mini.ai").setup()
	require("mini.pairs").setup()
	require("mini.comment").setup()
	require("mini.operators").setup()
end

return {
	"echasnovski/mini.nvim",
	version = false,
	config = config,
}
