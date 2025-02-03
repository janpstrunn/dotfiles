return {
	"stevearc/conform.nvim",
	opts = {
		formatters_by_ft = {
			lua = { "stylua" },
			sh = { "shfmt" },
			bash = { "shfmt" },
			zsh = { "shfmt" },
			python = { "black" },
			javascript = { "prettier" },
			typescript = { "prettier" },
			json = { "prettier" },
			html = { "prettier" },
			css = { "prettier" },
			-- markdown = { "prettier" },
		},
	},
}
