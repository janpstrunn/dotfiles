return {
	"stevearc/conform.nvim",
	ft = { "lua", "shell", "python", "javascript", "typescript", "json", "html", "css", "markdown", "rust" },
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
			markdown = { "prettier" },
			rust = { "rustfmt" },
		},
	},
}
