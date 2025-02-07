local mason = {
	"williamboman/mason.nvim",
	cmd = "Mason",
	event = "BufReadPre",
	opts = {
		ui = {
			icons = {
				package_installed = "✓",
				package_pending = "➜",
				package_uninstalled = "✗",
			},
		},
	},
}

local mason_lspconfig = {
	"williamboman/mason-lspconfig.nvim",
	opts = {
		ensure_installed = {
			"bashls",
			"clangd",
			"dockerls",
			"emmet_ls",
			"efm",
			"eslint",
			"jsonls",
			"lua_ls",
			"markdown_oxide",
			"marksman",
			"pyright",
			"tailwindcss",
			"ts_ls",
		},
		automatic_installation = false,
	},
	event = "BufReadPre",
	dependencies = "williamboman/mason.nvim",
}

return {
	mason,
	mason_lspconfig,
}
