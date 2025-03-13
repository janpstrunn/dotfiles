return {
  "williamboman/mason-lspconfig.nvim",
  opts = {
    ensure_installed = {
      "bashls",
      -- "clangd",
      "dockerls",
      "emmet_ls",
      "efm",
      "eslint",
      "jsonls",
      "lua_ls",
      -- "markdown_oxide",
      -- "marksman",
      "pyright",
      "tailwindcss",
      "ts_ls",
    },
    automatic_installation = false,
  },
  event = "BufReadPre",
  dependencies = "williamboman/mason.nvim",
}
