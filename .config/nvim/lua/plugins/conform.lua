return {
  "stevearc/conform.nvim",
  ft = { "lua", "shell", "python", "javascript", "typescript", "json", "html", "css", "markdown", "rust", "toml" },
  enabled = true,
  lazy = true,
  opts = {
    formatters_by_ft = {
      bash = { "shfmt" },
      css = { "prettier" },
      html = { "prettier" },
      javascript = { "prettier" },
      json = { "prettier" },
      lua = { "stylua" },
      markdown = { "prettier" },
      nix = { "alejandra" },
      python = { "black" },
      rust = { "rustfmt" },
      sh = { "shfmt" },
      toml = { "taplo" },
      typescript = { "prettier" },
      zsh = { "shfmt" },
    },
  },
}
