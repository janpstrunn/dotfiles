-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.init")

if vim.g.neovide then
  vim.g.neovide_scale_factor = 0.8
  vim.o.guifont = "JetBrainsMono Nerd Font Mono:h13"
end
