vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.filetype.add({
  pattern = { [".*/hypr/.*%.conf"] = "hyprlang" },
})
