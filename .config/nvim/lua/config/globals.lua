vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.opt.conceallevel = 2

vim.filetype.add({
  pattern = { [".*/hypr/.*%.conf"] = "hyprlang" },
})
