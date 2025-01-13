-- Remove trailing whitespace from all lines on save

local CleanOnSave = vim.api.nvim_create_augroup('CleanOnSave', {})
vim.api.nvim_create_autocmd({"BufWritePre"}, {
  group = CleanOnSave,
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

vim.api.nvim_create_augroup("ChangeColorscheme", { clear = true })
vim.api.nvim_create_autocmd("VimEnter", {
  group = "ChangeColorscheme",
  callback = function()
      vim.cmd("colorscheme catppuccin-mocha")
  end,
})
