-- Remove trailing whitespace from all lines on save

local CleanOnSave = vim.api.nvim_create_augroup('CleanOnSave', {})
vim.api.nvim_create_autocmd({"BufWritePre"}, {
  group = CleanOnSave,
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

-- Sets comment and number line colors to grey
vim.api.nvim_create_augroup("SetCommentColor", { clear = true })
vim.api.nvim_create_autocmd("VimEnter", {
  group = "SetCommentColor",
  callback = function()
    vim.cmd("highlight Comment ctermfg=Grey guifg=Grey")
    vim.cmd("highlight LineNr ctermfg=Grey guifg=Grey")
  end,
})
