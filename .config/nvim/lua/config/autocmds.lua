-- Remove trailing whitespace from all lines on save

local CleanOnSave = vim.api.nvim_create_augroup('CleanOnSave', {})
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  group = CleanOnSave,
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

-- Set colorscheme
vim.api.nvim_create_augroup("ChangeColorscheme", { clear = true })
vim.api.nvim_create_autocmd("VimEnter", {
  group = "ChangeColorscheme",
  callback = function()
    vim.cmd("colorscheme catppuccin-mocha")
  end,
})

-- Format file on save
vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function()
    local mode = vim.api.nvim_get_mode().mode
    local filetype = vim.bo.filetype
    if vim.bo.modified == true and mode == 'n' and filetype ~= "oil" then
      vim.cmd('lua vim.lsp.buf.format()')
    else
    end
  end
})
