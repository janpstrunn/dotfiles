-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

local CleanOnSave = vim.api.nvim_create_augroup("CleanOnSave", {})
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  group = CleanOnSave,
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

-- -- Set colorscheme
-- vim.api.nvim_create_augroup("ChangeColorscheme", { clear = true })
-- vim.api.nvim_create_autocmd("VimEnter", {
-- 	group = "ChangeColorscheme",
-- 	callback = function()
-- 		vim.cmd("colorscheme catppuccin-mocha")
-- 	end,
-- })

-- Format file on save
-- vim.api.nvim_create_autocmd("BufWritePre", {
-- 	callback = function()
-- 		local mode = vim.api.nvim_get_mode().mode
-- 		local filetype = vim.bo.filetype
-- 		if vim.bo.modified == true and mode == "n" and filetype ~= "oil" then
-- 			vim.cmd("lua vim.lsp.buf.format()")
-- 		else
-- 		end
-- 	end,
-- })

-- Organize Footnotes in Markdown Files
-- vim.api.nvim_create_autocmd("BufWritePre", {
-- 	pattern = "*.md",
-- 	callback = function(args)
-- 		local mode = vim.api.nvim_get_mode().mode
-- 		io.output(io.tmpfile())
-- 		local buf = args.buf or vim.api.nvim_get_current_buf()
-- 		if vim.bo.modified == true and mode == "n" and vim.bo[buf].filetype ~= "markdown" then
-- 			vim.cmd("silent! lua require('footnote').organize_footnotes()")
-- 		end
-- 	end,
-- })

-- Run Conform
vim.api.nvim_create_autocmd({ "FocusLost", "BufLeave" }, {
  pattern = "*",
  callback = function(args)
    local buf = args.buf or vim.api.nvim_get_current_buf()
    if vim.fn.mode() == "n" then
      vim.defer_fn(function()
        if vim.api.nvim_buf_is_valid(buf) then
          require("conform").format({ bufnr = buf })
        end
      end, 100)
    end
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "text", "plaintex", "typst", "gitcommit", "markdown" },
  callback = function()
    vim.opt_local.spelllang = { "pt", "en_us" }
  end,
})

local function disable_lsp_for_notes(bufnr)
  local bufname = vim.api.nvim_buf_get_name(bufnr)
  local file = io.open(bufname, "r")

  if not file then
    print("Error opening file: " .. bufname)
    return
  end

  local disable_lsp = false
  local lines_read = 0
  for line in file:lines() do
    lines_read = lines_read + 1
    if line:find("global_disable_lsp") then
      disable_lsp = true
      break
    end
    if lines_read >= 5 then
      break
    end
  end

  file:close()

  vim.defer_fn(function()
    local attached = false
    for _, client in pairs(vim.lsp.get_clients()) do
      if vim.lsp.buf_is_attached(bufnr, client.id) then
        attached = true
        break
      end
    end

    if disable_lsp then
      if attached then
        vim.cmd("LspStop")
      end
    else
      if not attached then
        vim.cmd("LspStart")
      end
    end
  end, 500)
end

vim.api.nvim_create_autocmd({ "BufEnter", "BufWrite" }, {
  pattern = "*.py",
  callback = function(args)
    disable_lsp_for_notes(args.buf)
  end,
})
