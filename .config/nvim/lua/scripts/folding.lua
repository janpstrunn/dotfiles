-- Originally created by Linkarzu
-- Find the original config here:
-- https://github.com/linkarzu/dotfiles-latest/blob/main/neovim/neobean/lua/config/keymaps.lua

-- Toggle Fold

vim.keymap.set("n", "<CR>", function()
  local line = vim.fn.line(".")
  local foldlevel = vim.fn.foldlevel(line)
  if foldlevel == 0 then
    vim.notify("No fold found", vim.log.levels.INFO)
  else
    vim.cmd("normal! za")
  end
end)

local function fold_headings_of_level(level)
  vim.cmd("normal! gg")
  local total_lines = vim.fn.line("$")
  for line = 1, total_lines do
    local line_content = vim.fn.getline(line)
    if line_content:match("^" .. string.rep("#", level) .. "%s") then
      vim.fn.cursor(line, 1)
      if vim.fn.foldclosed(line) == -1 then
        vim.cmd("normal! za")
      end
    end
  end
end

local function fold_markdown_headings(levels)
  local saved_view = vim.fn.winsaveview()
  for _, level in ipairs(levels) do
    fold_headings_of_level(level)
  end
  vim.cmd("nohlsearch")
  vim.fn.winrestview(saved_view)
end

vim.keymap.set("n", "zu", function()
  vim.cmd("edit!")
  vim.cmd("normal! zR") -- Unfold all headings level 2 or above
end)

vim.keymap.set("n", "zi", function()
  vim.cmd("normal gk")
  vim.cmd("normal! za") -- Fold the heading cursor currently on
end)

vim.keymap.set("n", "zj", function()
  vim.cmd("edit!")
  vim.cmd("normal! zR") -- Fold all headings level 1 or above" }
  fold_markdown_headings({ 6, 5, 4, 3, 2, 1 })
end)

vim.keymap.set("n", "zk", function()
  vim.cmd("edit!")
  vim.cmd("normal! zR") -- Fold all headings level 2 or above
  fold_markdown_headings({ 6, 5, 4, 3, 2 })
  vim.cmd("normal! za") -- Toggle fold all
end)

vim.keymap.set("n", "zl", function()
  vim.cmd("edit!")
  vim.cmd("normal! zR") -- Fold all headings level 3 or above
  fold_markdown_headings({ 6, 5, 4, 3 })
end)

vim.keymap.set("n", "z;", function()
  vim.cmd("edit!")
  vim.cmd("normal! zR") -- Fold all headings level 4 or above
  fold_markdown_headings({ 6, 5, 4 })
end)

vim.keymap.set("v", "<leader>mj", function()
  local start_row = vim.fn.line("v")
  local end_row = vim.fn.line(".")
  if start_row > end_row then
    start_row, end_row = end_row, start_row
  end
  local current_row = start_row
  while current_row <= end_row do
    local line = vim.api.nvim_buf_get_lines(0, current_row - 1, current_row, false)[1]
    if line == "" then
      vim.cmd(current_row .. "delete")
      end_row = end_row - 1
    else
      current_row = current_row + 1
    end
  end
end, { desc = "[J]oin Lines" })

vim.keymap.set("n", "<leader>md", function()
  local cursor_pos = vim.api.nvim_win_get_cursor(0)
  local current_buffer = vim.api.nvim_get_current_buf()
  local start_row = cursor_pos[1] - 1
  local col = cursor_pos[2]
  local line = vim.api.nvim_buf_get_lines(current_buffer, start_row, start_row + 1, false)[1]
  if line:match("^%s*%-") then
    line = line:gsub("^%s*%-", "")
    vim.api.nvim_buf_set_lines(current_buffer, start_row, start_row + 1, false, { line })
    return
  end
  local left_text = line:sub(1, col)
  local bullet_start = left_text:reverse():find("\n")
  if bullet_start then
    bullet_start = col - bullet_start
  end
  local right_text = line:sub(col + 1)
  local bullet_end = right_text:find("\n")
  local end_row = start_row
  while not bullet_end and end_row < vim.api.nvim_buf_line_count(current_buffer) - 1 do
    end_row = end_row + 1
    local next_line = vim.api.nvim_buf_get_lines(current_buffer, end_row, end_row + 1, false)[1]
    if next_line == "" then
      break
    end
    right_text = right_text .. "\n" .. next_line
    bullet_end = right_text:find("\n")
  end
  if bullet_end then
    bullet_end = col + bullet_end
  end
  local text_lines = vim.api.nvim_buf_get_lines(current_buffer, start_row, end_row + 1, false)
  local text = table.concat(text_lines, "\n")
  local new_text = "- " .. text
  local new_lines = vim.split(new_text, "\n")
  vim.api.nvim_buf_set_lines(current_buffer, start_row, end_row + 1, false, new_lines)
end)

vim.keymap.set({ "n", "v" }, "gk", function() -- Go previous header
  vim.cmd("silent! ?^##\\+\\s.*$")
  vim.cmd("nohlsearch")
end)

vim.keymap.set({ "n", "v" }, "gj", function() -- Go next header
  vim.cmd("silent! /^##\\+\\s.*$")
  vim.cmd("nohlsearch")
end)
