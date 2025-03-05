local keymap = vim.keymap

local function insert_time_list_item()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local row = cursor[1]
  local time_string = os.date("%H:%M")

  vim.api.nvim_buf_set_lines(0, row, row, false, { "- " .. time_string })

  vim.api.nvim_win_set_cursor(0, { row + 1, 2 })
end

keymap.set("n", "<leader>gt", insert_time_list_item, { desc = "Add [T]imestamp" })
