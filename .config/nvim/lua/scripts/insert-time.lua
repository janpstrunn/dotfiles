local function get_current_line()
  return vim.api.nvim_get_current_line()
end

local function set_current_line(new_line)
  vim.api.nvim_set_current_line(new_line)
end

local function insert_text_in_current_line(text_to_insert)
  local current_line = get_current_line()
  set_current_line(current_line .. " " .. text_to_insert)
end

vim.keymap.set("n", "<leader>gd", function()
  insert_text_in_current_line(os.date("%Y-%m-%d"))
end, { desc = "Insert [D]ate" })

vim.keymap.set("n", "<leader>gt", function()
  insert_text_in_current_line(os.date("%H:%M"))
end, { desc = "Insert [T]ime" })
