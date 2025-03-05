local function copy_block_reference()
  local charset = "abcdefghijklmnopqrstuvwxyz0123456789"
  local id = "^"
  for _ = 1, 6 do
    id = id .. charset:sub(math.random(1, #charset), math.random(1, #charset))
  end
  return id
end

local function id_handle()
  local line_number = vim.api.nvim_win_get_cursor(0)[1]
  local line = vim.api.nvim_buf_get_lines(0, line_number - 1, line_number, false)[1]

  local existing_id = line:match("%^%w%w%w%w%w%w")
  local id = existing_id or copy_block_reference()

  if not existing_id then
    line = line .. " " .. id
    vim.api.nvim_buf_set_lines(0, line_number - 1, line_number, false, { line })
  end

  local filename = vim.fn.expand("%:t") -- File name only
  local link = string.format("[[%s#%s]]", filename, id)

  vim.fn.setreg("+", link)
  print("Copied to clipboard: " .. link)
end

vim.keymap.set("n", "<leader>gr", id_handle, { desc = "Copy Block [R]eference" })
