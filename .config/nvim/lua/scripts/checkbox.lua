local function toggle_checkbox()
  local line = vim.api.nvim_get_current_line()
  if line:match("^%s*-%s*%[ %]") then
    local new_line = line:gsub("%[ %]", "[x]")
    vim.api.nvim_set_current_line(new_line)
  elseif line:match("^%s*-%s*%[x%]") then
    local new_line = line:gsub("%[x%]", "[ ]")
    vim.api.nvim_set_current_line(new_line)
  else
    print("No checkbox found on this line")
  end
end

vim.keymap.set("n", "<leader>tc", toggle_checkbox, { noremap = true, silent = true, desc = "Toggle: [C]heckbox" })
