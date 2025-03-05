local function toggle_wrap()
  if vim.wo.wrap then
    vim.wo.wrap = false
    vim.wo.linebreak = false
    print("Wrap Disabled")
  else
    vim.wo.wrap = true
    vim.wo.linebreak = true
    print("Wrap Enabled")
  end
end

vim.keymap.set("n", "<leader>cw", toggle_wrap, { desc = "Toggle [W]rap" })
