function OpenWikilink()
  local line = vim.api.nvim_get_current_line()
  local col = vim.fn.col('.') - 1
  local link_start = line:sub(1, col):find("%[%[[^%]]*$")
  local link_end = line:find("]]", col)

  if link_start and link_end then
    local link = line:sub(link_start + 2, link_end - 1)
    local file = link .. ".md"
    vim.cmd("edit " .. file)
  else
    vim.cmd("normal! gf")
  end
end

vim.api.nvim_set_keymap('n', 'gF', [[:lua OpenWikilink()<CR>]], { noremap = true, silent = true })
