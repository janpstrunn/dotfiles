local function blank_lines_markdown()
  local buf = vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)

  local formatted_lines = {}
  local inside_yaml_frontmatter = false

  for i, line in ipairs(lines) do
    -- Handle YAML frontmatter
    if line:match("^%-%-%-$") then
      if i == 1 or i == #lines then
        inside_yaml_frontmatter = not inside_yaml_frontmatter
      end
    end

    -- Remove all blank lines (except inside YAML frontmatter)
    if line == "" and not inside_yaml_frontmatter then
      goto continue
    end

    table.insert(formatted_lines, line)
    ::continue::
  end

  vim.api.nvim_buf_set_lines(buf, 0, -1, false, formatted_lines)
  print("Blank Lines Removed!")
end

vim.keymap.set("n", "<leader>fmb", blank_lines_markdown, { desc = "Remove [B]lank Lines" })
