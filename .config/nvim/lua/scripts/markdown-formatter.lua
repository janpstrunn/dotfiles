-- Markdown Formatter
-- Rules
-- 1. Remove extra spaces
-- 2. Remove extra blank lines
-- 3. Correct indent size
-- 4. Unorderd list convention use `-`
-- 5. No empty lists
-- 6. No extra blank lines between list items
-- 7. Hashs starting a line must be a heading
-- 8. Surround headings with blank lines

-- Footnote organization and other Markdown Formatting can be achieve by
-- Prettier and Footnotes.nvim

local function format_markdown()
  local buf = vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)

  local formatted_lines = {}
  local last_was_list = false
  local last_was_heading = false
  local inside_yaml_frontmatter = false

  for i, line in ipairs(lines) do
    -- Define YAML Frontmatter for blacklist
    if line:match("^%-%-%-$") then
      if i == 1 or i == #lines then
        inside_yaml_frontmatter = not inside_yaml_frontmatter
      end
    end

    -- 1. Trim extra spaces
    line = line:gsub("%s+", " "):gsub("%s+$", "")

    -- 2. Remove extra blank lines
    if line == "" and formatted_lines[#formatted_lines] == "" and not inside_yaml_frontmatter then
      goto continue
    end

    -- 3. Correct indent size
    local leading_spaces, list_marker = line:match("^(%s*)([-%*%d]+%.?) ")
    if list_marker then
      local indent_level = #leading_spaces
      if indent_level == 0 then
        line = line:gsub("^%s+", "") -- Top-level lists must not start with spaces
      elseif indent_level % 5 ~= 0 then
        local new_indent = math.max(5, math.floor(indent_level / 5) * 5)
        line = string.rep(" ", new_indent) .. list_marker .. " " .. line:match("^%s*[-*%d]+%.? (.+)")
      end

      last_was_list = true
      last_was_heading = false
    else
      last_was_list = false
    end

    -- 4. Unorderd list convention use `-`
    line = line:gsub("^%s*[*] ", "- ")

    -- 5. No empty lists
    if line:match("^%s*[-%d]+%.?$") and not inside_yaml_frontmatter then
      goto continue
    end

    -- 6. No extra blank lines between list items
    if last_was_list and line == "" and (lines[i + 1] or ""):match("^%s*[-%d]+%.? ") then
      goto continue
    end

    -- 7. Hashs starting a line must be a heading
    if line:match("^#+") then
      line = line:gsub("^(#+)([^# ])", "%1 %2")

      -- 8. Surround headings with blank lines
      if #formatted_lines > 0 and formatted_lines[#formatted_lines] ~= "" then
        table.insert(formatted_lines, "")
      end

      table.insert(formatted_lines, line)
      last_was_heading = true
      goto continue
    end

    -- 8. Surround headings with blank lines
    if last_was_heading then
      last_was_heading = false
      if line ~= "" then
        table.insert(formatted_lines, "")
      end
    end

    table.insert(formatted_lines, line)
    ::continue::
  end

  vim.api.nvim_buf_set_lines(buf, 0, -1, false, formatted_lines)
  print("Formatting done!")
end

vim.keymap.set("n", "<leader>fmm", format_markdown, { desc = "Format [M]arkdown" })
