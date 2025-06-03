local uv = vim.loop

vim.keymap.set('n', 'gF', function()
  local function OpenWikiLink(root, target)
    local result = nil
    local function scan(dir)
      for name, type in vim.fs.dir(dir) do
        local fullpath = dir .. "/" .. name
        if name == target then
          result = fullpath
          return true
        elseif type == "directory" then
          if scan(fullpath) then return true end
        end
      end
    end
    scan(root)
    return result
  end

  local line = vim.api.nvim_get_current_line()
  local col = vim.fn.col('.') - 1

  -- Detect [[...]]
  local link_start = line:sub(1, col):find("%[%[[^%]]*$")
  local link_end = line:find("]]", col)
  if not (link_start and link_end) then
    vim.cmd("normal! gf")
    return
  end

  local link = line:sub(link_start + 2, link_end - 1)
  local title, anchor = link:match("^(.-)#(%^.-)$")  -- title and optional blockref
  if not title then title = link end

  local filename = title .. ".md"

  -- Search for the file in ../ and all subdirs recursively
  local cwd = vim.fn.expand('%:p:h')
  local search_root = vim.fn.resolve(cwd .. "/..")
  local found = OpenWikiLink(search_root, filename)

  if not found then
    print("File not found: " .. filename)
    return
  end

  vim.cmd("edit " .. found)

  if anchor then
    vim.defer_fn(function()
      vim.cmd("normal! gg")
      vim.fn.search(anchor, 'W')
    end, 100)
  end
end, { desc = "Navigate to Wikilink at point", noremap = true, silent = true })
