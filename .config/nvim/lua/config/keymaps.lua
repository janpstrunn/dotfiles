local keymap = vim.keymap

-- Toggle Spelling

vim.api.nvim_set_keymap("n", "<F5>", ":set spell!<CR>", { noremap = true, silent = true })

-- Managing files

keymap.set("n", "<leader>w", ":w<CR>")
keymap.set("n", "<leader>cq", ":q!<CR>")
vim.keymap.set("n", "<C-->", "<CMD>Oil<CR>", { desc = "Open parent directory" })

-- Search and Replace

keymap.set("v", "<leader>ss", ":s/")
keymap.set("n", "<leader>sS", ":%s/")

-- File Permissions

keymap.set("n", "<leader>cx", ":!chmod +x %<cr>")

-- Managing lines

keymap.set("v", "K", ":m '<-2<CR>gv=gv", { silent = true }) -- Shift + K in Visual Mode
keymap.set("v", "J", ":m '>+1<CR>gv=gv", { silent = true }) -- Shift + J in Visual Mode

-- Motion

keymap.set("n", "<C-u>", "<C-u>zz") -- Page Up and Center
keymap.set("n", "<C-d>", "<C-d>zz") -- Page Down and Center

keymap.set("n", "n", "nzzzv") -- Next and Center
keymap.set("n", "N", "Nzzzv") -- Previous and Center

-- Visual

keymap.set("n", "<leader>h", ":noh<cr>") -- Remove highlight

-- Panes

keymap.set("n", "<C-h>", "<C-w>h") -- Navigate Left
keymap.set("n", "<C-j>", "<C-w>j") -- Navigate Down
keymap.set("n", "<C-k>", "<C-w>k") -- Navigate Up
keymap.set("n", "<C-l>", "<C-w>l") -- Navigate Right
keymap.set("n", "<C-h>", "TmuxNavigateLeft") -- Navigate Left
keymap.set("n", "<C-j>", "TmuxNavigateDown") -- Navigate Down
keymap.set("n", "<C-k>", "TmuxNavigateUp") -- Navigate Up
keymap.set("n", "<C-l>", "TmuxNavigateRight") -- Navigate Right
keymap.set("n", "<leader>m", ":Maximize<CR>") -- Maximize current pane

-- Indenting

keymap.set("v", "<", "<gv") -- Allows multiple indents without losing cursor
keymap.set("v", ">", ">gv") -- Allows multiple indents without losing cursor

-- Time

keymap.set("n", "<leader>gt", ":r !date +\"\\%H:\\%M\"<CR>")
keymap.set("n", "<leader>gd", ":r !./.config/nvim/lua/scripts/time-day.sh<CR>")

-- LSP

keymap.set('n', '<leader>fmd', vim.lsp.buf.format)

-- Functions

function toggle_checkbox()
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

keymap.set('n', '<leader>ti', toggle_checkbox, { noremap = true, silent = true })

vim.keymap.set("i", "<c-t>", function()
  require("telescope.builtin").registers()
end, { remap = true, silent = false, })

----------------------------------------------------------
--                     Obsidian                         --
----------------------------------------------------------

keymap.set("n", "<leader>oq", ":ObsidianQuickSwitch<cr>")
keymap.set("n", "<leader>ow", ":ObsidianWorkspace<cr>")

keymap.set("n", "<leader>or", ":ObsidianRename<cr>")
keymap.set("n", "<leader>os", ":ObsidianSearch<cr>")
keymap.set("n", "<leader>om", ":ObsidianTOC<cr>")
keymap.set("n", "<leader>ot", ":ObsidianTags<cr>")
keymap.set("n", "<leader>oy", ":ObsidianTemplate<cr>")
keymap.set("n", "<leader>oo", ":ObsidianOpen<cr>")

-- Links

keymap.set("n", "<leader>ol", "viw:ObsidianLink<cr>") -- Link under cursor
keymap.set("v", "<leader>l", ":ObsidianLink<cr>") -- Link in visual mode

keymap.set("n", "<leader>ob", ":ObsidianBacklinks<cr>")
keymap.set("n", "<leader>oB", ":ObsidianLinks<cr>")

-- Others

keymap.set("n", "<leader>oc", ":ObsidianCheck<cr>")

----------------------------------------------------------
--                      Folding                         --
----------------------------------------------------------

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

