local keymap = vim.keymap

-- Toggle Spelling

vim.api.nvim_set_keymap("n", "<F5>", ":set spell!<CR>", { noremap = true, silent = true })

-- Managing files

keymap.set("n", "<leader>w", ":w<CR>")
keymap.set("n", "<leader>c", ":q!<CR>")

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
keymap.set("n", "<C-m>", ":Maximize<CR>") -- Maximize current pane

-- Indenting

keymap.set("v", "<", "<gv") -- Allows multiple indents without losing cursor
keymap.set("v", ">", ">gv") -- Allows multiple indents without losing cursor

-- Time

keymap.set("n", "<leader>gt", ":r !date +\"\\%H:\\%M\"<CR>")
keymap.set("n", "<leader>gd", ":r !./.config/nvim/lua/scripts/time-day.sh<CR>")

-- Obsidian

--- OUROBOROS (o-o)

keymap.set("n", "<leader>oof", ":Telescope find_files search_dirs={\"/mnt/pandora/Pandora/obsidian/OUROBOROS\"}<cr>")
keymap.set("n", "<leader>oog", ":Telescope live_grep search_dirs={\"/mnt/pandora/Pandora/obsidian/OUROBOROS\"}<cr>")

-- HEPHAESTUS (o-h)

keymap.set("n", "<leader>ohf", ":Telescope find_files search_dirs={\"/mnt/pandora/Pandora/obsidian/HEPHAESTUS/\"}<cr>")
keymap.set("n", "<leader>ohg", ":Telescope live_grep search_dirs={\"/mnt/pandora/Pandora/obsidian/HEPHAESTUS/\"}<cr>")

-- vim.keymap.set("n", "<leader>on", ":ObsidianTemplate Atomic Note<cr> :lua vim.cmd([[1,/^\\S/s/^\\n\\{1,}//]])<cr>")
-- vim.keymap.set("n", "<leader>of", ":s/\\(# \\)[^_]*_/\\1/ | s/-/ /g<cr>")

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
