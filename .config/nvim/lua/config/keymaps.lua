local keymap = vim.keymap
local M = {}

-- Toggle Spelling

vim.api.nvim_set_keymap(
	"n",
	"<leader>sp",
	":set spell!<CR>",
	{ noremap = true, silent = true, desc = "[S]pell: Toggle" }
)

-- Managing files

keymap.set("n", "<leader>qq", ":q!<CR>", { desc = "[Q]uit: [Q]uit no save" })
keymap.set("n", "<leader>qa", ":qa!<CR>", { desc = "[Q]uit: [A]ll" })
keymap.set("n", "<leader>qw", ":wq<CR>", { desc = "[Q]uit: [W]rite" })

keymap.set("n", "<M-q>", ":qa!<CR>", { desc = "Exit without saving" })
keymap.set("n", "<M-w>", ":w<CR>", { desc = "Write Changes" })

keymap.set("n", "<M-j>", ":cnext<CR>", { desc = "Next QuickFix" })
keymap.set("n", "<M-k>", ":cprev<CR>", { desc = "Previous QuickFix" })

-- Search and Replace

keymap.set("v", "<leader>ss", ":s/", { desc = "[S]earch: [S]elected" })
keymap.set("n", "<leader>sS", ":%s/", { desc = "[S]earch: Whole File" })

-- File Permissions

keymap.set("n", "<leader>cx", ":!chmod +x %<cr>", { desc = "Make Executable" })

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

-- Panes & Tabs

keymap.set("n", "<C-h>", "<C-w>h") -- Navigate Left
keymap.set("n", "<C-j>", "<C-w>j") -- Navigate Down
keymap.set("n", "<C-k>", "<C-w>k") -- Navigate Up
keymap.set("n", "<C-l>", "<C-w>l") -- Navigate Right
keymap.set("n", "<C-h>", "TmuxNavigateLeft") -- Navigate Left
keymap.set("n", "<C-j>", "TmuxNavigateDown") -- Navigate Down
keymap.set("n", "<C-k>", "TmuxNavigateUp") -- Navigate Up
keymap.set("n", "<C-l>", "TmuxNavigateRight") -- Navigate Right
keymap.set("n", "<leader>mz", ":Maximize<CR>") -- Maximize current pane

keymap.set("n", "<M-t>", ":tabnew<CR>") -- Create a new Tab

-- Indenting

keymap.set("v", "<", "<gv") -- Allows multiple indents without losing cursor
keymap.set("v", ">", ">gv") -- Allows multiple indents without losing cursor

-- URL

keymap.set("n", "<leader>gx", ":!xdg-open <c-r><c-a>", { desc = "Follow URL" })

-- LSP

keymap.set("n", "<leader>fmd", vim.lsp.buf.format, { desc = "[L]SP Format" })

-- Functions

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

keymap.set("n", "<leader>tc", toggle_checkbox, { noremap = true, silent = true, desc = "[T]oggle: [C]heckbox" })

vim.keymap.set("i", "<c-t>", function()
	require("telescope.builtin").registers()
end, { remap = true, silent = false })

----------------------------------------------------------
--                     Obsidian                         --
----------------------------------------------------------

keymap.set("n", "<leader>oq", ":ObsidianQuickSwitch<cr>", { desc = "[Q]uick Switch" })
keymap.set("n", "<leader>ow", ":ObsidianWorkspace<cr>", { desc = "[W]orkspace" })
keymap.set("n", "<leader>or", ":ObsidianRename<cr>", { desc = "[R]ename" })
keymap.set("n", "<leader>os", ":ObsidianSearch<cr>", { desc = "[S]earch" })
keymap.set("n", "<leader>oc", ":ObsidianTOC<cr>", { desc = "To[C]" })
keymap.set("n", "<leader>om", ":ObsidianTags<cr>", { desc = "[M]arks" })
keymap.set("n", "<leader>ot", ":ObsidianTemplate<cr>", { desc = "[T]emplate" })
keymap.set("n", "<leader>oo", ":ObsidianOpen<cr>", { desc = "[O]pen UX" })
keymap.set("n", "<leader>dd", ":ObsidianToday<cr>", { desc = "[D]aily Note" })

-- Links

keymap.set("n", "<leader>ol", "viw:ObsidianLink<cr>", { desc = "[L]ink" }) -- Link under cursor
keymap.set("v", "<leader>l", ":ObsidianLink<cr>", { desc = "[L]ink" }) -- Link in visual mode

keymap.set("n", "<leader>ob", ":ObsidianBacklinks<cr>", { desc = "[B]acklinks" })

keymap.set("n", "<leader>oB", ":ObsidianLinks<cr>", { desc = "Coming Links" })

-- Others

keymap.set("n", "<leader>oz", ":ObsidianCheck<cr>", { desc = "Check plugin" })

----------------------------------------------------------
--                      Linkarzu                        --
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

vim.keymap.set("v", "<leader>mj", function()
	local start_row = vim.fn.line("v")
	local end_row = vim.fn.line(".")
	if start_row > end_row then
		start_row, end_row = end_row, start_row
	end
	local current_row = start_row
	while current_row <= end_row do
		local line = vim.api.nvim_buf_get_lines(0, current_row - 1, current_row, false)[1]
		if line == "" then
			vim.cmd(current_row .. "delete")
			end_row = end_row - 1
		else
			current_row = current_row + 1
		end
	end
end, { desc = "[P]Delete newlines in selected text (join)" })

vim.keymap.set("n", "<leader>md", function()
	local cursor_pos = vim.api.nvim_win_get_cursor(0)
	local current_buffer = vim.api.nvim_get_current_buf()
	local start_row = cursor_pos[1] - 1
	local col = cursor_pos[2]
	local line = vim.api.nvim_buf_get_lines(current_buffer, start_row, start_row + 1, false)[1]
	if line:match("^%s*%-") then
		line = line:gsub("^%s*%-", "")
		vim.api.nvim_buf_set_lines(current_buffer, start_row, start_row + 1, false, { line })
		return
	end
	local left_text = line:sub(1, col)
	local bullet_start = left_text:reverse():find("\n")
	if bullet_start then
		bullet_start = col - bullet_start
	end
	local right_text = line:sub(col + 1)
	local bullet_end = right_text:find("\n")
	local end_row = start_row
	while not bullet_end and end_row < vim.api.nvim_buf_line_count(current_buffer) - 1 do
		end_row = end_row + 1
		local next_line = vim.api.nvim_buf_get_lines(current_buffer, end_row, end_row + 1, false)[1]
		if next_line == "" then
			break
		end
		right_text = right_text .. "\n" .. next_line
		bullet_end = right_text:find("\n")
	end
	if bullet_end then
		bullet_end = col + bullet_end
	end
	local text_lines = vim.api.nvim_buf_get_lines(current_buffer, start_row, end_row + 1, false)
	local text = table.concat(text_lines, "\n")
	local new_text = "- " .. text
	local new_lines = vim.split(new_text, "\n")
	vim.api.nvim_buf_set_lines(current_buffer, start_row, end_row + 1, false, new_lines)
end)

vim.keymap.set({ "n", "v" }, "gk", function() -- Go previous header
	vim.cmd("silent! ?^##\\+\\s.*$")
	vim.cmd("nohlsearch")
end)

vim.keymap.set({ "n", "v" }, "gj", function() -- Go next header
	vim.cmd("silent! /^##\\+\\s.*$")
	vim.cmd("nohlsearch")
end)

----------------------------------------------------------
--                     Janpstrunn                       --
----------------------------------------------------------

-- Nvim Scissors

vim.keymap.set("n", "<leader>se", function()
	require("scissors").editSnippet()
end, { desc = "Snippet: Edit" })

vim.keymap.set({ "n", "x" }, "<leader>sa", function()
	require("scissors").addNewSnippet()
end, { desc = "Snippet: Add" })

-- Current Line Scripts

local function get_current_line()
	return vim.api.nvim_get_current_line()
end

local function set_current_line(new_line)
	vim.api.nvim_set_current_line(new_line)
end

-- Get Current Line (General Usage)

local function insert_text_in_current_line(text_to_insert)
	local current_line = get_current_line()
	local new_line = current_line .. " " .. text_to_insert
	set_current_line(new_line)
end

-- Time

vim.keymap.set("n", "<leader>gd", function()
	insert_text_in_current_line(os.date("%Y-%m-%d %H:%M:%S"))
end, { desc = "Insert [D]ate" })

vim.keymap.set("n", "<leader>gt", function()
	insert_text_in_current_line(os.date("%H:%M:%S"))
end, { desc = "Insert [T]ime" })

-- Copy Block Reference

local function copy_block_reference()
	local charset = "abcdef0123456789"
	local id = "^"
	for _ = 1, 6 do
		local rand = math.random(1, #charset)
		id = id .. charset:sub(rand, rand)
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

-- Week Daily

local function open_weeknote()
	local obsidian_dir = os.getenv("JOURNAL") .. "/Week/"

	local year = os.date("%Y")
	local week = os.date("%U")

	if #week == 1 then
		week = "0" .. week
	end

	local weeknote = string.format("%s-W%s.md", year, week)
	local weeknote_path = obsidian_dir .. weeknote
	vim.cmd("edit " .. weeknote_path)
end

vim.keymap.set("n", "<leader>dw", open_weeknote, { desc = "Open [W]eek Note" })

-- Week Monthly

local function open_monthnote()
	local obsidian_dir = os.getenv("JOURNAL") .. "/Month/"

	local year = os.date("%Y")
	local month = os.date("%B")

	local monthnote = string.format("%s, %s.md", month, year)
	local monthnote_path = obsidian_dir .. monthnote

	vim.cmd("edit " .. monthnote_path)
end

vim.keymap.set("n", "<leader>dm", open_monthnote, { desc = "Open [M]onth Note" })

-- Week Yearly

local function open_yearnote()
	local obsidian_dir = os.getenv("JOURNAL")
	local year = os.date("%Y")
	local yearnote = string.format("%s.md", year)

	local yearnote_path = obsidian_dir .. yearnote

	vim.cmd("edit " .. yearnote_path)
end

vim.keymap.set("n", "<leader>dy", open_yearnote, { desc = "Open [Y]ear Note" })

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
				local new_indent = math.max(6, math.floor(indent_level / 5) * 5)
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

-- Remove all blank lines

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

-- Toggle Wrap

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
