local function insert_git_link()
  local git_dir = vim.fn.system("git rev-parse --show-toplevel"):gsub("%s+", "")

  if git_dir == "" then
    print("Not in a git repository")
    return
  end

local username = os.getenv("USER")
  local repo_name = vim.fn.system("basename " .. git_dir):gsub("%s+", "")
  local file_path = vim.fn.expand("%:p"):gsub(git_dir .. "/", "")
  local url = "https://github.com/" .. username .. "/" .. repo_name .. "/blob/main/" .. file_path

  local filetype = vim.bo.filetype
  local comment_line = ""

  if filetype == "python" then
    comment_line = "# " .. url
  elseif filetype == "rust" then
    comment_line = "// " .. url
  elseif filetype == "sh" then
    comment_line = "# " .. url
  elseif filetype == "lua" then
    comment_line = "-- " .. url
  else
    comment_line = "# " .. url
  end

  vim.api.nvim_put({ comment_line }, "l", true, true)
end

vim.keymap.set("n", "<leader>gi", insert_git_link, { noremap = true, silent = true, desc = "Git: [I]nsert Link" })
