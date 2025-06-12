-- Originally created by Piotr1215
-- Find the original config here:
-- https://github.com/Piotr1215/dotfiles/blob/master/.config/nvim/lua/user_functions/tasks.lua

local keymap = vim.keymap

local function create_or_update_task()
  local current_line = vim.fn.getline(".")
  local cursor_pos = vim.fn.col(".")
  local file_path = vim.fn.expand("%:p") -- Get full path of current file
  local line_number = vim.fn.line(".") -- Get current line number

  -- Keywords we are looking for
  local keywords = { "TODO", "HACK", "NOTE", "PERF", "TEST", "WARN" }

  for _, keyword in ipairs(keywords) do
    local start_index, end_index = string.find(current_line, keyword)
    if start_index then
      local task_description = string.sub(current_line, end_index + 2, cursor_pos - 1)
      task_description = string.gsub(task_description, "%(j%)", "")
      local task_tag = "+" .. string.lower(keyword)

      -- Ask for project and other tags
      local project = vim.fn.input("Enter project (empty = none): ")
      local additional_tags_input = vim.fn.input("Enter tags (separated by spaces): ")
      local additional_tags = {}

      -- Prefix each additional tag with a "+"
      for tag in additional_tags_input:gmatch("%S+") do
        table.insert(additional_tags, "+" .. tag)
      end

      -- Prepare the task command
      local task_cmd = string.format('task add %s "%s"', task_tag, task_description)

      -- Add additional tags if available
      if #additional_tags > 0 then
        task_cmd = task_cmd .. " " .. table.concat(additional_tags, " ")
      end

      -- Add project if available
      if project and #project > 0 then
        task_cmd = task_cmd .. " project:" .. project
      end

      -- Execute the task add command
      local output = vim.fn.system(task_cmd)
      print("Output: ", output)

      for line in output:gmatch("[^\r\n]+") do
        local task_id = string.match(line, "Created task (%d+)%.")
        if task_id then
          print("Task ID extracted: ", task_id)

          -- Annotate task with filename and line number in the nvimline format
          local annotation = string.format("nvimline:%s:%s", line_number, file_path)
          local annotate_cmd = string.format('task %s annotate "%s"', task_id, annotation)
          local annotate_output = vim.fn.system(annotate_cmd)

          print("Annotation output: ", annotate_output)
          return
        else
          print("Failed to extract task ID")
        end
      end
    end
  end
end

local function mark_task_done()
  -- Get the current line and parse it
  local line = vim.api.nvim_get_current_line()
  print("Original line: ", line)

  -- Uncomment the line
  vim.cmd("normal! gcc")
  line = vim.api.nvim_get_current_line()
  -- Remove (j) from the line
  line = string.gsub(line, "%s*%(j%)%s*", " ")
  print("Uncommented line: ", line)

  local patterns = { "TODO:", "HACK:", "NOTE:", "PERF:", "TEST:", "WARN:" }
  local taskDescription = nil
  for _, pattern in ipairs(patterns) do
    local start_idx = string.find(line, pattern)
    if start_idx then
      taskDescription = string.sub(line, start_idx + string.len(pattern) + 1)
      break
    end
  end
  print("Task description: ", taskDescription or "nil")

  -- If a task description was found, mark it as done
  if taskDescription then
    local output = vim.fn.system("yes | task description~'" .. taskDescription .. "' done")
    print("Command output: ", output)
    -- Check the command's output to make sure the task was marked done
    if string.find(output, "Completed") then
      -- Delete the current line
      vim.cmd([[normal dd]])
    end
  end
end

local function go_to_task_in_taskwarrior_tui()
  -- Get the current line and save it as the original line
  local original_line = vim.api.nvim_get_current_line()

  -- Uncomment the line
  vim.cmd("normal! gcc")
  local uncommented_line = vim.api.nvim_get_current_line()

  local patterns = { "TODO:", "HACK:", "NOTE:", "PERF:", "TEST:", "WARN:" }
  local taskDescription = nil

  for _, pattern in ipairs(patterns) do
    local start_idx = string.find(uncommented_line, pattern)
    if start_idx then
      taskDescription = string.sub(uncommented_line, start_idx + string.len(pattern) + 1)
      taskDescription = string.sub(taskDescription, 1, 50)
      break
    end
  end

  -- If a task description was found, use it to go to the task in taskwarrior-tui
  if taskDescription then
    vim.fn.system(" ~/.config/taskwarrior-tui/__switch_to_tui.sh '" .. taskDescription .. "'")
  end

  -- Replace the line back with the original
  vim.api.nvim_set_current_line(original_line)
end

keymap.set(
  "n",
  "<leader>tt",
  go_to_task_in_taskwarrior_tui,
  { noremap = true, silent = true, desc = "Open [T]askwarrior TUI" }
)

keymap.set("i", "<C-q>", create_or_update_task, { noremap = true, silent = true, desc = "Task: Create" })

keymap.set("n", "<leader>td", mark_task_done, { noremap = true, silent = true, desc = "Task: [D]one" })
