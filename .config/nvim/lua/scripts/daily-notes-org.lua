-- These functions allow to quickly move to the daily notes:
-- JOURNAL environment variable is required set at .(shell)rc
-- Day: $JOURNAL/daily/
-- Week: $JOURNAL/weekly/
-- Month: $JOURNAL/monthly/
-- Year: $JOURNAL/

-- Today

local function open_today()
  local org_dir = os.getenv("JOURNAL") .. "/daily/"

  local today = os.date("%F")

  local dailynote = string.format("%s.org", today)
  local dailynote_path = org_dir .. dailynote
  vim.cmd("edit " .. dailynote_path)
end

vim.keymap.set("n", "<leader>dd", open_today, { desc = "[D]aily Note" })

-- Week

local function open_weeknote()
  local org_dir = os.getenv("JOURNAL") .. "/weekly/"

  local year = os.date("%Y")
  local week = tonumber(os.date("%U"))

  week = week + 1

  local week_str = string.format("%02d", week)
  local weeknote = string.format("%s-W%s.org", year, week_str)
  local weeknote_path = org_dir .. weeknote

  vim.cmd("edit " .. weeknote_path)
end

vim.keymap.set("n", "<leader>dw", open_weeknote, { desc = "[W]eek Note" })

-- Monthly

local function open_monthnote()
  local org_dir = os.getenv("JOURNAL") .. "/monthly/"

  local year = os.date("%Y")
  local month = os.date("%B")

  local monthnote = string.format("%s, %s.org", month, year)
  local monthnote_path = org_dir .. monthnote

  vim.cmd("edit " .. monthnote_path)
end

vim.keymap.set("n", "<leader>dm", open_monthnote, { desc = "[M]onth Note" })

-- Yearly

local function open_yearnote()
  local org_dir = os.getenv("JOURNAL")
  local year = os.date("%Y")
  local yearnote = string.format("%s.org", year)

  local yearnote_path = org_dir .. yearnote

  vim.cmd("edit " .. yearnote_path)
end

vim.keymap.set("n", "<leader>dy", open_yearnote, { desc = "[Y]ear Note" })

-- Week Notes Review

-- This will take the YYYY-[W]W.org file format and will open all daily notes
-- from that week range in seven buffers to be reviewed
-- If not ran inside a week note, it will fallback to current week note

local function open_weekly_notes()
  local org_dir = os.getenv("JOURNAL") .. "/daily/"
  local current_file = vim.fn.expand("%:t")
  local year, week

  local match_year, match_week = current_file:match("(%d+)-W(%d+)%.org")
  if match_year and match_week then
    year, week = tonumber(match_year), tonumber(match_week)
  else
    year = tonumber(os.date("%Y"))
    week = tonumber(os.date("%U"))
  end

  local first_day_of_year = os.time({ year = year, month = 1, day = 1 })
  local first_weekday = os.date("*t", first_day_of_year).wday
  local days_to_first_sunday = (first_weekday == 1) and 0 or (8 - first_weekday)
  local first_sunday = first_day_of_year + days_to_first_sunday * 86400

  local week_start = first_sunday + (week - 2) * 7 * 86400

  for i = 0, 6 do
    local day = os.date("%Y-%m-%d", week_start + i * 86400)
    local note_path = org_dir .. day .. ".org"
    vim.cmd("silent edit " .. note_path)
  end
end

vim.keymap.set("n", "<leader>dr", open_weekly_notes, { desc = "[R]eview Week Notes" })

-- Insert Week Notes

-- This will insert all daily notes from the week range, based on the week
-- note in the current buffer, or fallback to current week

local function insert_weekly_notes()
  local current_file = vim.fn.expand("%:t")
  local year, week

  local match_year, match_week = current_file:match("(%d+)-W(%d+)%.org")
  if match_year and match_week then
    year, week = tonumber(match_year), tonumber(match_week)
  else
    year = tonumber(os.date("%Y"))
    week = tonumber(os.date("%U"))
  end

  local first_day_of_year = os.time({ year = year, month = 1, day = 1 })
  local first_weekday = os.date("*t", first_day_of_year).wday
  local days_to_first_sunday = (first_weekday == 1) and 0 or (8 - first_weekday)
  local first_sunday = first_day_of_year + days_to_first_sunday * 86400

  local week_start = first_sunday + (week - 2) * 7 * 86400

  local lines = {}
  for i = 0, 6 do
    local day = os.date("%Y-%m-%d", week_start + i * 86400)
    table.insert(lines, string.format("[[%s]]", day))
  end

  vim.api.nvim_put(lines, "l", true, true)
end

vim.keymap.set("n", "<leader>di", insert_weekly_notes, { desc = "Insert all daily notes for this week" })
