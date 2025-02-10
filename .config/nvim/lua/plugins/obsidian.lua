return {
  "epwalsh/obsidian.nvim",
  event = "VimEnter",
  lazy = true,
  enabled = function()
    if vim.cmd("silent! uname -a | grep 'Android' || echo 'false'") == false then
      return false
    else
      return true
    end
  end,
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  opts = {
    workspaces = {
      {
        name = "personal",
        path = "/mnt/beelzebub/Pandora/obsidian/OUROBOROS/",
      },
      {
        name = "tree",
        path = "/mnt/beelzebub/Pandora/obsidian/YGGDRASIL/",
      },
      {
        name = "programming",
        path = "/mnt/beelzebub/Pandora/obsidian/HEPHAESTUS/",
      },
    },
    notes_subdir = "Notes",
    log_level = vim.log.levels.INFO,
    completion = {
      nvim_cmp = false,
      min_chars = 2,
    },
    mappings = {
      ["gf"] = {
        action = function()
          return require("obsidian").util.gf_passthrough()
        end,
        opts = { noremap = false, expr = true, buffer = true },
      },
      -- ["<leader>ch"] = {
      --   action = function()
      --     return require("obsidian").util.toggle_checkbox()
      --   end,
      --   opts = { buffer = true },
      -- },
      -- ["<cr>"] = {
      -- 	action = function()
      -- 		return require("obsidian").util.smart_action()
      -- 	end,
      -- 	opts = { buffer = true, expr = true },
      -- },
    },

    new_notes_location = "",

    daily_notes = {
      folder = "Journals/Daily",
      date_format = "%Y-%m-%d",
      template = "Journal - Daily.md",
    },

    note_id_func = function(title)
      if title ~= nil then
        return title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
      else
        local suffix = ""
        for _ = 1, 4 do
          suffix = suffix .. string.char(math.random(65, 90))
        end
        return tostring(os.time()) .. "-" .. suffix
      end
    end,

    ---@param spec { id: string, dir: obsidian.Path, title: string|? }
    ---@return string|obsidian.Path The full path to the new note.
    note_path_func = function(spec)
      local path = spec.dir / tostring(spec.id)
      return path:with_suffix(".md")
    end,

    wiki_link_func = function(opts)
      return opts.file
    end,

    markdown_link_func = function(opts)
      return require("obsidian.util").markdown_link(opts)
    end,
    preferred_link_style = "wiki",
    disable_frontmatter = true,
    ---@return table
    note_frontmatter_func = function(note)
      local out = {}
      if note.metadata ~= nil and not vim.tbl_isempty(note.metadata) then
        for k, v in pairs(note.metadata) do
          out[k] = v
        end
      end
      return out
    end,

    templates = {
      folder = "templates",
      date_format = "%Y-%m-%d",
      time_format = "%H:%M",
      substitutions = {
        week_note = function()
          local current_time = os.time()
          local year = os.date("%Y", current_time)
          local week = os.date("%W", current_time)
          return string.format("[[%s-W%02d]]", year, tonumber(week))
        end,
      },
    },

    use_advanced_uri = false,

    open_app_foreground = false,

    picker = {
      name = "telescope.nvim",
      note_mappings = {
        new = "<C-x>",
        insert_link = "<C-l>",
      },
      tag_mappings = {
        tag_note = "<C-x>",
        insert_tag = "<C-l>",
      },
    },

    sort_by = "modified",
    sort_reversed = true,

    search_max_lines = 1000,

    open_notes_in = "current",

    -- callbacks = {
    -- 	---@param client obsidian.Client
    -- 	post_setup = function(client) end,
    --
    -- 	---@param client obsidian.Client
    -- 	---@param note obsidian.Note
    -- 	enter_note = function(client, note) end,
    --
    -- 	---@param client obsidian.Client
    -- 	---@param note obsidian.Note
    -- 	leave_note = function(client, note) end,
    --
    -- 	---@param client obsidian.Client
    -- 	---@param note obsidian.Note
    -- 	pre_write_note = function(client, note) end,
    --
    -- 	---@param client obsidian.Client
    -- 	---@param workspace obsidian.Workspace
    -- 	post_set_workspace = function(client, workspace) end,
    -- },

    -- Ref: https://github.com/epwalsh/obsidian.nvim
    ui = {
      enable = false, -- set to false to disable all additional syntax features
      update_debounce = 200, -- update delay after a text change (in milliseconds)
      max_file_length = 5000, -- disable UI features for files with more than this many lines
      checkboxes = {
        [" "] = { char = "󰄱", hl_group = "ObsidianTodo" },
        ["x"] = { char = "", hl_group = "ObsidianDone" },
      },
      bullets = { char = "•", hl_group = "ObsidianBullet" },
      external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
      reference_text = { hl_group = "ObsidianRefText" },
      highlight_text = { hl_group = "ObsidianHighlightText" },
      tags = { hl_group = "ObsidianTag" },
      block_ids = { hl_group = "ObsidianBlockID" },
      hl_groups = {
        ObsidianTodo = { bold = true, fg = "#f78c6c" },
        ObsidianDone = { bold = true, fg = "#89ddff" },
        ObsidianRightArrow = { bold = true, fg = "#f78c6c" },
        ObsidianTilde = { bold = true, fg = "#ff5370" },
        ObsidianImportant = { bold = true, fg = "#d73128" },
        ObsidianBullet = { bold = true, fg = "#89ddff" },
        ObsidianRefText = { underline = true, fg = "#c792ea" },
        ObsidianExtLinkIcon = { fg = "#c792ea" },
        ObsidianTag = { italic = true, fg = "#7c5cff" },
        ObsidianBlockID = { italic = true, fg = "#7c5cff" },
        ObsidianHighlightText = { bg = "#75662e" },
      },
    },

    attachments = {
      img_folder = "Resources",

      ---@return string
      img_name_func = function()
        return string.format("%s-", os.time())
      end,

      ---@param client obsidian.Client
      ---@param path obsidian.Path the absolute path to the image file
      ---@return string
      img_text_func = function(client, path)
        path = client:vault_relative_path(path) or path
        return string.format("![%s](%s)", path.name, path)
      end,
    },
  },
}
