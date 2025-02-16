-- Ref: https://github.com/epwalsh/obsidian.nvim

return {
  "epwalsh/obsidian.nvim",
  event = "VimEnter",
  lazy = true,
  enabled = false,
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  opts = {
    workspaces = {
      {
        name = "personal",
        path = "/path/to/vault",
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
    -- 	post_setup = function(client) end,
    -- 	enter_note = function(client, note) end,
    -- 	leave_note = function(client, note) end,
    -- 	pre_write_note = function(client, note) end,
    -- 	post_set_workspace = function(client, workspace) end,
    -- },

    ui = {
      enable = false,
    },

    attachments = {
      img_folder = "Embed",

      img_name_func = function()
        return string.format("%s-", os.time())
      end,

      img_text_func = function(client, path)
        path = client:vault_relative_path(path) or path
        return string.format("![%s](%s)", path.name, path)
      end,
    },
  },
}
