local markdown_footnote = {
  "chenxin-yan/footnote.nvim",
  ft = "markdown",
  enabled = false,
  lazy = true,
  config = function()
    require("footnote").setup({
      keys = {
        new_footnote = "<C-f>",
        organize_footnotes = "",
        next_footnote = "]f",
        prev_footnote = "[f",
      },
      organize_on_new = false,
    })
  end,
}

local markdown_bullet = {
  "bullets-vim/bullets.vim",
  enabled = true,
  lazy = true,
  ft = { "markdown" },
  config = function()
    vim.g.bullets_delete_last_bullet_if_empty = 1
  end,
}

local markdown_table = {
  "SCJangra/table-nvim",
  event = "VeryLazy",
  ft = "markdown",
  enabled = false,
  opts = {
    padd_column_separators = true, -- Insert a space around column separators.
    mappings = { -- next and prev work in Normal and Insert mode. All other mappings work in Normal mode.
      next = "<TAB>", -- Go to next cell.
      prev = "<S-TAB>", -- Go to previous cell.
      insert_row_up = "<A-k>", -- Insert a row above the current row.
      insert_row_down = "<A-j>", -- Insert a row below the current row.
      move_row_up = "<A-S-k>", -- Move the current row up.
      move_row_down = "<A-S-j>", -- Move the current row down.
      insert_column_left = "<A-h>", -- Insert a column to the left of current column.
      insert_column_right = "<A-l>", -- Insert a column to the right of current column.
      move_column_left = "<A-S-h>", -- Move the current column to the left.
      move_column_right = "<A-S-l>", -- Move the current column to the right.
      insert_table = "<A-t>", -- Insert a new table.
      insert_table_alt = "<A-S-t>", -- Insert a new table that is not surrounded by pipes.
      delete_column = "<A-d>", -- Delete the column under cursor.
    },
  },
}

local markdown_binds = {
  "tadmccorkle/markdown.nvim",
  event = "VeryLazy",
  enabled = false,
  ft = "markdown",
  opts = {
    -- Disable all keymaps by setting mappings field to 'false'.
    -- Selectively disable keymaps by setting corresponding field to 'false'.
    mappings = {
      inline_surround_toggle = "<leader>mt", -- (string|boolean) toggle inline style
      inline_surround_toggle_line = "<leader>ml", -- (string|boolean) line-wise toggle inline style
      inline_surround_delete = "<leader>mu", -- (string|boolean) delete emphasis surrounding cursor
      inline_surround_change = "<leader>mc", -- (string|boolean) change emphasis surrounding cursor
      link_add = "<leader>mp", -- (string|boolean) add link
      link_follow = false, -- (string|boolean) follow link
      go_curr_heading = "]c", -- (string|boolean) set cursor to current section heading
      go_parent_heading = "]p", -- (string|boolean) set cursor to parent section heading
      go_next_heading = "]]", -- (string|boolean) set cursor to next section heading
      go_prev_heading = "[[", -- (string|boolean) set cursor to previous section heading
    },
    inline_surround = {
      -- For the emphasis, strong, strikethrough, and code fields:
      -- * 'key': used to specify an inline style in toggle, delete, and change operations
      -- * 'txt': text inserted when toggling or changing to the corresponding inline style
      emphasis = {
        key = "i",
        txt = "*",
      },
      strong = {
        key = "b",
        txt = "**",
      },
      strikethrough = {
        key = "s",
        txt = "~~",
      },
      code = {
        key = "c",
        txt = "`",
      },
    },
    link = {
      paste = {
        enable = true, -- whether to convert URLs to links on paste
      },
    },
    toc = {
      -- Comment text to flag headings/sections for omission in table of contents.
      omit_heading = "toc omit heading",
      omit_section = "toc omit section",
      -- Cycling list markers to use in table of contents.
      -- Use '.' and ')' for ordered lists.
      markers = { "-" },
    },
    -- Hook functions allow for overriding or extending default behavior.
    -- Called with a table of options and a fallback function with default behavior.
    -- Signature: fun(opts: table, fallback: fun())
    hooks = {
      -- Called when following links. Provided the following options:
      -- * 'dest' (string): the link destination
      -- * 'use_default_app' (boolean|nil): whether to open the destination with default application
      --   (refer to documentation on <Plug> mappings for explanation of when this option is used)
      follow_link = nil,
    },
    on_attach = nil, -- (fun(bufnr: integer)) callback when plugin attaches to a buffer
  },
}

return {
  markdown_table,
  markdown_binds,
  markdown_bullet,
  markdown_footnote,
}
