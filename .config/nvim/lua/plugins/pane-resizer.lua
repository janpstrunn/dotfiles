return {
  {
    "mboyov/pane-resizer.nvim",
    enabled = false,
    config = function()
      require("pane_resizer").setup({
        NVIMTREE_WIDTH = 40,
        FOCUSED_WIDTH_PERCENTAGE = 0.5,
      })
    end,
  },
}
