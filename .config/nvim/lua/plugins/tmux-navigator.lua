return {
  "christoomey/vim-tmux-navigator",
  lazy = true,
  enabled = true,
  cond = function()
    return vim.env.TMUX ~= nil -- Load only if inside a tmux session
  end,
}
