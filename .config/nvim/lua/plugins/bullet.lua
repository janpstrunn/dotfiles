return {
  "bullets-vim/bullets.vim",
  lazy = true,
  ft = { "markdown" },
  config = function()
    vim.g.bullets_delete_last_bullet_if_empty = 1
  end,
}
