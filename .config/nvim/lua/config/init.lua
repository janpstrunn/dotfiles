local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Require

require("config.autocmds")
require("config.globals")
require("config.options")
require("config.keymaps")
require("scripts.init")

require("lazy").setup({
  spec = {
    { "LazyVim/LazyVim", import = "lazyvim.plugins" },
    { import = "plugins" },
  },
  defaults = {
    lazy = true, -- Every Plugin is Lazy Loaded
    version = false,
  },
  install = { colorscheme = { "tokyonight", "habamax" } },
  checker = {
    enabled = false, -- check for plugin updates periodically
    notify = false, -- notify on update
  },
  performance = {
    rtp = {
      -- disable some rtp plugins
      disabled_plugins = {
        -- "2html_plugin",
        -- "bugreport",
        -- "compiler",
        -- "ftplugin",
        -- "getscript",
        -- "getscriptPlugin",
        "gzip",
        -- "logipat",
        -- "matchit",
        -- "matchparen",
        -- "netrw",
        -- "netrwFileHandlers",
        -- "netrwPlugin",
        -- "netrwSettings",
        -- "optwin",
        -- "rplugin",
        -- "rrhelper",
        -- "spellfile_plugin",
        -- "synmenu",
        -- "syntax",
        -- "tar",
        "tarPlugin",
        "tohtml",
        "tutor",
        -- "vimball",
        -- "vimballPlugin",
        -- "zip",
        "zipPlugin",
      },
    },
    change_detection = {
      notify = false,
    },
  },
})
