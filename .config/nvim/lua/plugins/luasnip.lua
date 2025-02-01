local config = function()
  require("luasnip.loaders.from_vscode").lazy_load {
    paths = { "~/.config/nvim/snippets" },
  }
end

return {
  "L3MON4D3/LuaSnip",
  dependencies = { "rafamadriz/friendly-snippets" },
  version = "v2.*",
  config = config,
  build = "make install_jsregexp"
}
