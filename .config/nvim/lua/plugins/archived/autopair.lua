return {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    enabled = false,
    config = function()
        require("nvim-autopairs").setup({
            disable_filetype = { "TelescopePrompt", "vim" },
        })
    end,
}
