return {
  "kiddos/gemini.nvim",
  lazy = true,
  cmd = {
    "GeminiApply",
    "GeminiChat",
    "GeminiCodeExplain",
    "GeminiCodeReview",
    "GeminiFunctionHint",
    "GeminiTask",
    "GeminiUnitTest",
  },
  config = function()
    require("gemini").setup()
  end,
  opts = {
    completion = {
      enabled = true,
      blacklist_filetypes = { 'help', 'qf', 'json', 'yaml', 'toml', 'md' },
      blacklist_filenames = { '.secrets' },
      completion_delay = 600,
      move_cursor_end = false,
      insert_result_key = '<S-Tab>',
    },
  -- instruction = {
  --   enabled = true,
  --   prompts = {
  --     {
  --       name = 'CUSTOM PROMPT',
  --       command_name = 'GeminiCustomPrompt',
  --       menu = 'Custom Prompt 🚀',
  --       get_prompt = function(lines, bufnr)
  --         local code = vim.fn.join(lines, '\n')
  --         local filetype = vim.api.nvim_get_option_value('filetype', { buf = bufnr })
  --         local prompt = 'Context:\n\n```%s\n%s\n```\n\n'
  --             .. 'Write your custom prompt\n'
  --         return string.format(prompt, filetype, code)
  --       end,
  --     },
  --   }
  -- },
  },
}
