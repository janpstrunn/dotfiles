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
}
