return {
  'nvim-orgmode/orgmode',
  event = 'VeryLazy',
  enabled = false,
  ft = { 'org' },
  config = function()
    -- Setup orgmode
    require('orgmode').setup({
      org_agenda_files = '~/org/**/*',
      org_default_notes_file = '~/org/refile.org',
    })
    require('nvim-treesitter.configs').setup({
      ignore_install = { 'org' },
    })
  end,
}
