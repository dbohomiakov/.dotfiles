-- local status_ok, nordic = pcall(require, "nordic")
-- if not status_ok then
--   return
-- end
--
-- nordic.colorscheme({
--     -- Underline style used for spelling
--     -- Options: 'none', 'underline', 'undercurl'
--     underline_option = 'none',
--
--     -- Italics for certain keywords such as constructors, functions,
--     -- labels and namespaces
--     italic = true,
--
--     -- Italic styled comments
--     italic_comments = false,
--
--     -- Minimal mode: different choice of colors for Tabs and StatusLine
--     minimal_mode = false,
--
--     -- Darker backgrounds for certain sidebars, popups, etc.
--     -- Options: true, false, or a table of explicit names
--     -- Supported: terminal, qf, vista_kind, packer, nvim-tree, telescope, whichkey
--     alternate_backgrounds = false,
--
--     -- Callback function to define custom color groups
--     -- See 'lua/nordic/colors/example.lua' for example defitions
--     custom_colors = function(c, s, cs)
--       return {}
--     end
-- })
--
--

require('solarized').setup({
    theme = 'neo' -- or comment to use solarized default theme.
})


-- -- Setup color scheme
-- vim.g.nord_contrast = true
-- vim.g.nord_borders = false
-- vim.g.nord_disable_background = true
-- vim.g.nord_italic = false
-- vim.g.nord_uniform_diff_background = true
-- vim.cmd("set background=dark")
--
-- require('nord').set()
