local actions = require("telescope.actions")

local telescope = require("telescope")
local action_layout = require("telescope.actions.layout")

require('telescope').setup({
  defaults = {
    -- trim the indentation at the beginning of presented line in the result window
    vimgrep_arguments = {
      "rg",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case",
      "--trim"
    },
    mappings = {
      i = {
        ["<esc>"] = actions.close,
        ["<C-u>"] = false,
        ["<C-h>"] = "which_key",
        ["<M-p>"] = action_layout.toggle_preview,
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        -- disable
        ["<C-n>"] = false,
        ["<C-p>"] = false,
      },
      n = {
        -- ["<M-p>"] = action_layout.toggle_preview,
      },
    }
  },
  pickers = {
    find_files = {
      find_command = { "rg", "--files", "--hidden", "--glob", "!.git/*" },
      -- find_command = { "fd", "--type", "f", "--strip-cwd-prefix" }
    },
    -- Default configuration for builtin pickers goes here:
    -- picker_name = {
    --   picker_config_key = value,
    --   ...
    -- }
    -- Now the picker_config_key will be applied every time you call this
    -- builtin picker
  },
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    },
		lsp_handlers = {
			code_action = {
				telescope = require('telescope.themes').get_dropdown({}),
			},
		},
  },
})

telescope.load_extension('lsp_handlers')
telescope.load_extension('fzf')
