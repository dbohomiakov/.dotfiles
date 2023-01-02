local wk = require('which-key')

local mappings = {
  e = {"<cmd>NvimTreeToggle<cr>", "Explorer"},
  p = {
    name = "Projects",
    f = {"<cmd>Telescope find_files<cr>", "Telescope Find Files"},
    g = {"<cmd>Telescope live_grep<cr>", "Telescope Live Grep"},
    s = {"<cmd>Telescope projects<cr>", "Telescope List of Projects"},
    r = {"<cmd>lua require('telescope').extensions.neoclip.default()<cr>", "Telescope registers"},
    v = {"<cmd>lua require('swenv.api').pick_venv()<cr>", "Telescope venv"},
  },
  b = {"<cmd>Telescope buffers<cr>", "Telescope List of Buffers"},
  ff = {"<cmd>Telescope file_browser path=%:p:h<cr>", "Telescope File Browser"},
  ["\\"] = {"<cmd>vert sb<cr>", "Split window vertically"},
  ["-"] = {"<cmd>sb<cr>", "Split window horizontally"},
  s = {"<cmd>w<cr>", "Save"},
  k = {"<cmd>q<cr>", "Quit"},
  l = {
    name="LSP",
    s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
    S = {
      "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
      "Workspace Symbols"
    },
    r = { '<cmd>lua require("renamer").rename()<cr>', "Rename" },
  },
}

local opts = {prefix = '<leader>'}
wk.register(mappings, opts)

