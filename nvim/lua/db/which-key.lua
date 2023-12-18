local wk = require('which-key')

local mappings = {
  e = {"<CMD>NvimTreeToggle<CR>", "Explorer"},
  p = {
    name = "Projects",
    f = {"<CMD>Telescope find_files<CR>", "Find Files"},
    g = {"<CMD>Telescope live_grep<CR>", "Live Grep"},
    s = {"<CMD>Telescope projects<CR>", "List of Projects"},
    r = {"<CMD>lua require('telescope').extensions.neoclip.default()<CR>", "Registers"},
    v = {"<CMD>lua require('swenv.api').pick_venv()<CR>", "venv"},
    F = {"<CMD>lua fuzzyFindFiles()<CR>"},
  },
  b = {
    name="Buffers",
    b = {"<CMD>Telescope buffers<CR>", "List of Buffers"},
    n = {"<CMD>new<CR>", "New Buffer"},
  },
  ff = {"<CMD>Telescope file_browser path=%:p:h<CR>", "Telescope File Browser"},
  ["\\"] = {"<CMD>vert sb<CR>", "Split window vertically"},
  ["-"] = {"<CMD>sb<CR>", "Split window horizontally"},
  s = {"<CMD>w<CR>", "Save"},
  k = {"<CMD>q<CR>", "Quit"},
  l = {
    name="LSP",
    s = {"<CMD>Telescope lsp_document_symbols<CR>", "Document Symbols"},
    S = {
      "<CMD>Telescope lsp_dynamic_workspace_symbols<CR>",
      "Workspace Symbols"
    },
    r = {"<CMD>lua require('renamer').rename()<CR>", "Rename"},
    o = {"<CMD>SymbolsOutline<CR>", "Outline"},
    --f  = {"<CMD>Telescope lsp_references<CR>", "List of references"},
    -- f = {"<CMD>vim.lsp.buf.format<CR>", "Format"},
    -- a = {"<CMD>vim.lsp.buf.code_action<CR>", "Code action"},
    -- a = {"<CMD>vim.lsp.buf.add_workspace_folder<CR>", ""}
  },
}

local opts = {prefix = '<leader>'}
wk.register(mappings, opts)
