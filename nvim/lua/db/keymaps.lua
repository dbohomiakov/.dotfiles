local opts = { noremap = true, silent = true }

--local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.maplocalleader = " "
vim.g.mapleader = " "

keymap("n", "<C-s>", ':Telescope current_buffer_fuzzy_find<CR>', opts)
keymap("n", "<C-s>", ':Telescope current_buffer_fuzzy_find<CR>', opts)

---- NORMAL ----
-- Resize with arrows
--keymap("n", "<A-Up>", ":resize -2<CR>", opts)
--keymap("n", "<A-Down>", ":resize +2<CR>", opts)
--keymap("n", "<A-Left>", ":vertical resize +2<CR>", opts)
--keymap("n", "<A-Right>", ":vertical resize -2<CR>", opts)
-- Move line up and down
keymap("n", "<A-Up>", "<Esc>:m .-2<CR>==", opts)
keymap("n", "<A-Down>", "<Esc>:m .+1<CR>==", opts)
-- add/delete new line
keymap("n", "<C-j>", "o<Esc>==", opts)
keymap("n", "<C-k>", "ddk==", opts)

---- INSERT ---- 
-- ESC emulation
keymap("i", "jk", "<ESC>", opts)
keymap("i", "kj", "<ESC>", opts)

---- VISUAL ---- 
-- ESC emulation
keymap("v", "jk", "<ESC>", opts)
keymap("v", "kj", "<ESC>", opts)
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)
-- Move text up and down
keymap("x", "<A-Down>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-Up>", ":move '<-2<CR>gv-gv", opts)
-- Sort lines
keymap("v", "gs", ": '<,'>sort<CR>", opts)
