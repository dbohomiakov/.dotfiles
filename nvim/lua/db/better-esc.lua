local status_ok, besc = pcall(require, "better_escape")
if not status_ok then
  return
end

besc.setup({
    mapping = {"jk", "kj"},
    timeout = vim.o.timeoutlen,
    clear_empty_lines = false,
    keys = function()
      return vim.api.nvim_win_get_cursor(0)[2] > 1 and '<esc>l' or '<esc>'
    end,
})
