local status_ok, neoclip = pcall(require, "neoclip")
if not status_ok then
	return
end

neoclip.setup({
  preview = true,
  content_spec_column = false,
  enable_persistent_history = true,
})

