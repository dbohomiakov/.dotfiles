local cmp_status_ok, dressing = pcall(require, "dressing")
if not cmp_status_ok then
  return
end

dressing.setup()
