vim.cmd([[
    " Save last cursor position at \ mark and open buffer at last cursor position
    augroup LastCursorPos
    autocmd!
    autocmd BufReadPost * if @% !~# "\.git[\/\\]COMMIT_EDITMSG$" && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    augroup end
]])
