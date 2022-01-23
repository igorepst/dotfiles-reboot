require('impatient')
vim.cmd([[
    " https://gist.github.com/romainl/56f0c28ef953ffc157f36cc495947ab3
    set grepprg=rg\ --vimgrep

    function! Grep(...)
	return system(join([&grepprg] + [expandcmd(join(a:000, ' '))], ' '))
    endfunction

    command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr Grep(<f-args>)
    command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr Grep(<f-args>)

    cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'
    cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'lgrep') ? 'LGrep' : 'lgrep'

    augroup ig
	autocmd!
	autocmd QuickFixCmdPost cgetexpr cwindow
	autocmd QuickFixCmdPost lgetexpr lwindow

        " Save last cursor position at \ mark and open buffer at last cursor position
        autocmd BufReadPost * if @% !~# "\.git[\/\\]COMMIT_EDITMSG$" && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
         autocmd BufEnter * setlocal formatoptions-=ro
    augroup end

    " Map Q to q and Q! to q!
    command! -bang Q q<bang>
    " Don't use Ex mode, use Q for formatting.
    nnoremap Q gq
]])
require('user.cmp')
