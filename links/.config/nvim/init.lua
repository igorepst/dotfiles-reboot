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
    "command! -bang Q q<bang>

    function! Redir(cmd, rng, start, end)
	for win in range(1, winnr('$'))
		if getwinvar(win, 'scratch')
			execute win . 'windo close'
		endif
	endfor
	if a:cmd =~ '^!'
		let cmd = a:cmd =~' %'
			\ ? matchstr(substitute(a:cmd, ' %', ' ' . expand('%:p'), ''), '^!\zs.*')
			\ : matchstr(a:cmd, '^!\zs.*')
		if a:rng == 0
			let output = systemlist(cmd)
		else
			let joined_lines = join(getline(a:start, a:end), '\n')
			let cleaned_lines = substitute(shellescape(joined_lines), "'\\\\''", "\\\\'", 'g')
			let output = systemlist(cmd . " <<< $" . cleaned_lines)
		endif
	else
		redir => output
		execute a:cmd
		redir END
		let output = split(output, "\n")
	endif
	vnew
	let w:scratch = 1
	setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile
	call setline(1, output)
endfunction

command! -nargs=1 -complete=command -bar -range Redir silent call Redir(<q-args>, <range>, <line1>, <line2>)

set rtp+=~/.zsh/volatile/igorepst/_gh_release/vifm/vifm/data/vim 

]])
   require('user.cmp')

vim.api.nvim_add_user_command('Q', 'q', {bang = true})

local wf = vim.fn.expand('~/.work/vim-work.lua')
if vim.fn.filereadable(wf) > 0 then vim.cmd('source ' .. wf) end
