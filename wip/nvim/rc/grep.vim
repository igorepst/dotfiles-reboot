" https://gist.github.com/romainl/56f0c28ef953ffc157f36cc495947ab3
set grepprg=rg\ --vimgrep\ --ignore-file\ $HOME/.config/ripgrep/ignore\ --smart-case

command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr custom#Grep(<q-args>)
command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr custom#Grep(<q-args>)

autocmd ig_au QuickFixCmdPost cgetexpr cwindow
autocmd ig_au QuickFixCmdPost lgetexpr lwindow

nnoremap <Leader>// :LGrep<Space>

" No relative number in quickfix and location list
autocmd ig_au FileType qf setlocal norelativenumber
command! LListToggle call custom#LListToggle()
nnoremap <Leader>ll :LListToggle<CR>
