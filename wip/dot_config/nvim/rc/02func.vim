nnoremap <Leader><F2> :call custom#ReloadConfigFiles()<CR>


command! ToggleSearchHighlight :call custom#ToggleBoolOpt('hlsearch')
command! TrimTrailingSpace silent! :%s/\s\+$//
command! ReindentEntireFile :execute "normal! mzgg=G`z" | TrimTrailingSpace
command! SudoWrite :call custom#SudoWriteCmd()
command! SudoRead :call custom#SudoReadCmd()

nnoremap <Leader>ph :ToggleSearchHighlight<CR>
nnoremap <Leader>pr :ReindentEntireFile<CR>

command! FormatJSON %!python -m json.tool

