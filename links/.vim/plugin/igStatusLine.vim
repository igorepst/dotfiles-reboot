set statusline=
set statusline+=%1*
set statusline+=%{StatuslineMode()}
set statusline+=%9*
set statusline+=\ 
set statusline+=%m
set statusline+=\ 
set statusline+=%r
set statusline+=\ 
set statusline+=%F
set statusline+=%=
set statusline+=%y
set statusline+=\ 
set statusline+=%{&ff}
set statusline+=\ 
set statusline+=%{strlen(&fenc)?&fenc:'none'}
set statusline+=\ 
set statusline+=%c
set statusline+=:
set statusline+=%l
set statusline+=/
set statusline+=%L
set statusline+=\ 
set statusline+=%P
hi User1 ctermbg=lightgreen ctermfg=black guibg=lightgreen guifg=black
hi User9 ctermbg=black ctermfg=white guibg=black guifg=white

function! StatuslineMode()
    let l:mode=mode()
    if l:mode==#"n"
        return "NORMAL"
    elseif l:mode==?"v"
        return "VISUAL"
    elseif l:mode==#"i"
        return "INSERT"
    elseif l:mode==#"R"
        return "REPLACE"
    elseif l:mode==?"s"
        return "SELECT"
    elseif l:mode==#"t"
        return "TERMINAL"
    elseif l:mode==#"c"
        return "COMMAND"
    elseif l:mode==#"!"
        return "SHELL"
    endif
endfunction