if exists('g:autoloaded_fzfIgMenu')
    finish
endif
let g:autoloaded_fzfIgMenu = 1

function! fzfIgMenu#fzfIgMenuCreateCmd() abort
    for [key, value] in items(g:fzfIgMenu_dict)
        " Capitalize and trim various characters in key
        let l:keyr = substitute(key, '\(\<.\)', '\u&', 'g')
        let l:keyr = substitute(l:keyr, '\(-\|\s\+\|_\)', '', 'g')
        let l:keyr = g:fzfIgMenu_cmdPrefix . l:keyr 
        execute 'command! ' . l:keyr . ' ' . value['f']
        if has_key(value, 'k')
            execute 'nnoremap ' . value['k'] . ' :' . l:keyr . '<CR>'
        endif
    endfor
endfunction

function! fzfIgMenu#fzfIgMenuRun(e) abort
    for [key, value] in items(g:fzfIgMenu_dict)
        if key ==# a:e
            execute value['f']
            break
        endif
    endfor
endfunction
