if exists('g:autoloaded_fzfIgMenu')
    finish
endif
let g:autoloaded_fzfIgMenu = 1

function! fzfIgMenu#fzfIgMenuCreateCmd() abort
    for [key, value] in items(g:fzfIgMenu_dict)
        " Capitalize and trim various characters in key
        let l:keyr = g:fzfIgMenu_cmdPrefix . substitute(substitute(substitute(key, '\(\<.\)', '\u&', 'g'), '\(\s\+\)', '', 'g'), '\(-\)', '', 'g')
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
