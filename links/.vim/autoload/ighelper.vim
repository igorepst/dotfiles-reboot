
function! ighelper#ToggleBoolOpt(option) abort
    execute 'set ' . a:option . '! ' . a:option . '?'
endfunction

