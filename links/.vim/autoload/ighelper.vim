
function! ighelper#ToggleBoolOpt(option) abort
    execute 'set ' . a:option . '! ' . a:option . '?'
endfunction

function! ighelper#UseSpellConditionally() abort
    let dir = expand('%:p:h')
    if dir != expand($VIMRUNTIME) && dir != expand($VIMRUNTIME)."/doc"
                \ && dir !~ g:ig_plug_path . '/.*/doc'
        setlocal spell spelllang=en,ru,he
    endif
endfunction
