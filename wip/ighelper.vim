
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

" https://github.com/Valloric/ListToggle/blob/master/plugin/listtoggle.vim
function! ighelper#LListToggle() abort
    let buffer_count_before = ighelper#BufferCount()
    " Location list can't be closed if there's cursor in it, so we need 
    " to call lclose twice to move cursor to the main pane
    silent! lclose
    silent! lclose

    if ighelper#BufferCount() == buffer_count_before
        execute "silent! lopen 10"
    endif
endfunction

function! ighelper#BufferCount() abort
    return len(filter(range(1, bufnr('$')), 'bufwinnr(v:val) != -1'))
endfunction

" Perform a search in a subshell
function! ighelper#Grep(args) abort
    let args = split(a:args, ' ')
    return system(join([&grepprg, shellescape(args[0]), get(args, 1, '')], ' '))
endfunction

" Reload the config files. cannot be defined as function!, as
" it is in use during reload
if !exists("*ighelper#ReloadConfigFiles")
    function ighelper#ReloadConfigFiles() abort
        silent write
        source $MYVIMRC
        if has('gui_running') && filereadable($MYGVIMRC)
            source $MYGVIMRC
        endif
    endfunction
endif
