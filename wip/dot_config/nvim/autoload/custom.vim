
function! custom#UseSpellConditionally() abort
    let dir = expand('%:p:h')
    if dir != expand($VIMRUNTIME)."/doc"
                \ && dir !~ g:ig_plug_path . "/.*/doc"
        setlocal spell spelllang=en,ru,he
    endif
endfunction

" https://github.com/Valloric/ListToggle/blob/master/plugin/listtoggle.vim
function! custom#LListToggle() abort
    let buffer_count_before = custom#BufferCount()
    " Location list can't be closed if there's cursor in it, so we need 
    " to call lclose twice to move cursor to the main pane
    silent! lclose
    silent! lclose

    if custom#BufferCount() == buffer_count_before
        execute "silent! lopen 10"
    endif
endfunction

function! custom#BufferCount() abort
    return len(filter(range(1, bufnr('$')), 'bufwinnr(v:val) != -1'))
endfunction

" Perform a search in a subshell
function! custom#Grep(args) abort
    let args = split(a:args, ' ')
    return system(join([&grepprg, shellescape(args[0]), get(args, 1, '')], ' '))
endfunction

function! custom#SudoWriteCmd() abort
    silent execute 'doautocmd <nomodeline> BufWritePre'
    silent execute 'write !sudo -A tee >/dev/null %'
    if v:shell_error
        echoerr 'Error! Wrong password?'
    else
        setlocal nomodified buftype=nofile
        silent execute 'doautocmd BufWritePost'
    endif
endfunction

function! custom#SudoReadCmd() abort
    silent %delete_
    silent execute 'doautocmd <nomodeline> BufReadPre'
    silent execute 'read !sudo -A cat 2>/dev/null %'
    if v:shell_error
        echoerr 'Error! Wrong password?'
    else
        silent 1delete_
        setlocal nomodified
        silent execute 'doautocmd BufReadPost'
    endif
endfunction

" Reload the config files. cannot be defined as function!, as
" it is in use during reload
if !exists("*custom#ReloadConfigFiles")
    function custom#ReloadConfigFiles() abort
        silent write
        source $MYVIMRC
        if has('gui_running') && filereadable($MYGVIMRC)
            source $MYGVIMRC
        endif
    endfunction
endif

function! custom#ToggleBoolOpt(option) abort
    execute 'set ' . a:option . '! ' . a:option . '?'
endfunction

function! custom#ToggleFlagOpt(option,flag) abort
    execute ('let lopt = &' . a:option)
    if lopt =~ (".*" . a:flag . ".*")
        execute ('set ' . a:option . '-=' . a:flag . ' ' . a:option . '?')
    else
        execute ('set ' . a:option . '+=' . a:flag . ' ' . a:option . '?')
    endif
endfunction

function! custom#Fzfhistory(arg, bang) abort
    let bang = a:bang || a:arg[len(a:arg)-1] == '!'
    if a:arg[0] == ':'
        call fzf#vim#command_history(bang)
    elseif a:arg[0] == '/'
        call fzf#vim#search_history(bang)
    else
        call fzf#vim#history(fzf#vim#with_preview(), bang)
    endif
endfunction

" Show plugin help
function! custom#Vimplug_doc() abort
    let name = matchstr(getline('.'), '^- \zs\S\+\ze:')
    if has_key(g:plugs, name)
        for doc in split(globpath(g:plugs[name].dir, 'doc/*.txt'), '\n')
            execute 'tabe' doc
        endfor
    endif
endfunction

" Open plugin's GitHub page
function! custom#Vimplug_gx() abort
    let line = getline('.')
    let sha  = matchstr(line, '^  \X*\zs\x\{7,9}\ze ')
    let name = empty(sha) ? matchstr(line, '^[-x+] \zs[^:]\+\ze:')
                \ : getline(search('^- .*:$', 'bn'))[2:-2]
    let uri  = get(get(g:plugs, name, {}), 'uri', '')
    if uri !~ 'github.com'
        return
    endif
    let repo = matchstr(uri, '[^:/]*/'.name)
    let url  = empty(sha) ? 'https://github.com/'.repo
                \ : printf('https://github.com/%s/commit/%s', repo, sha)
    call netrw#BrowseX(url, 0)
endfunction

" Browse help files and README.md
function! custom#Vimplug_help_sink(line) abort
    let dir = g:plugs[a:line].dir
    for pat in ['doc/*.txt', 'README.md']
        let match = get(split(globpath(dir, pat), "\n"), 0, '')
        if len(match)
            execute 'tabedit' match
            return
        endif
    endfor
    tabnew
    execute 'Explore' dir
endfunction
