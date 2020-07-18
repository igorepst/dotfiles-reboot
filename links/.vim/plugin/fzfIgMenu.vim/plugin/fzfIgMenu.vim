if exists('g:loaded_fzfIgMenu')
  finish
endif
let g:loaded_fzfIgMenu = 1

if !exists('g:fzfIgMenu_dict') | let g:fzfIgMenu_dict = {} | endif
if !exists('g:fzfIgMenu_createCmd') | let g:fzfIgMenu_createCmd = 0 | endif
if !exists('g:fzfIgMenu_cmdPrefix') | let g:fzfIgMenu_cmdPrefix = '' | endif

if (g:fzfIgMenu_createCmd)
    call fzfIgMenu#fzfIgMenuCreateCmd()
endif

nnoremap <silent> <Plug>FzfIgMenuOpen :call fzf#run({'source': sort(keys(g:fzfIgMenu_dict)), 'sink': function('fzfIgMenu#fzfIgMenuRun'), 
            \ 'options': '--reverse', 'down': '40%'})<CR>
