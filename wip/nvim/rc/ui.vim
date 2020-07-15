set termguicolors "True-color terminal support

set background=dark
let g:seoul256_srgb = 1
let g:seoul256_background = 236
colorscheme seoul256

function! Ig_ui_toggle(arg) abort
    if a:arg == 'on'
        set laststatus=2 ruler
    else
        set laststatus=0 noruler
    endif
    set noshowmode
endfunction
call Ig_ui_toggle('on')


let g:lightline = {
            \ 'colorscheme': 'seoul256',
            \ }


let g:terminal_color_0  = '#4e4e4e'
let g:terminal_color_1  = '#d68787'
let g:terminal_color_2  = '#5f865f'
let g:terminal_color_3  = '#d8af5f'
let g:terminal_color_4  = '#85add4'
let g:terminal_color_5  = '#d7afaf'
let g:terminal_color_6  = '#87afaf'
let g:terminal_color_7  = '#d0d0d0'
let g:terminal_color_8  = '#626262'
let g:terminal_color_9  = '#d75f87'
let g:terminal_color_10 = '#87af87'
let g:terminal_color_11 = '#ffd787'
let g:terminal_color_12 = '#add4fb'
let g:terminal_color_13 = '#ffafaf'
let g:terminal_color_14 = '#87d7d7'
let g:terminal_color_15 = '#e4e4e4'

