" <TAB>: completion.
inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ deoplete#manual_complete()
function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction

" <S-TAB>: completion back.
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <expr><C-g> deoplete#refresh()
inoremap <expr><C-e> deoplete#cancel_popup()
inoremap <expr><ESC> pumvisible() ? deoplete#cancel_popup() : "\<ESC>"
inoremap <silent><expr><C-l> deoplete#complete_common_string()

" <CR>: close popup, but stay on the same line
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function() abort
    return pumvisible() ? deoplete#close_popup() : "\<CR>"
endfunction

call deoplete#custom#source('_', 'matchers',
            \ ['matcher_fuzzy', 'matcher_length'])

call deoplete#custom#source('tabnine', 'rank', 300)
call deoplete#custom#source('tabnine', 'min_pattern_length', 2)
call deoplete#custom#var('tabnine', {
            \ 'line_limit': 500,
            \ 'max_num_results': 20,
            \ })

call deoplete#custom#source('zsh', 'filetypes', ['zsh', 'sh'])

call deoplete#custom#source('_', 'converters', [
            \ 'converter_remove_paren',
            \ 'converter_remove_overlap',
            \ 'matcher_length',
            \ 'converter_truncate_abbr',
            \ 'converter_truncate_info',
            \ 'converter_truncate_menu',
            \ 'converter_auto_delimiter',
            \ ])
call deoplete#custom#source('tabnine', 'converters', [
            \ 'converter_remove_overlap',
            \ 'converter_truncate_info',
            \ ])

call deoplete#custom#option('keyword_patterns', {
            \ '_': '[a-zA-Z_]\k*\(?',
            \ 'tex': '[^\w|\s][a-zA-Z_]\w*',
            \ })

call deoplete#custom#option({
            \ 'auto_refresh_delay': 10,
            \ 'auto_complete': v:false,
            \ 'smart_case': v:true,
            \ 'ignore_case': v:true,
            \ 'skip_multibyte': v:true,
            \ 'prev_completion_mode': 'none',
            \ 'auto_preview': v:true,
            \ })

call deoplete#custom#source('emoji', 'filetypes', ['gitcommit', 'markdown'])
call deoplete#custom#source('emoji', 'converters', ['converter_emoji'])
