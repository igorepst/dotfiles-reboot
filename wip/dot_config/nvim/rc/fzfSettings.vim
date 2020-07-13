let $FZF_DEFAULT_OPTS='--no-preview --ansi --reverse --height 60%'

let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-x': 'split',
            \ 'ctrl-v': 'vsplit' }

let g:fzf_commits_log_options = '--color=always --abbrev-commit --date=relative '
            \ . '--format="%C(red)%h%Creset - %s %Cgreen(%cr)%Creset %C(bold blue)<%an>%Creset"'

" Toggle status line when entering FZF
autocmd ig_au FileType fzf call Ig_ui_toggle('off')
            \| autocmd ig_au BufLeave <buffer> call Ig_ui_toggle('on')

" Override built-in to support previews
command! -bang -nargs=* History call custom#Fzfhistory(<q-args>, <bang>0)
command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

command! -bar -bang FzfHelpMap call fzf#vim#maps("n",{'options':'--query ^' .
            \ shellescape(substitute(get(g:,"mapleader","\\"), ' ', '\<Space\>', '')) .'f'}, <bang>0)

" From the current dir down
nnoremap <Leader>ff :Files<CR>
nnoremap <Leader>fp :exe 'Files' g:ig_plug_path<CR>
" git ls-files
nnoremap <Leader>fg :GFiles<CR>
" git status
nnoremap <Leader>fs :GFiles?<CR>
" Open buffers
nnoremap <Leader>fb :Buffers<CR>
" Lines in loaded buffers
nnoremap <Leader>fb/ :Lines<Space>
" Lines in the current buffer
nnoremap <Leader>f/ :BLines<Space>
" Commits for the current buffer
nnoremap <Leader>fc :BCommits<CR>
" History - v:old_files and open bufers
nnoremap <Leader>fh :History<CR>
" Command history
nnoremap <Leader>fh: :History:<CR>
" Search history
nnoremap <Leader>fh/ :History/<CR>
nnoremap <Leader>fw :Windows<CR>
nnoremap <Leader>fm :Maps<CR>
nnoremap <Leader>f: :Commands<CR>
nnoremap <Leader>fl :Helptags<CR>
nnoremap <Leader>f? :FzfHelpMap<CR>
nnoremap <Leader>fr :Rg<CR>


