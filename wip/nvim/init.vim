" Map Space as a mapleader, needs unmapping beforehand
nnoremap <Space> <nop>
let mapleader="\<Space>"

" Define one augroup for all autocommands
augroup ig_au
    autocmd!
augroup END

let g:ig_config_dir=stdpath('config') . '/rc/'
for f in split(glob(ig_config_dir . '*.vim'), '\n')
    if f !~ g:ig_config_dir . '_.*$'
        execute 'source' f
    endif
endfor

" Always jump to the last known cursor position.
autocmd ig_au BufReadPost *
            \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
            \ |   execute "normal! g`\""
            \ | endif

set mouse+=a

" matchit.vim - % to jump between pairs
let b:match_ignorecase = 1

" 'noinsert' is needed by deoplete to select first match in popup
set completeopt=longest,menu,preview,noinsert

" Command-line completion
set wildmode=longest:full,full
" Use 'wildcharM' to send <Tab> in mappings
set wildcharm=<Tab>
set wildignorecase

set ignorecase
set number relativenumber
set shortmess+=sI
set timeoutlen=500

runtime! man.vim
" Open Vim help or Man
nnoremap <expr> K (&filetype is# 'vim' ? (':help ' . fnameescape(expand('<cword>')) . "\n") :
            \ (':call man#open_page(0,0,"","' . fnameescape(expand('<cword>')) . "\")\n"))

runtime! menu.vim
nnoremap <Leader>pm :emenu<Space><Tab>

" Toggle invisible characters
nnoremap <Leader><F3> :call custom#ToggleBoolOpt('list')<CR>
set listchars=tab:▸\ ,eol:¬,trail:-,nbsp:+

" Break undo sequence on Space, Tab and Enter
inoremap <Space> <Space><C-g>u
inoremap <Tab> <Tab><C-g>u
inoremap <CR> <CR><C-g>u

set splitbelow splitright
set diffopt+=vertical
nnoremap <Leader>dp :call custom#ToggleFlagOpt('diffopt','algorithm:patience')<CR>
nnoremap <Leader>di :call custom#ToggleFlagOpt('diffopt','indent-heuristic')<CR>

" Map Q to q and Q! to q!
command! -bang  Q q<bang>

" Show a context around the cursor
set scrolloff=5

" 'set' doesn't do path expansion
execute 'set spellfile=' . stdpath('config') . '/spell/en.utf-8.add'
autocmd ig_au BufRead,BufNewFile *.txt call custom#UseSpellConditionally()

" Disable automatic comment leader
autocmd ig_au FileType * setlocal formatoptions-=ro

" Add '-' to use search with '*' on expressions of the form 'xxx-yyy'
set iskeyword+=-

let g:netrw_home = stdpath('data')
let g:netrw_sort_options="i" " Case insensitive sort
let g:netrw_keepdir=0 " Change working dir
let g:netrw_hide=0 " Show all
let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_liststyle=3 " Tree
let g:netrw_sizestyle="H"
nnoremap <expr> <Leader>- ":Explore" . (&filetype ==# 'netrw' ? ".." : ".") . "<CR>"

" Move [selected] lines with autoindent
nnoremap <C-Up>   :<C-u>silent! move-2<CR>==
nnoremap <C-Down> :<C-u>silent! move+<CR>==
inoremap <C-Up>   <Esc>:<C-u>silent! move-2<CR>==gi
inoremap <C-Down> <Esc>:<C-u>silent! move+<CR>==gi
xnoremap <C-Up>   :<C-u>silent! '<,'>move-2<CR>gv=gv
xnoremap <C-Down> :<C-u>silent! '<,'>move'>+<CR>gv=gv

nnoremap <Leader>ws :sp<Space>
nnoremap <Leader>wv :vert split<Space>
nnoremap <Leader>wq :quit<CR>
nnoremap <Leader>wn :vnew<CR>
nnoremap <Leader>wo :only<CR>
nnoremap <Leader>wi :hide<CR>
nnoremap <Leader>wj <C-W><Down>
nnoremap <Leader>wk <C-W><Up>
nnoremap <Leader>wh <C-W><Left>
nnoremap <Leader>wl <C-W><Right>
" Move cursor to window below/right or top-left window.
nnoremap <Leader>ww <C-W>w
" Rotate windows downwards/rightwards
nnoremap <Leader>wr <C-W>r
" Move the current window to a new tab page
nnoremap <Leader>wt <C-W>T
" Make all windows equally high & wide
nnoremap <Leader>w= <C-W>=

nnoremap <Leader>vw :w<CR>
nnoremap <Leader>vx :x<CR>
" Replace the word under cursor
nnoremap <Leader>* :%s/\<<c-r><c-w>\>//g<left><left>
" Autoclose tags
"inoremap ( ()<Left>
"inoremap { {}<Left>
"inoremap [ []<Left>
"inoremap " ""<Left>
"inoremap ' ''<Left>

set tabstop=8
set softtabstop=4
set shiftwidth=4
set expandtab

" Highlight sh as Bash by default
let g:is_bash=1

" Highlight ```[type] code blocks in MD
let g:markdown_fenced_languages = [
            \  'css',
            \  'erb=eruby',
            \  'sh',
            \  'javascript',
            \  'js=javascript',
            \  'json=javascript',
            \  'ruby',
            \  'sass',
            \  'xml',
            \  'vim',
            \]

nnoremap ; :
nnoremap ! :!
nnoremap <TAB> :bnext<CR>
nnoremap <S-TAB> :bprevious<CR>
" Toggle between two buffers, going to next if there is no alternative one,
" skipping deleted
nnoremap <silent> <Leader><Leader> :<C-u>exe v:count ? v:count . 'b' : 'b' . (bufloaded(0) ? '#' : 'n')<CR>

nnoremap <Leader>ov :e $MYVIMRC<CR>

" Integrate Ctrl+C/V system clipboard
set clipboard+=unnamedplus
" Copy text objects to the system clipboard
nnoremap <silent> <Leader>cp "+y
"xnoremap <silent> <Leader>cp "+y
"vnoremap <silent> <Leader>cp "+y

" Create file's directory before saving, if it doesn't exist.
" Original: https://stackoverflow.com/a/4294176/151048
augroup BWCCreateDir
  autocmd!
  autocmd BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))
augroup END
fun! s:MkNonExDir(file, buf)
  if empty(getbufvar(a:buf, '&buftype')) && a:file !~# '\v^\w+\:\/'
    call mkdir(fnamemodify(a:file, ':h'), 'p')
  endif
endfun
