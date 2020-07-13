let autoload_plug_path = stdpath('data') . '/site/autoload/plug.vim'
if !filereadable(autoload_plug_path)
    silent execute '!curl -fLo ' . autoload_plug_path . '  --create-dirs
                \ "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"'
    autocmd ig_au VimEnter * PlugInstall --sync | source $MYVIMRC
endif
unlet autoload_plug_path


let g:ig_plug_path=stdpath('data') . '/site/plugged'
call plug#begin(g:ig_plug_path)
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins', 'on': [] }
Plug 'tbodt/deoplete-tabnine', { 'do': './install.sh', 'on': [] }
Plug 'Shougo/neco-vim', { 'on': [] }
Plug 'Shougo/context_filetype.vim'
Plug 'zchee/deoplete-zsh', { 'on': [] }
Plug 'Shougo/echodoc.vim', { 'on': [] }
Plug 'fszymanski/deoplete-emoji', { 'on': [] }

Plug 'scrooloose/nerdcommenter'
Plug 'chrisbra/Colorizer'
Plug 'plytophogy/vim-diffchanges'
Plug 'mtdl9/vim-log-highlighting'

Plug 'junegunn/seoul256.vim'
Plug 'itchyny/lightline.vim'

Plug 'vifm/vifm.vim'
call plug#end()


" 'H' to show plugin's doc
autocmd ig_au FileType vim-plug nnoremap <buffer> <silent> H :call custom#Vimplug_doc()<cr>

" 'gx' to open GitHub URL in browser
autocmd ig_au FileType vim-plug nnoremap <buffer> <silent> gx :call custom#Vimplug_gx()<cr>

" Browse help files and README.md
command! PlugHelp call fzf#run(fzf#wrap({
            \ 'source': sort(keys(g:plugs)),
            \ 'sink':   function('custom#Vimplug_help_sink')}))

