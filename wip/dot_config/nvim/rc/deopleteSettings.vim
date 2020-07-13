" Lazy loading on events
augroup load_insert
    autocmd!
    autocmd InsertEnter * call plug#load('deoplete.nvim', 'deoplete-tabnine', 'neco-vim', 'echodoc.vim', 'deoplete-emoji')
                \| exe 'source' g:ig_config_dir . '_deopleteLazySettings.vim'
                \| call deoplete#enable()
                \| exe 'EchoDocEnable'
                \| autocmd! load_insert
augroup END

augroup load_insert_sh
    autocmd!
    autocmd FileType sh,zsh
                \ autocmd InsertEnter * call plug#load('deoplete-zsh')
                \| autocmd! load_insert_sh
augroup END
