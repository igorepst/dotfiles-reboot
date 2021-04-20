local cmd = vim.cmd
local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

local function set_options(scope,options)
    for key, value in pairs(options) do
        scopes[scope][key] = value
        if scope ~= 'o' then scopes['o'][key] = value end
    end
end

-- https://gist.github.com/voyeg3r/223f148a115c17a02a15660cc7335f4c
function ToggleComment()
    vim.api.nvim_exec([[
        if getline('.') =~ '^\s*$'
            " Skip empty line
    elseif getline('.') =~ '^\s*' . b:comment_leader
            " Uncomment the line
            execute 'silent s/\v\s*\zs' . b:comment_leader . '\s*\ze//' | nohlsearch
        else
            " Comment the line
            execute 'silent s/\v^(\s*)/\1' . b:comment_leader . ' /' | nohlsearch
        endif
    ]], true)
end

-- https://github.com/norcalli/nvim_utils
local function nvim_create_augroups(definitions)
    for group_name, definition in pairs(definitions) do
        vim.api.nvim_command('augroup '..group_name)
        vim.api.nvim_command('autocmd!')
        for _, def in ipairs(definition) do
            local command = table.concat(vim.tbl_flatten{'autocmd', def}, ' ')
            vim.api.nvim_command(command)
        end
        vim.api.nvim_command('augroup END')
    end
end

local autocmds = {
    ig_au = {
        {"FileType", "c,cpp,java,rust", "let b:comment_leader = '//'"};
        {"FileType", "sh,bash,zsh,jproperties,tmux,conf,xf86conf,fstab,ps1,python", "let b:comment_leader = '#'"};
        {"FileType", "vim,vifm", "let b:comment_leader = '\"'"};
        {"FileType", "xdefaults", "let b:comment_leader = '!'"};
        {"FileType", "lua,haskell", "let b:comment_leader = '--'"};
    };
}

nvim_create_augroups(autocmds)


local options_global = {
    background = 'light',
    mouse = 'a',
    termguicolors = true,
    shortmess = 'atToOIc',
    completeopt = 'menuone,noselect',
    smarttab = true,
    showmode = false,
    ruler = false,
    clipboard = 'unnamedplus',
    ignorecase = true,
    wildignorecase = true,
    hidden = true,
    autoread = true,
    splitbelow = true,
    splitright = true,
    timeoutlen = 500,
    scrolloff = 5,
    sidescroll = 1,
    sidescrolloff = 5,
    linebreak = true,
    whichwrap = 'b,s,<,>,[,]',
    listchars = 'tab:> ,trail:-,extends:>,precedes:<,nbsp:+,eol:$',
    wildmode = 'longest:full,full'
}

local options_buffer = {
    expandtab = true,
    tabstop = 8,
    softtabstop = 4,
    shiftwidth = 4,
}

local options_window = {
    relativenumber = true
}

set_options('o', options_global)
set_options('b', options_buffer)
set_options('w', options_window)

local function map(mode, lhs, rhs, opts)
    local options = {noremap = true}
    if opts then options = vim.tbl_extend('force', options, opts) end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

vim.g.mapleader = ' '
map('n', '<Leader>bl', ':buffers<CR>')
--  Actually 'C-/' and not 'C-_'
map('n', '<C-_>', ':lua ToggleComment()<CR>')
map('v', '<C-_>', ':lua ToggleComment()<CR>')
map('i', '<C-_>', '<Esc>:lua ToggleComment()<CR>i')

vim.g.netrw_sort_options = "i"
vim.g.netrw_keepdir = 0
vim.g.netrw_hide = 0
vim.g.netrw_banner = 0
vim.g.netrw_browse_split = 4
vim.g.netrw_liststyle = 3
vim.g.netrw_sizestyle = "H"

require('ie.plugins')

--cmd 'colorscheme PaperColor'
--vim.api.nvim_set_var('github_colors_soft',1)
--cmd 'colorscheme github'
--vim.api.nvim_set_var('material_theme_style','lighter')
--vim.api.nvim_set_var('material_terminal_italics', 1)
--cmd 'colorscheme material'
--vim.g.material_style = "lighter-contrast"
--vim.g.material_italic_comments = 1
--vim.g.material_italic_keywords = 1
--vim.g.material_italic_functions = 1
--require('colorbuddy').colorscheme('material')
--vim.g.tempus_enforce_background_color = 1
--cmd 'colorscheme tempus_totus'
cmd 'colorscheme hemisu'



--" Map Q to q and Q! to q!
--command! -bang Q q<bang>
--" Don't use Ex mode, use Q for formatting.
--nnoremap Q gq

--" Break undo sequence on Space, Tab and Enter
--inoremap <Space> <Space><C-g>u
--inoremap <Tab> <Tab><C-g>u
--inoremap <CR> <CR><C-g>u
--" Make it possible to undo CTRL-U in insert mode
--inoremap <C-U> <C-G>u<C-U>

--" Diff between written and current states
--command! DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

--Plug 'vifm/vifm.vim'
--Plug 'PProvost/vim-ps1'
--Plug 'rust-lang/rust.vim'


--" Define one augroup for all autocommands
--augroup ig_au
--    autocmd!
--augroup END

--" Always jump to the last known cursor position.
--autocmd ig_au BufReadPost *
--            \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
--            \ |   exe "normal! g`\""
--            \ | endif

--let g:fzfIgMenu_dict = {
--            \ "Toggle list": #{f: "call ighelper#ToggleBoolOpt('list')", k: "<Leader>tl"},
--            \ "Toggle wrap": #{f: "call ighelper#ToggleBoolOpt('wrap')",},
--            \ "Toggle hlsearch": #{f: "call ighelper#ToggleBoolOpt('hlsearch')",},
--            \ "Convert to DOS": #{f: "set ff=dos",},
--            \ "Show DOS line endings": #{f: "edit ++ff=unix",},
--            \ "Convert to Unix": #{f: "set ff=unix",},
--            \ "Convert to UTF-8": #{f: "set fileencoding=utf8",},
--            \ "Search in runtime": #{f: "call SearchInRuntime()",},
--            \ "Find file in runtime": #{f: "call FindFileInRuntime()",},
--            \ "Reload config": #{f: "call ighelper#ReloadConfigFiles()",},
--            \ "Format JSON": #{f: "%!jq -R '. as $line | try fromjson catch $line'",},
--            \ "Trim trailing space": #{f: "execute ':%s/\\s\\+$//e'",},
--            \ "Reindent file": #{f: "execute 'normal! mzgg=G`z' | IgTrimTrailingSpace",},
--            \ }
--let g:fzfIgMenu_createCmd = 1
--let g:fzfIgMenu_cmdPrefix = 'Ig'

--nmap <Leader><F2> <Plug>FzfIgMenuOpen

--function! SearchInRuntime() abort
--    call fzf#vim#grep("rg --column --line-number --no-heading --color=always '' " . g:igVimPath . ' ' . expand("$VIM"), 1) 
--endfunction

--function! FindFileInRuntime() abort
--    call fzf#run({'sink': 'e', 'source': "fd . " . g:igVimPath . ' ' . expand("$VIM")})
--endfunction

--" matchit.vim - % to jump between pairs
--set matchpairs+=<:> 
--packadd! matchit
--let b:match_ignorecase = 1

--" Large file: more than 10Mb
--autocmd ig_au BufReadPre *
--            \ let f=expand("<afile>") |
--            \ if getfsize(f) > 10485760 |
--            \ set eventignore+=FileType |
--            \ setlocal noswapfile bufhidden=unload buftype=nowrite undolevels=-1 |
--            \ else |
--            \ set eventignore-=FileType |
--            \ endif

