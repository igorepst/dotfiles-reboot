local cmd = vim.cmd
local fn = vim.fn
local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

local function set_options(scope,options)
    for key, value in pairs(options) do
        scopes[scope][key] = value
        if scope ~= 'o' then scopes['o'][key] = value end
    end
end

function ToggleComment(mode)
    if not vim.b.comment_leader then error('Comment leader is undefined') end
    local lines = {}
    local line_number_start, line_number_end, line, hl, ms, tmp
    if mode == 'v' then
        line_number_start = fn.line("'<")
        line_number_end = fn.line("'>")
    else
        line_number_start = fn.line('.')
        line_number_end = line_number_start + (vim.v.count == 0 and 0 or vim.v.count - 1)
    end
    for i = line_number_start, line_number_end do
        line = fn.getline(i)
        if string.find(line, '^%s*$') then
            table.insert(lines, line)
        else
            tmp = string.gsub(vim.b.comment_leader, "%p", "%%%1")
            ms = string.match(line, '^%s*' .. tmp)
            if ms then
                line = string.gsub(line, tmp, "", 1)
                if string.sub(line, 1, 1) == ' ' then
                    line = string.sub(line, 2)
                end
                table.insert(lines, line)
                hl = true
            else
                table.insert(lines, vim.b.comment_leader .. ' ' .. line)
                hl = true
            end
        end
    end
    if hl then
        vim.api.nvim_buf_set_lines(0, line_number_start - 1, line_number_end, false, lines)
        fn.cursor(line_number_end, 0)
        vim.api.nvim_command('nohlsearch')
    end
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
        {"FileType", "dosini", "let b:comment_leader = ';'"};
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
    wildignore = '*.o,*.obj,*.hi',
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
    wildmode = 'longest:full,full',
    title = true,
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
map('n', '<C-_>', ':lua ToggleComment("n")<CR>')
map('v', '<C-_>', ':lua ToggleComment("v")<CR>')
map('i', '<C-_>', '<Esc>:lua ToggleComment("i")<CR>i')

vim.g.netrw_sort_options = "i"
vim.g.netrw_keepdir = 0
vim.g.netrw_hide = 0
vim.g.netrw_banner = 0
vim.g.netrw_browse_split = 4
vim.g.netrw_liststyle = 3
vim.g.netrw_sizestyle = "H"

require('plugins')
map('i', '<C-Space>', 'compe#complete()', {silent = true, expr = true})
map('i', '<CR>', "compe#confirm('<CR>')", {silent = true, expr = true})
map('i', '<Esc>', "compe#close('<Esc>')", {silent = true, expr = true})

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

function SearchInRuntime()
    fn['fzf#vim#grep']("rg --column --line-number --no-heading --color=always '' "
        .. fn.stdpath('config') .. ' ' .. fn.stdpath('data') .. ' ' .. fn.expand("$VIMRUNTIME"), 1)
end

function FindFileInRuntime()
    fn['fzf#run']({sink = "e", source = "fd --type file . "
        .. fn.stdpath('config') .. ' ' .. fn.stdpath('data') .. ' ' .. fn.expand("$VIMRUNTIME")})
end

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

