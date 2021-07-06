local cmd = vim.cmd
local fn = vim.fn
local opt = vim.opt
local g = vim.g

-- TODO switch for commentstring
function ToggleComment(mode)
    if not vim.b.comment_leader then error('Comment leader is undefined') end
    local lines = {}
    local line_number_start, line_number_end, line, hl, ms, tmp
    if mode == 'v' then
        line_number_start = vim.fn.line("'<")
        line_number_end = vim.fn.line("'>")
    else
        line_number_start = vim.fn.line('.')
        line_number_end = line_number_start + (vim.v.count == 0 and 0 or vim.v.count - 1)
    end
    for i = line_number_start, line_number_end do
        line = vim.fn.getline(i)
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
        vim.fn.cursor(line_number_end, 0)
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
        {"FileType", "sh,bash,zsh,jproperties,tmux,conf,xf86conf,fstab,ps1,python,yaml", "let b:comment_leader = '#'"};
        {"FileType", "vim,vifm", "let b:comment_leader = '\"'"};
        {"FileType", "xdefaults", "let b:comment_leader = '!'"};
        {"FileType", "dosini", "let b:comment_leader = ';'"};
        {"FileType", "lua,haskell,cabalconfig", "let b:comment_leader = '--'"};
    };
}

nvim_create_augroups(autocmds)

opt.title = true
opt.background = 'light'
opt.mouse = 'a'
opt.termguicolors = true
opt.completeopt = { 'menuone', 'noselect' }
opt.smarttab = true
opt.showmode = false
opt.ruler = false
opt.clipboard = 'unnamedplus'
opt.ignorecase = true
opt.wildignorecase = true
opt.wildignore = { '*.o', '*.obj', '*.hi' }
opt.hidden = true
opt.autoread = true
opt.splitbelow = true
opt.splitright = true
opt.timeoutlen = 500
opt.scrolloff = 5
opt.sidescroll = 1
opt.sidescrolloff = 5
opt.linebreak = true
opt.expandtab = true
opt.tabstop = 8
opt.softtabstop = 4
opt.shiftwidth = 4
opt.relativenumber = true
opt.wildmode = { 'longest:full', 'full' }
opt.listchars = { tab = '> ', trail = '-', extends = '>', precedes = '<', nbsp = '+', eol = '$' }
opt.shortmess = 'atToOIc'
opt.whichwrap = 'b,s,<,>,[,]'

g.mapleader = ' '
g.netrw_sort_options = "i"
g.netrw_keepdir = 0
g.netrw_hide = 0
g.netrw_banner = 0
g.netrw_browse_split = 4
g.netrw_liststyle = 3
g.netrw_sizestyle = "H"


local function map(mode, lhs, rhs, opts)
    local options = {noremap = true}
    if opts then options = vim.tbl_extend('force', options, opts) end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map('n', '<Leader>bl', ':buffers<CR>')
--  Actually 'C-/' and not 'C-_'
map('n', '<C-_>', ':lua ToggleComment("n")<CR>')
map('v', '<C-_>', ':lua ToggleComment("v")<CR>')
map('i', '<C-_>', '<Esc>:lua ToggleComment("i")<CR>i')

map('i', '<C-Space>', 'compe#complete()', {silent = true, expr = true})
map('i', '<CR>', "compe#confirm('<CR>')", {silent = true, expr = true})
map('i', '<Esc>', "compe#close('<Esc>')", {silent = true, expr = true})
require('plugins')

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
local snap = require'snap'

-- normal mode mapping <Leader><Leader> for searching files in cwd 
snap.register.map('n', '<Leader><Leader>', snap.create(function ()
  return {
    producer = snap.get'consumer.fzf'(snap.get'producer.ripgrep.file'.hidden),
    select = snap.get'select.file'.select,
    multiselect = snap.get'select.file'.multiselect,
    views = {snap.get'preview.file'}
  }
end))

-- creates normal mode mapping <Leader>f for grepping files in cwd 
snap.register.map('n', '<Leader>f', snap.create(function ()
  return {
        producer = snap.get'consumer.limit'(10000, snap.get'producer.ripgrep.vimgrep'),
--     producer = snap.get'producer.ripgrep.vimgrep',
    select = snap.get'select.vimgrep'.select,
    multiselect = snap.get'select.vimgrep'.multiselect,
    views = {snap.get'preview.vimgrep'}
  }
end))

