" vi:syntax=vim

" base16-vim (https://github.com/chriskempson/base16-vim)
" by Chris Kempson (http://chriskempson.com)
" Tango scheme by @Schnouki, based on the Tango Desktop Project

" This enables the coresponding base16-shell script to run so that
" :colorscheme works in terminals supported by base16-shell scripts
" User must set this variable in .vimrc
"   let g:base16_shell_path=base16-builder/output/shell/
if !has("gui_running")
  if exists("g:base16_shell_path")
    execute "silent !/bin/sh ".g:base16_shell_path."/base16-tango.sh"
  endif
endif

" GUI color definitions
let s:gui00        = "2e3436"
let g:base16_gui00 = "2e3436"
let s:gui01        = "8ae234"
let g:base16_gui01 = "8ae234"
let s:gui02        = "fce94f"
let g:base16_gui02 = "fce94f"
let s:gui03        = "555753"
let g:base16_gui03 = "555753"
let s:gui04        = "729fcf"
let g:base16_gui04 = "729fcf"
let s:gui05        = "d3d7cf"
let g:base16_gui05 = "d3d7cf"
let s:gui06        = "ad7fa8"
let g:base16_gui06 = "ad7fa8"
let s:gui07        = "eeeeec"
let g:base16_gui07 = "eeeeec"
let s:gui08        = "cc0000"
let g:base16_gui08 = "cc0000"
let s:gui09        = "ef2929"
let g:base16_gui09 = "ef2929"
let s:gui0A        = "c4a000"
let g:base16_gui0A = "c4a000"
let s:gui0B        = "4e9a06"
let g:base16_gui0B = "4e9a06"
let s:gui0C        = "06989a"
let g:base16_gui0C = "06989a"
let s:gui0D        = "3465a4"
let g:base16_gui0D = "3465a4"
let s:gui0E        = "75507b"
let g:base16_gui0E = "75507b"
let s:gui0F        = "34e2e2"
let g:base16_gui0F = "34e2e2"

" Terminal color definitions
let s:cterm00        = "00"
let g:base16_cterm00 = "00"
let s:cterm03        = "08"
let g:base16_cterm03 = "08"
let s:cterm05        = "07"
let g:base16_cterm05 = "07"
let s:cterm07        = "15"
let g:base16_cterm07 = "15"
let s:cterm08        = "01"
let g:base16_cterm08 = "01"
let s:cterm0A        = "03"
let g:base16_cterm0A = "03"
let s:cterm0B        = "02"
let g:base16_cterm0B = "02"
let s:cterm0C        = "06"
let g:base16_cterm0C = "06"
let s:cterm0D        = "04"
let g:base16_cterm0D = "04"
let s:cterm0E        = "05"
let g:base16_cterm0E = "05"
if exists("base16colorspace") && base16colorspace == "256"
  let s:cterm01        = "18"
  let g:base16_cterm01 = "18"
  let s:cterm02        = "19"
  let g:base16_cterm02 = "19"
  let s:cterm04        = "20"
  let g:base16_cterm04 = "20"
  let s:cterm06        = "21"
  let g:base16_cterm06 = "21"
  let s:cterm09        = "16"
  let g:base16_cterm09 = "16"
  let s:cterm0F        = "17"
  let g:base16_cterm0F = "17"
else
  let s:cterm01        = "10"
  let g:base16_cterm01 = "10"
  let s:cterm02        = "11"
  let g:base16_cterm02 = "11"
  let s:cterm04        = "12"
  let g:base16_cterm04 = "12"
  let s:cterm06        = "13"
  let g:base16_cterm06 = "13"
  let s:cterm09        = "09"
  let g:base16_cterm09 = "09"
  let s:cterm0F        = "14"
  let g:base16_cterm0F = "14"
endif

" Neovim terminal colours
if has("nvim")
  let g:terminal_color_0 =  "#2e3436"
  let g:terminal_color_1 =  "#cc0000"
  let g:terminal_color_2 =  "#4e9a06"
  let g:terminal_color_3 =  "#c4a000"
  let g:terminal_color_4 =  "#3465a4"
  let g:terminal_color_5 =  "#75507b"
  let g:terminal_color_6 =  "#06989a"
  let g:terminal_color_7 =  "#d3d7cf"
  let g:terminal_color_8 =  "#555753"
  let g:terminal_color_9 =  "#cc0000"
  let g:terminal_color_10 = "#4e9a06"
  let g:terminal_color_11 = "#c4a000"
  let g:terminal_color_12 = "#3465a4"
  let g:terminal_color_13 = "#75507b"
  let g:terminal_color_14 = "#06989a"
  let g:terminal_color_15 = "#eeeeec"
  let g:terminal_color_background = g:terminal_color_0
  let g:terminal_color_foreground = g:terminal_color_5
  if &background == "light"
    let g:terminal_color_background = g:terminal_color_7
    let g:terminal_color_foreground = g:terminal_color_2
  endif
elseif has("terminal")
  let g:terminal_ansi_colors = [
        \ "#2e3436",
        \ "#cc0000",
        \ "#4e9a06",
        \ "#c4a000",
        \ "#3465a4",
        \ "#75507b",
        \ "#06989a",
        \ "#d3d7cf",
        \ "#555753",
        \ "#cc0000",
        \ "#4e9a06",
        \ "#c4a000",
        \ "#3465a4",
        \ "#75507b",
        \ "#06989a",
        \ "#eeeeec",
        \ ]
endif

" Theme setup
hi clear
syntax reset
let g:colors_name = "base16-tango"

" Highlighting function
" Optional variables are attributes and guisp
function! g:Base16hi(group, guifg, guibg, ctermfg, ctermbg, ...)
  let l:attr = get(a:, 1, "")
  let l:guisp = get(a:, 2, "")

  " See :help highlight-guifg
  let l:gui_special_names = ["NONE", "bg", "background", "fg", "foreground"]

  if a:guifg != ""
    if index(l:gui_special_names, a:guifg) >= 0
      exec "hi " . a:group . " guifg=" . a:guifg
    else
      exec "hi " . a:group . " guifg=#" . a:guifg
    endif
  endif
  if a:guibg != ""
    if index(l:gui_special_names, a:guibg) >= 0
      exec "hi " . a:group . " guibg=" . a:guibg
    else
      exec "hi " . a:group . " guibg=#" . a:guibg
    endif
  endif
  if a:ctermfg != ""
    exec "hi " . a:group . " ctermfg=" . a:ctermfg
  endif
  if a:ctermbg != ""
    exec "hi " . a:group . " ctermbg=" . a:ctermbg
  endif
  if l:attr != ""
    exec "hi " . a:group . " gui=" . l:attr . " cterm=" . l:attr
  endif
  if l:guisp != ""
    if index(l:gui_special_names, l:guisp) >= 0
      exec "hi " . a:group . " guisp=" . l:guisp
    else
      exec "hi " . a:group . " guisp=#" . l:guisp
    endif
  endif
endfunction


fun <sid>hi(group, guifg, guibg, ctermfg, ctermbg, attr, guisp)
  call g:Base16hi(a:group, a:guifg, a:guibg, a:ctermfg, a:ctermbg, a:attr, a:guisp)
endfun

" Vim editor colors
call <sid>hi("Normal",        s:gui05, s:gui00, s:cterm05, s:cterm00, "", "")
call <sid>hi("Bold",          "", "", "", "", "bold", "")
call <sid>hi("Debug",         s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("Directory",     s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("Error",         s:gui00, s:gui08, s:cterm00, s:cterm08, "", "")
call <sid>hi("ErrorMsg",      s:gui08, s:gui00, s:cterm08, s:cterm00, "", "")
call <sid>hi("Exception",     s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("FoldColumn",    s:gui0C, s:gui01, s:cterm0C, s:cterm01, "", "")
call <sid>hi("Folded",        s:gui03, s:gui01, s:cterm03, s:cterm01, "", "")
call <sid>hi("IncSearch",     s:gui01, s:gui09, s:cterm01, s:cterm09, "none", "")
call <sid>hi("Italic",        "", "", "", "", "italic", "")
call <sid>hi("Macro",         s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("MatchParen",    "", s:gui03, "", s:cterm03,  "", "")
call <sid>hi("ModeMsg",       s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("MoreMsg",       s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("Question",      s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("Search",        s:gui01, s:gui0A, s:cterm01, s:cterm0A,  "", "")
call <sid>hi("Substitute",    s:gui01, s:gui0A, s:cterm01, s:cterm0A, "none", "")
call <sid>hi("SpecialKey",    s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("TooLong",       s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("Underlined",    s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("Visual",        "", s:gui02, "", s:cterm02, "", "")
call <sid>hi("VisualNOS",     s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("WarningMsg",    s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("WildMenu",      s:gui00, s:gui05, s:cterm00, s:cterm05, "", "")
call <sid>hi("Title",         s:gui0D, "", s:cterm0D, "", "none", "")
call <sid>hi("Conceal",       s:gui0D, s:gui00, s:cterm0D, s:cterm00, "", "")
call <sid>hi("Cursor",        s:gui00, s:gui05, s:cterm00, s:cterm05, "inverse", "")
call <sid>hi("NonText",       s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("Whitespace",    s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("LineNr",        s:gui03, s:gui00, s:cterm03, s:cterm00, "", "")
call <sid>hi("SignColumn",    s:gui03, s:gui00, s:cterm03, s:cterm00, "", "")
call <sid>hi("StatusLine",    s:gui04, s:gui01, s:cterm04, s:cterm01, "none", "")
call <sid>hi("StatusLineNC",  s:gui03, s:gui01, s:cterm03, s:cterm01, "none", "")
call <sid>hi("VertSplit",     s:gui01, s:gui00, s:cterm01, s:cterm00, "none", "")
call <sid>hi("ColorColumn",   "", s:gui01, "", s:cterm01, "none", "")
call <sid>hi("CursorColumn",  "", s:gui01, "", s:cterm01, "none", "")
call <sid>hi("CursorLine",    "", s:gui01, "", s:cterm01, "none", "")
call <sid>hi("CursorLineNr",  s:gui04, s:gui01, s:cterm04, s:cterm01, "bold", "")
call <sid>hi("QuickFixLine",  "", s:gui01, "", s:cterm01, "none", "")
call <sid>hi("PMenu",         s:gui05, s:gui02, s:cterm05, s:cterm02, "none", "")
call <sid>hi("PMenuSel",      s:gui01, s:gui05, s:cterm01, s:cterm05, "", "")
call <sid>hi("TabLine",       s:gui03, s:gui01, s:cterm03, s:cterm01, "none", "")
call <sid>hi("TabLineFill",   s:gui03, s:gui01, s:cterm03, s:cterm01, "none", "")
call <sid>hi("TabLineSel",    s:gui0B, s:gui01, s:cterm0B, s:cterm01, "none", "")

" Standard syntax highlighting
call <sid>hi("Boolean",      s:gui09, "", s:cterm09, "", "", "")
call <sid>hi("Character",    s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("Comment",      s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("Conditional",  s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("Constant",     s:gui09, "", s:cterm09, "", "", "")
call <sid>hi("Define",       s:gui0E, "", s:cterm0E, "", "none", "")
call <sid>hi("Delimiter",    s:gui0F, "", s:cterm0F, "", "", "")
call <sid>hi("Float",        s:gui09, "", s:cterm09, "", "", "")
call <sid>hi("Function",     s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("Identifier",   s:gui08, "", s:cterm08, "", "none", "")
call <sid>hi("Include",      s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("Keyword",      s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("Label",        s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("Number",       s:gui09, "", s:cterm09, "", "", "")
call <sid>hi("Operator",     s:gui0F, "", s:cterm0F, "", "none", "")
call <sid>hi("PreProc",      s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("Repeat",       s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("Special",      s:gui0C, "", s:cterm0C, "", "", "")
call <sid>hi("SpecialChar",  s:gui0F, "", s:cterm0F, "", "", "")
call <sid>hi("Statement",    s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("StorageClass", s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("String",       s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("Structure",    s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("Tag",          s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("Todo",         s:gui0A, s:gui01, s:cterm0A, s:cterm01, "", "")
call <sid>hi("Type",         s:gui0A, "", s:cterm0A, "", "none", "")
call <sid>hi("Typedef",      s:gui0A, "", s:cterm0A, "", "", "")

" Standard highlights to be used by plugins
call <sid>hi("GitAddSign",           s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("GitChangeSign",        s:gui04, "", s:cterm04, "", "", "")
call <sid>hi("GitDeleteSign",        s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("GitChangeDeleteSign",  s:gui04, "", s:cterm04, "", "", "")

call <sid>hi("ErrorSign",    s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("WarningSign",  s:gui09, "", s:cterm09, "", "", "")
call <sid>hi("InfoSign",     s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("HintSign",     s:gui0C, "", s:cterm0C, "", "", "")

call <sid>hi("ErrorFloat",   s:gui08, s:gui02, s:cterm08, s:cterm02, "", "")
call <sid>hi("WarningFloat", s:gui09, s:gui02, s:cterm09, s:cterm02, "", "")
call <sid>hi("InfoFloat",    s:gui0D, s:gui02, s:cterm0D, s:cterm02, "", "")
call <sid>hi("HintFloat",    s:gui0C, s:gui02, s:cterm0C, s:cterm02, "", "")

call <sid>hi("ErrorHighlight",   "", "", s:cterm00, s:cterm08, "underline", s:gui08)
call <sid>hi("WarningHighlight", "", "", s:cterm00, s:cterm09, "underline", s:gui09)
call <sid>hi("InfoHighlight",    "", "", s:cterm00, s:cterm0D, "underline", s:gui0D)
call <sid>hi("HintHighlight",    "", "", s:cterm00, s:cterm0C, "underline", s:gui0C)

call <sid>hi("SpellBad",     "", "", s:cterm00, s:cterm08, "undercurl", s:gui08)
call <sid>hi("SpellLocal",   "", "", s:cterm00, s:cterm0C, "undercurl", s:gui0C)
call <sid>hi("SpellCap",     "", "", s:cterm00, s:cterm0D, "undercurl", s:gui0D)
call <sid>hi("SpellRare",    "", "", s:cterm00, s:cterm0E, "undercurl", s:gui0E)

call <sid>hi("ReferenceText",   s:gui01, s:gui0A, s:cterm01, s:cterm0A, "", "")
call <sid>hi("ReferenceRead",   s:gui01, s:gui0B, s:cterm01, s:cterm0B, "", "")
call <sid>hi("ReferenceWrite",  s:gui01, s:gui08, s:cterm01, s:cterm08, "", "")

" C highlighting
call <sid>hi("cOperator",   s:gui0C, "", s:cterm0C, "", "", "")
call <sid>hi("cPreCondit",  s:gui0E, "", s:cterm0E, "", "", "")

" C# highlighting
call <sid>hi("csClass",                 s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("csAttribute",             s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("csModifier",              s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("csType",                  s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("csUnspecifiedStatement",  s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("csContextualStatement",   s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("csNewDecleration",        s:gui08, "", s:cterm08, "", "", "")

" Clap highlighting
hi default link ClapInput             ColorColumn
hi default link ClapSpinner           ColorColumn
hi default link ClapDisplay           Default
hi default link ClapPreview           ColorColumn
hi default link ClapCurrentSelection  CursorLine
hi default link ClapNoMatchesFound    ErrorFloat

" Coc highlighting
hi default link CocErrorSign         ErrorSign
hi default link CocWarningSign       WarningSign
hi default link CocInfoSign          InfoSign
hi default link CocHintSign          HintSign

hi default link CocErrorFloat        ErrorFloat
hi default link CocWarningFloat      WarningFloat
hi default link CocInfoFloat         InfoFloat
hi default link CocHintFloat         HintFloat

hi default link CocErrorHighlight    ErrorHighlight
hi default link CocWarningHighlight  WarningHighlight
hi default link CocInfoHighlight     InfoHighlight
hi default link CocHintHighlight     HintHighlight

hi default link CocSem_angle             Keyword
hi default link CocSem_annotation        Keyword
hi default link CocSem_attribute         Type
hi default link CocSem_bitwise           Keyword
hi default link CocSem_boolean           Boolean
hi default link CocSem_brace             Normal
hi default link CocSem_bracket           Normal
hi default link CocSem_builtinAttribute  Type
hi default link CocSem_builtinType       Type
hi default link CocSem_character         String
hi default link CocSem_class             Structure
hi default link CocSem_colon             Normal
hi default link CocSem_comma             Normal
hi default link CocSem_comment           Comment
hi default link CocSem_comparison        Keyword
hi default link CocSem_concept           Keyword
hi default link CocSem_constParameter    Identifier
hi default link CocSem_dependent         Keyword
hi default link CocSem_dot               Keyword
hi default link CocSem_enum              Structure
hi default link CocSem_enumMember        Constant
hi default link CocSem_escapeSequence    Type
hi default link CocSem_event             Identifier
hi default link CocSem_formatSpecifier   Type
hi default link CocSem_function          Function
hi default link CocSem_interface         Type
hi default link CocSem_keyword           Keyword
hi default link CocSem_label             Keyword
hi default link CocSem_logical           Keyword
hi default link CocSem_macro             Macro
hi default link CocSem_method            Function
hi default link CocSem_modifier          Keyword
hi default link CocSem_namespace         Identifier
hi default link CocSem_number            Number
hi default link CocSem_operator          Operator
hi default link CocSem_parameter         Identifier
hi default link CocSem_parenthesis       Normal
hi default link CocSem_property          Identifier
hi default link CocSem_punctuation       Keyword
hi default link CocSem_regexp            Type
hi default link CocSem_selfKeyword       Constant
hi default link CocSem_semicolon         Normal
hi default link CocSem_string            String
hi default link CocSem_struct            Structure
hi default link CocSem_type              Type
hi default link CocSem_typeAlias         Type
hi default link CocSem_typeParameter     Type
hi default link CocSem_unknown           Normal
hi default link CocSem_variable          Identifier

call <sid>hi("CocHighlightRead",   s:gui0B, s:gui01,  s:cterm0B, s:cterm01, "", "")
call <sid>hi("CocHighlightText",   s:gui0A, s:gui01,  s:cterm0A, s:cterm01, "", "")
call <sid>hi("CocHighlightWrite",  s:gui08, s:gui01,  s:cterm08, s:cterm01, "", "")
call <sid>hi("CocListMode",        s:gui01, s:gui0B,  s:cterm01, s:cterm0B, "bold", "")
call <sid>hi("CocListPath",        s:gui01, s:gui0B,  s:cterm01, s:cterm0B, "", "")
call <sid>hi("CocSessionsName",    s:gui05, "", s:cterm05, "", "", "")

" CSS highlighting
call <sid>hi("cssBraces",      s:gui05, "", s:cterm05, "", "", "")
call <sid>hi("cssClassName",   s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("cssColor",       s:gui0C, "", s:cterm0C, "", "", "")

" Diff highlighting
call <sid>hi("DiffAdd",      s:gui0B, s:gui02,  s:cterm0B, s:cterm02, "", "")
call <sid>hi("DiffChange",   s:gui05, s:gui02,  s:cterm05, s:cterm02, "", "")
call <sid>hi("DiffDelete",   s:gui08, s:gui02,  s:cterm08, s:cterm02, "", "")
call <sid>hi("DiffText",     s:gui0D, s:gui02,  s:cterm0D, s:cterm02, "", "")
call <sid>hi("DiffAdded",    s:gui0B, s:gui00,  s:cterm0B, s:cterm00, "", "")
call <sid>hi("DiffFile",     s:gui08, s:gui00,  s:cterm08, s:cterm00, "", "")
call <sid>hi("DiffNewFile",  s:gui0B, s:gui00,  s:cterm0B, s:cterm00, "", "")
call <sid>hi("DiffLine",     s:gui0D, s:gui00,  s:cterm0D, s:cterm00, "", "")
call <sid>hi("DiffRemoved",  s:gui08, s:gui00,  s:cterm08, s:cterm00, "", "")

" Git highlighting
call <sid>hi("gitcommitOverflow",       s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("gitcommitSummary",        s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("gitcommitComment",        s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("gitcommitUntracked",      s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("gitcommitDiscarded",      s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("gitcommitSelected",       s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("gitcommitHeader",         s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("gitcommitSelectedType",   s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("gitcommitUnmergedType",   s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("gitcommitDiscardedType",  s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("gitcommitBranch",         s:gui09, "", s:cterm09, "", "bold", "")
call <sid>hi("gitcommitUntrackedFile",  s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("gitcommitUnmergedFile",   s:gui08, "", s:cterm08, "", "bold", "")
call <sid>hi("gitcommitDiscardedFile",  s:gui08, "", s:cterm08, "", "bold", "")
call <sid>hi("gitcommitSelectedFile",   s:gui0B, "", s:cterm0B, "", "bold", "")

" GitGutter highlighting
hi default link GitGutterAdd            GitAddSign
hi default link GitGutterChange         GitChangeSign  
hi default link GitGutterDelete         GitDeleteSign
hi default link GitGutterChangeDelete   GitChangeDeleteSign

" HTML highlighting
call <sid>hi("htmlBold",    s:gui05, "", s:cterm0A, "", "bold", "")
call <sid>hi("htmlItalic",  s:gui05, "", s:cterm0E, "", "italic", "")
call <sid>hi("htmlEndTag",  s:gui05, "", s:cterm05, "", "", "")
call <sid>hi("htmlTag",     s:gui05, "", s:cterm05, "", "", "")

" JavaScript highlighting
call <sid>hi("javaScript",        s:gui05, "", s:cterm05, "", "", "")
call <sid>hi("javaScriptBraces",  s:gui05, "", s:cterm05, "", "", "")
call <sid>hi("javaScriptNumber",  s:gui09, "", s:cterm09, "", "", "")
" pangloss/vim-javascript highlighting
call <sid>hi("jsOperator",          s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("jsStatement",         s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("jsReturn",            s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("jsThis",              s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("jsClassDefinition",   s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("jsFunction",          s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("jsFuncName",          s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("jsFuncCall",          s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("jsClassFuncName",     s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("jsClassMethodType",   s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("jsRegexpString",      s:gui0C, "", s:cterm0C, "", "", "")
call <sid>hi("jsGlobalObjects",     s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("jsGlobalNodeObjects", s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("jsExceptions",        s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("jsBuiltins",          s:gui0A, "", s:cterm0A, "", "", "")

" Mail highlighting
call <sid>hi("mailQuoted1",  s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("mailQuoted2",  s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("mailQuoted3",  s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("mailQuoted4",  s:gui0C, "", s:cterm0C, "", "", "")
call <sid>hi("mailQuoted5",  s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("mailQuoted6",  s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("mailURL",      s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("mailEmail",    s:gui0D, "", s:cterm0D, "", "", "")

" Markdown highlighting
call <sid>hi("markdownCode",              s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("markdownError",             s:gui05, s:gui00, s:cterm05, s:cterm00, "", "")
call <sid>hi("markdownCodeBlock",         s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("markdownHeadingDelimiter",  s:gui0D, "", s:cterm0D, "", "", "")

" NERDTree highlighting
call <sid>hi("NERDTreeDirSlash",  s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("NERDTreeExecFile",  s:gui05, "", s:cterm05, "", "", "")

" PHP highlighting
call <sid>hi("phpMemberSelector",  s:gui05, "", s:cterm05, "", "", "")
call <sid>hi("phpComparison",      s:gui05, "", s:cterm05, "", "", "")
call <sid>hi("phpParent",          s:gui05, "", s:cterm05, "", "", "")
call <sid>hi("phpMethodsVar",      s:gui0C, "", s:cterm0C, "", "", "")

" Python highlighting
call <sid>hi("pythonOperator",  s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("pythonRepeat",    s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("pythonInclude",   s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("pythonStatement", s:gui0E, "", s:cterm0E, "", "", "")

" Ruby highlighting
call <sid>hi("rubyAttribute",               s:gui0D, "", s:cterm0D, "", "", "")
call <sid>hi("rubyConstant",                s:gui0A, "", s:cterm0A, "", "", "")
call <sid>hi("rubyInterpolationDelimiter",  s:gui0F, "", s:cterm0F, "", "", "")
call <sid>hi("rubyRegexp",                  s:gui0C, "", s:cterm0C, "", "", "")
call <sid>hi("rubySymbol",                  s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("rubyStringDelimiter",         s:gui0B, "", s:cterm0B, "", "", "")

" SASS highlighting
call <sid>hi("sassidChar",     s:gui08, "", s:cterm08, "", "", "")
call <sid>hi("sassClassChar",  s:gui09, "", s:cterm09, "", "", "")
call <sid>hi("sassInclude",    s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("sassMixing",     s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("sassMixinName",  s:gui0D, "", s:cterm0D, "", "", "")

" Signify highlighting
hi default link SignifySignAdd    GitAddSign
hi default link SignifySignChange GitChangeSign
hi default link SignifySignDelete GitDeleteSign

" Startify highlighting
call <sid>hi("StartifyBracket",  s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("StartifyFile",     s:gui07, "", s:cterm07, "", "", "")
call <sid>hi("StartifyFooter",   s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("StartifyHeader",   s:gui0B, "", s:cterm0B, "", "", "")
call <sid>hi("StartifyNumber",   s:gui09, "", s:cterm09, "", "", "")
call <sid>hi("StartifyPath",     s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("StartifySection",  s:gui0E, "", s:cterm0E, "", "", "")
call <sid>hi("StartifySelect",   s:gui0C, "", s:cterm0C, "", "", "")
call <sid>hi("StartifySlash",    s:gui03, "", s:cterm03, "", "", "")
call <sid>hi("StartifySpecial",  s:gui03, "", s:cterm03, "", "", "")

" Treesitter-refactor highlighting
if has("nvim")
  call <sid>hi("TSDefinition",       "", s:gui03, "", s:cterm03, "", "")
  call <sid>hi("TSDefinitionUsage",  "", s:gui02, "", s:cterm02, "none", "")
endif

" LSP highlighting
if has("nvim")
  hi default link LspDiagnosticsSignError    ErrorSign
  hi default link LspDiagnosticsSignWarning  WarningSign
  hi default link LspDiagnosticsSignInfo     InfoSign
  hi default link LspDiagnosticsSignHint     HintSign

  hi default link LspDiagnosticsVirtualTextError    ErrorSign
  hi default link LspDiagnosticsVirtualTextWarning  WarningSign
  hi default link LspDiagnosticsVirtualTextInfo     InfoSign
  hi default link LspDiagnosticsVirtualTextHint     HintSign

  hi default link LspDiagnosticsFloatingError    ErrorFloat
  hi default link LspDiagnosticsFloatingWarning  WarningFloat
  hi default link LspDiagnosticsFloatingInfo     InfoFloat
  hi default link LspDiagnosticsFloatingHint     HintFloat

  hi default link LspDiagnosticsUnderlineError    ErrorHighlight
  hi default link LspDiagnosticsUnderlineWarning  WarningHighlight
  hi default link LspDiagnosticsUnderlineInfo     InfoHighlight
  hi default link LspDiagnosticsUnderlineHint     HintHighlight

  hi default link LsoReferenceText   ReferenceText
  hi default link LsoReferenceRead   ReferenceRead
  hi default link LsoReferenceWrite  ReferenceWrite
endif

" Java highlighting
call <sid>hi("javaOperator",     s:gui0D, "", s:cterm0D, "", "", "")

" Remove functions
delf <sid>hi

" Remove color variables
unlet s:gui00 s:gui01 s:gui02 s:gui03  s:gui04  s:gui05  s:gui06  s:gui07  s:gui08  s:gui09 s:gui0A  s:gui0B  s:gui0C  s:gui0D  s:gui0E  s:gui0F
unlet s:cterm00 s:cterm01 s:cterm02 s:cterm03 s:cterm04 s:cterm05 s:cterm06 s:cterm07 s:cterm08 s:cterm09 s:cterm0A s:cterm0B s:cterm0C s:cterm0D s:cterm0E s:cterm0F
