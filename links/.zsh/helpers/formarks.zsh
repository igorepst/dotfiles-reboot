#Alt+g
export FZF_MARKS_JUMP='g'
export PATHMARKS_FILE=~/.zsh/volatile/pathmarks
source ~/.zsh/plugins/formarks/formarks.plugin.zsh

function pathmarks-colorize-override() {
    local field='\(\S\+\s*\)'
    local esc=$(printf '\033')
    local N="${esc}[0m"
    local R="${esc}[31m"
    sed "s#^${field}${field}${field}${field}#\1$R\2$N\3\4#"
}

functions[wfxr::pathmarks-colorize]=$functions[pathmarks-colorize-override]
