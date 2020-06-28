# Add previous command to pet
function pprev() {
    PREV=$(fc -lrn | head -n 1)
    sh -c "pet new -t `printf %q "$PREV"`"
}

# Search snippets and output on the shell
function pet-select() {
    BUFFER=$(pet search --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle redisplay
}
zle -N pet-select
#Alt+p
bindkey 'ð' pet-select
