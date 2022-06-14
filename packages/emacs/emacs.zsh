function _ig_update_emacs() {
    echo 'Updating Emacs packages'
    if emacs --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"lisp\" user-emacs-directory))(require 'ig-packages)(straight-pull-all)(straight-prune-build))"; then
	systemctl --user restart emacs
    fi
}
_ig_update_funcs+=("_ig_update_emacs")

macsman() {
    emacsclient -nc -e "(progn (require 'man)(select-frame-set-input-focus (selected-frame))(let ((Man-notify-method 'bully)) (man \"$*\")))" >/dev/null
}
compdef _man macsman
alias man=macsman

alias emacs='emacs -mm'
alias e='emacsclient -nc'
alias et='emacsclient -t'

export LSP_USE_PLISTS=true
