function _ig_update_emacs() {
    echo 'Updating Emacs packages'
    if emacs --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"lisp\" user-emacs-directory))(require 'ig-packages)(straight-pull-all))"; then
	systemctl --user restart emacs
    fi
}
_ig_update_funcs+=("_ig_update_emacs")

alias emacs='emacs -mm'
alias e='emacsclient -nc'
alias et='emacsclient -t'
