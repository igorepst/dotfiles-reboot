function _ig_update_emacs() {
    printf '\033[0;32m%s\033[0m\n' 'Updating Emacs packages'
    if emacs --batch -l ~/.config/emacs/early-init.el -l ~/.config/emacs/lisp/autoload/ig-autoload-utils.el -f ig-update-packages
    then
	systemctl --user restart emacs
    fi
}
_ig_update_funcs+=("_ig_update_emacs")

macsman() {
    emacsclient -nc -e "(progn (require 'man)(select-frame-set-input-focus (selected-frame))(let ((Man-notify-method 'bully)) (man \"$*\")))" >/dev/null
}
compdef _man macsman
alias man=macsman

alias e='emacsclient -nc'
alias et='emacsclient -t'

export LSP_USE_PLISTS=true
