export SUDO_EDITOR=emacs
((systemctl --user import-environment PATH && systemctl --user start emacs.service)&)
