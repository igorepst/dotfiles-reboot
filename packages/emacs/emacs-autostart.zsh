export SUDO_EDITOR=emacs
((systemctl --user import-environment PATH && systemctl --user import-environment TIME_STYLE && systemctl --user start emacs.service)&)
