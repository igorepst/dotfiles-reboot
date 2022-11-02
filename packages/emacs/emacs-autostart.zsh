export SUDO_EDITOR=emacs
((systemctl --user import-environment PATH TIME_STYLE SSH_AUTH_SOCK SSH_AGENT_PID 2>/dev/null && systemctl --user start emacs.service)&)
