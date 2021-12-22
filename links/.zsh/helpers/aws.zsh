if [ -n "$commands[aws_completer]" ]; then
  autoload -Uz bashcompinit && bashcompinit
  complete -C aws_completer aws
fi
