if command -v aws_completer &> /dev/null; then
  autoload -Uz bashcompinit && bashcompinit
  complete -C aws_completer aws
fi
