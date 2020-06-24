if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source ~/.zsh/p10k.zsh
source ~/.zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme

source ~/.zsh/aliases.zsh
source ~/.zsh/fzf.zsh
