#!/usr/bin/env bash

printf '%sInstalling third party packages:%s\n\n' "${GREEN}" "${RESET}"

printf '%sInstalling Emacs:%s\n' "${GREEN}" "${RESET}"
"${DOTFILES_DIR}/packages/emacs/install.sh"

printf '%sInstalling Rofi:%s\n' "${GREEN}" "${RESET}"
"${DOTFILES_DIR}/packages/rofi/install.sh"
