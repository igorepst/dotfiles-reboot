#!/usr/bin/env bash

echo ${GREEN}'Installing vim plugins'${RESET}
echo
vim +PlugInstall +qall
