#!/usr/bin/env bash

function doWork() {
    echo ${GREEN}'Installing vim plugins'${RESET}
    echo
    vim +PlugInstall +qall
}

doWork
