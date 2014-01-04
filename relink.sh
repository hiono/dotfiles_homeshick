#!/bin/bash

# PARENT_DIR=$(cd $(dirname $0)/..;pwd)
SCRIPT_DIR=$(cd $(dirname $0);pwd)

[ -d $(pwd)/.git ] && git pull

DOT_FILES=(.addpath .aspell.conf .bash_aliases .bashrc .bash_logout .gitconfig .gitignore .profile .tmux.conf .emacs.d/init.el)

for file in ${DOT_FILES[@]}
do
    find $(dirname $HOME/$file) -name $(basename $HOME/$file) -xtype l -delete
    diff -w ${SCRIPT_DIR}/$file $HOME/$file 2>&1 > /dev/null
    if [ $? == 1 ]; then
	ln -s ${SCRIPT_DIR}/$file $HOME/$file.dot \
	    && echo "File exists: create '$file.dot'"
    else
	ln -snf ${SCRIPT_DIR}/$file $HOME/$file \
	    && echo "OK: create symbolic link '$file'"
    fi
done
