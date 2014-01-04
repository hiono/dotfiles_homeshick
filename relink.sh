#!/bin/bash

cd $HOME/dotfiles
git pull

DOT_FILES=(.addpath .aspell.conf .bash_aliases .bashrc .gitconfig .gitignore .profile .tmux.conf .emacs.d/init.el)

for file in ${DOT_FILES[@]}
do
    if [ -a $HOME/$file ]; then
	ln -s $HOME/dotfiles/$file $HOME/$file.dot
	echo "ファイルが存在しますから.dotファイルつくるよ: $file"
    else
	ln -s $HOME/dotfiles/$file $HOME/$file
	echo "シンボリックリンクを貼りました: $file"
    fi
done
