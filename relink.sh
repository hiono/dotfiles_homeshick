#!/bin/bash

# options
usage_exit() {
    echo "Usage: $0 [-f] [-h]" 1>&2
    echo "    -f: force override"
    echo "    -h: help"
    exit 1
}
FORCE=0
while getopts fh OPT
do
    case $OPT in
	f)  FORCE=1
	    ;;
	h)  usage_exit
	    ;;
\?) usage_exit
;;
    esac
done

shift $((OPTIND - 1))

# PARENT_DIR=$(cd $(dirname $0)/..;pwd)
SCRIPT_DIR=$(cd $(dirname $0);pwd)

[ -d $(pwd)/.git ] && git pull

DOT_FILES=(.addpath .aspell.conf .bash_aliases .bashrc .bash_logout .gitconfig .gitignore .profile .tmux.conf .emacs.d/init.el)

for file in ${DOT_FILES[@]}
do
    DEST=$HOME/$file
    SRC=${SCRIPT_DIR}/$file
    find $(dirname ${DEST}) -name $(basename ${DEST}) -xtype l -delete
    [ $? == 1 ] && continue
    if [ $FORCE == 1 ]; then
	mkdir -p $(dirname ${DEST})
	if [ -f ${DEST} ] && [ ! -h ${DEST} ]; then
	    BACKUP=$(dirname ${DEST})/.dotbackup
	    mkdir -p ${BACKUP}
	    mv ${DEST} ${BACKUP}/$file.$(date +"%y-%m-%d-%T")
	fi
    fi
    diff -w ${SRC} ${DEST} 2>&1 > /dev/null
    if [ $? == 1 ] && [ $FORCE == 0 ]; then
	ln -s ${SRC} ${DEST}.dot && echo "File exists: create '$file.dot'"
    else
	ln -snf ${SRC} ${DEST}	 && echo "OK: create symbolic link '$file'"
    fi
done
