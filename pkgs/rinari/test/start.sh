#!/bin/sh

# Runs specs in emacs by default. Use "start.sh $EMACSEN" to run
# against an arbitrary emacs implementation; e.g. emacs21 or xemacs

if [ $# -eq 0 ] ; then
    EMACS=emacs
else
    EMACS=$1
fi

$EMACS -q --eval "(load (expand-file-name \"./lisp/init.el\"))"
