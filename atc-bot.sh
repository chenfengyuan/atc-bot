#!/bin/sh
#(progn (ql:quickload "alexandria") (ql:quickload "inotify")(ql:quickload "trivial-timeout")(load "/home/cfy/gits/clp/count-time.lisp")(load "/home/cfy/gits/atc-bot/atc-bot.lisp"))
#(sb-ext:save-lisp-and-die "/tmp/atc-bot" :executable t)
echo -e '(in-package :cfy.atc-bot)\n(ignore-errors (main-func))(sb-ext:quit)'|./atc-bot $1
