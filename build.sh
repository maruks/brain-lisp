#!/usr/bin/env bash

buildapp --output brain --manifest-file quicklisp-manifest.txt \
    --load-system brain \
    --eval '(defun main (args) (declare (ignore args)) (brain:start) (sb-impl::toplevel-repl nil))' \
    --entry main
