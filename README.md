# Brain cellular automaton

https://en.wikipedia.org/wiki/Brian%27s_Brain

## Install dependencies

    (ql:quickload '(:websocket-driver :clack :cl-json :hunchentoot))

## Start

    sbcl --eval "(progn (asdf:operate 'asdf:load-op 'brain) (brain:start))"

## Create manifest file

    (asdf:operate 'asdf:load-op 'brain)
    (ql:write-asdf-manifest-file "quicklisp-manifest.txt")

## Build executable

    buildapp --output brain --manifest-file quicklisp-manifest.txt \
        --compress-core \
        --load-system brain \
        --eval '(defun main (args) (declare (ignore args)) (brain:start) (sb-impl::toplevel-repl nil))' \
        --entry main
