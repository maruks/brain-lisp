LISP ?= sbcl

build:
	$(LISP) --non-interactive \
	--load brain.asd \
	--eval "(ql:quickload 'trivial-dump-core)" \
	--eval "(ql:quickload 'brain)" \
	--eval "(trivial-dump-core:save-executable \"brain\" #'brain-server:main)"

test:
	$(LISP) --non-interactive \
	--load brain.asd \
	--eval "(ql:quickload 'brain/tests)" \
	--eval "(asdf:test-system 'brain)"

start:
	$(LISP) --non-interactive \
	--load brain.asd \
	--eval "(ql:quickload 'brain)" \
	--eval "(brain-server:main)"
