LISP ?= sbcl

.PHONY: clean

build:
	@$(LISP) --eval '(ql:quickload :unix-opts)' \
                 --eval '(ql:quickload :str)'	    \
                 --load tagmov.asd                  \
                 --eval '(asdf:make :tagmov)'       \
                 --eval '(quit)'

clean:
	@rm -f tagmov