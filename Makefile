LISP ?= sbcl
EVAL ?= "--eval"
LOAD ?= "--load"

.PHONY: clean

build:
	@$(LISP) $(EVAL) '(ql:quickload :alexandria)'  \
                 $(EVAL) '(ql:quickload :unix-opts)'   \
                 $(EVAL) '(ql:quickload :parse-float)' \
                 $(LOAD) tagmov.asd                    \
                 $(EVAL) '(asdf:make :tagmov)'         \
                 $(EVAL) '(quit)'

clean:
	@rm -f tagmov