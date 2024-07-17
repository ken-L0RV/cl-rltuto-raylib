LISP ?= sbcl

#all: test
all: build

run:
	$(LISP) --noinform \
		--non-interactive \
		--load run.lisp

build:
	$(LISP) --non-interactive \
		--load cl-rltuto-raylib.asd \
		--eval '(ql:quickload :cl-rltuto-raylib)' \
		--eval '(asdf:make :cl-rltuto-raylib)' \

#test:
#	$(LISP) --non-interactive \
#		--load run-test.lisp
