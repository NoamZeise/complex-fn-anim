LISP ?= sbcl

build:
	$(LISP) --load canim.asd \
		--eval '(ql:quickload :canim)' \
			--eval '(asdf:make :canim)' \
			--eval '(quit)'
clean:
	rm -r build
