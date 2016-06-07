quicklisp: quicklisp.lisp quicklisp.lisp.asc
	gpg --verify quicklisp.lisp.asc quicklisp.lisp
	@echo
	@echo "When SBCL loads, run the following:"
	@echo "  (quicklisp-quickstart:install)"
	@echo
	sbcl --load quicklisp.lisp
	$(MAKE) clean-quicklisp

quicklisp.lisp:
	@curl -O https://beta.quicklisp.org/quicklisp.lisp

quicklisp.lisp.asc:
	@curl -O https://beta.quicklisp.org/quicklisp.lisp.asc

clean-quicklisp:
	rm quicklisp.lisp.asc quicklisp.lisp

clean: clean-quicklisp

.PHONEY: quicklisp
