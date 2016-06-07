PROJ = shop2
QL_DIR = ~/quicklisp
QL_LOCAL_DIR = $(QL_DIR)/local-projects
LOCAL_INSTALL = $(QL_LOCAL_DIR)/$(PROJ)

quicklisp: quicklisp.lisp quicklisp.lisp.asc $(LOCAL_INSTALL)
	gpg --verify quicklisp.lisp.asc quicklisp.lisp
	@echo
	@echo "When SBCL loads, run the following:"
	@echo "  (quicklisp-quickstart:install)"
	@echo
	-sbcl --load quicklisp.lisp
	@$(MAKE) clean-quicklisp

quicklisp.lisp:
	@curl -O https://beta.quicklisp.org/quicklisp.lisp

quicklisp.lisp.asc:
	@curl -O https://beta.quicklisp.org/quicklisp.lisp.asc

$(LOCAL_INSTALL): $(QL_DIR) $(QL_LOCAL_DIR)
	@ln -s `pwd` $(LOCAL_INSTALL)
	@echo
	@echo "Now, from SBCL, run the following:"
	@echo "  (ql:quickload \"shop2\")"

$(QL_DIR):
	mkdir $(QL_DIR)

$(QL_LOCAL_DIR):
	@mkdir =p $(QL_LOCAL_DIR)

clean-quicklisp:
	rm quicklisp.lisp.asc quicklisp.lisp

clean: clean-quicklisp

.PHONEY: quicklisp
