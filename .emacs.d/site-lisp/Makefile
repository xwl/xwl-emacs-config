SUBDIRS=config debian dictionary-el qterm ruby wget-el wubi

BATCH=emacs -batch -q -eval                             \
 "(progn (add-to-list (quote load-path) \".\"))"

LISPS=$(filter-out xwl-install.el template.el, $(wildcard *.el))

all: #byte-compile xwl-install.el
	@echo "byte-compile disabled"

xwl-install.el: $(LISPS)
	$(BATCH) --eval "(require 'autoload)" \
		--eval '(find-file "xwl-install.el")'  \
		--eval '(erase-buffer)' \
		--eval '(mapc (lambda (x) (generate-file-autoloads (symbol-name x))) (quote ($(LISPS))))' \
		--eval '(insert "\n(provide (quote xwl-install))\n")' \
		--eval '(save-buffer)'

byte-compile:
	(echo "=== make in current === "; make -f Makefile.common)
	for i in $(SUBDIRS); do \
	(echo "=== make in $${i} ==="; cd $${i}; make; cd -); \
	done

clean:
	(echo "=== make clean in current === "; make -f Makefile.common clean)
	for i in $(SUBDIRS); do \
	(echo "=== make clean in $${i} ==="; cd $${i}; make clean; cd -); \
	done
