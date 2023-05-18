
all:
.PHONY: all


install-deps:
	opam update
	opam install 'camomile<2' dolog magic-mime ocamlfind ubase
.PHONY: install-deps


pre-commit-hook: help.txt
.PHONY: pre-commit-hook


help.txt: 0pen.ml
	. ${HOME}/.opam/opam-init/init.sh && ./0pen.ml --help > $@
	git add $@


toc:
	sourcetoc 0pen.ml
.PHONY: toc
