
all:
.PHONY: all


install-deps:
	opam update
	opam install 'camomile<2' dolog magic-mime ocamlfind ubase
.PHONY: install-deps


toc:
	sourcetoc 0pen.ml
.PHONY: toc
