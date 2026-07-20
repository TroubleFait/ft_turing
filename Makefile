SOURCES = main.ml

RESULT = ft_turing

_ := $(shell test -e OCamlMakefile || cp $(shell opam var lib)/ocaml-makefile/OCamlMakefile OCamlMakefile)

.PHONY: all g clean fclean re

all: native-code

g:
	$(MAKE) native-code OCAMLFLAGS="-g"

clean: mostlyclean
	find . \( -iname "*.cm*" -o -iname "*.o" \) -print -delete

fclean: clean
	rm -f OCamlMakefile
	rm $(RESULT)

re: fclean all

include OCamlMakefile
