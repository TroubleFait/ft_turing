SOURCES =	execution/turing_machine.ml
SOURCES +=	main.ml


RESULT = ft_turing

_ := $(shell test -e OCamlMakefile || cp $(shell opam var lib)/ocaml-makefile/OCamlMakefile OCamlMakefile)

.PHONY: all g ft_clean fclean re

all:
	@$(MAKE) --no-print-directory native-code OCAMLFLAGS="-warn-error A"

g:
	@$(MAKE) --no-print-directory native-code OCAMLFLAGS="-warn-error A -g"

ft_clean: cleanup
	@find . \( -iname "*.cm*" -o -iname "*.o" \) -print -delete

fclean: ft_clean
	@rm $(RESULT)

re: fclean all

include OCamlMakefile
