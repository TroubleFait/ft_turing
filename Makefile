mkmodule = $(addprefix $(1), $(addsuffix .mli, $(2)) $(addsuffix .ml, $(2)))

PARSER_DIR = json_parser/
PARSER = read_file lexer parser

SOURCES = $(call mkmodule, $(PARSER_DIR), $(PARSER))
SOURCES += main.ml

RESULT = ft_turing

_ := $(shell test -e OCamlMakefile || cp $(shell opam var lib)/ocaml-makefile/OCamlMakefile OCamlMakefile)

.PHONY: all g ft_clean fclean re

all: native-code

g:
	@$(MAKE) --no-print-directory native-code OCAMLFLAGS="-g"

ft_clean: clean
	@find . \( -iname "*.cm*" -o -iname "*.o" \) -print -delete

fclean: ft_clean
	@rm -f OCamlMakefile
	@rm $(RESULT)

re: fclean all

include OCamlMakefile
