mkmodule = $(addprefix $(1), $(addsuffix .mli, $(2)) $(addsuffix .ml, $(2)))

PARSER_DIR = json_parser/
PARSER = read_file lexer parser
EXECUTION_DIR = execution/
EXECUTION = rules tape turing_machine

SOURCES = $(call mkmodule, ./, utils)
SOURCES += $(call mkmodule, $(PARSER_DIR), $(PARSER))
SOURCES += $(call mkmodule, $(EXECUTION_DIR), $(EXECUTION))
SOURCES += main.ml

RESULT = ft_turing

_ := $(shell test -e OCamlMakefile || cp $(shell opam var lib)/ocaml-makefile/OCamlMakefile OCamlMakefile)

.PHONY: all g ft_clean fclean re

all:
	@$(MAKE) --no-print-directory native-code OCAMLFLAGS="-warn-error A"

g:
	@$(MAKE) --no-print-directory native-code OCAMLFLAGS="-warn-error A -g"

ft_clean: clean
	@find . \( -iname "*.cm*" -o -iname "*.o" \) -print -delete

fclean: ft_clean
	@rm -rf OCamlMakefile
	@rm $(RESULT)

re: fclean all

include OCamlMakefile
