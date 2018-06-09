ROOT_SRC := $(shell echo ./*.ml ./*.mll ./*.mly)
PASS_SRC := $(shell echo ./Pass/*.ml)

joos-peepholeopt: $(ROOT_SRC) $(PASS_SRC)
	ocamlbuild -use-menhir -use-ocamlfind Main.native
	cp -L Main.native joos-peephole

force_build: clean $(ROOT_SRC) $(PASS_SRC)
	ocamlbuild -use-menhir -use-ocamlfind Main.native
	cp -L Main.native joos-peephole

generate_grammar_doc: Parser.mly
	obelisk html Parser.mly > ../doc/Grammar.html

.PHONY:
install_tools:
	opam install menhir obelisk
	opam install ANSITerminal yojson

.PHONY:
clean:
	@echo "cleaning up ..."
	@rm -f Main.native
	@rm -rf _build
	@rm -f joos-peephole
