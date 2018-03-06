obelisk html Parser.mly > ../doc/Grammar.html
ocamlbuild -use-menhir -use-ocamlfind Main.native
cp -L Main.native joos-peephole
