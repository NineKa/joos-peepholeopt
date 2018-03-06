obelisk html Parser.mly > Grammar.html
ocamlbuild -use-menhir -use-ocamlfind Main.native
cp -L Main.native joos-peephole
