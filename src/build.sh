if [ "$1" = 'force' ]; then
    unlink Main.native
    rm -rf _build
    rm joos-peephole
fi;

obelisk html Parser.mly > ../doc/Grammar.html
ocamlbuild -use-menhir -use-ocamlfind Main.native
cp -L Main.native joos-peephole
