MODULES=main shuffle state bid score tricks game_graphics \
bid_screen play_screen rules_screen start_screen end_screen
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN = main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,ounit2,str,qcheck,graphics
default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

