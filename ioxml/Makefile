OCAMLC_FLAGS=-g
OCAMLOPT_FLAGS=
OBJ=foo.cmi foo.cmo main.cmo
all: $(OBJ) main

foo.cmi: foo.mli
	ocamlc $(OCAMLC_FLAGS) -pp "camlp4o pa_ioXML.cmo" -I +camlp4 -c foo.mli
foo.cmo: foo.ml foo.mli
	ocamlc $(OCAMLC_FLAGS) -pp "camlp4o pa_ioXML.cmo" -I +camlp4 -c foo.ml
main.cmo: main.ml foo.cmi
	ocamlc -I +camlp4 -c main.ml
main: $(OBJ)
	ocamlc -I +camlp4 ioXML.cmo foo.cmo main.cmo -o main
clean:
	rm -vf $(OBJ) main
