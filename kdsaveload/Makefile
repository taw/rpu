OCAMLC_FLAGS=-g
OCAMLOPT_FLAGS=
OBJ=scene.cmi raytracer.cmo raytracer.cmx
XOBJ=benchmark_ov.cmi benchmark_ov.cmx benchmark_xml.cmi benchmark_xml.cmo raytracer.cmi xml_scene.cmi xml_scene.cmo \
     benchmark_ov.o raytracer.o
all: $(OBJ) raytracer.opt benchmark_ov.opt benchmark_xml

raytracer.opt: raytracer.cmx
	ocamlopt $(OCAMLOPT_FLAGS) -o $@ $^

xml_scene.cmi: xml_scene.mli
	ocamlc $(OCAMLC_FLAGS) -pp "camlp4o pa_ioXML.cmo" -I +camlp4 -c xml_scene.mli
xml_scene.cmo: xml_scene.ml
	ocamlc $(OCAMLC_FLAGS) -pp "camlp4o pa_ioXML.cmo" -I +camlp4 -c xml_scene.ml
benchmark_xml: xml_scene.cmi xml_scene.cmo benchmark_xml.ml
	ocamlc -I +camlp4 ioXML.cmo xml_scene.cmo benchmark_xml.ml -o benchmark_xml

%.opt: %.ml
	ocamlopt $(OCAMLOPT_FLAGS) -o $@ $^
%.cmo: %.ml
	ocamlc $(OCAMLC_FLAGS) -c $<
%.cmx: %.ml
	ocamlopt $(OCAMLOPT_FLAGS) -c $<
%.cmi: %.mli
	ocamlc $(OCAMLC_FLAGS) -c $<
clean:
	rm -vf $(OBJ) $(XOBJ) raytracer.opt benchmark_ov.opt benchmark_xml
