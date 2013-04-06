OCAMLC_FLAGS=-g -w A
OCAMLOPT_FLAGS=-w A
OBJ=scene.cmi scene.cmx scene.cmo  math.cmi math.cmx math.cmo raytracer.cmx raytracer.cmo \
    build_kdtree.cmx build_kdtree.cmo vm.cmo vm.cmx sample_shaders.cmo sample_shaders.cmx \
    sample_shaders.cmi vm.cmi shader.cmi shader_print.cmx shader_print.cmo shader_print.cmi \
    ast.cmi parser.ml parser.mli parser.cmi parser.cmo lexer.ml lexer.cmo ast.cmo rpuasm.cmo \
    shader_print_html.cmo shader_print_html.cmx \
    parser.cmx ast.cmx rpuasm.cmx lexer.cmx
# These are created as a side effect of the main actions
XOBJ=raytracer.cmi raytracer.o build_kdtree.cmi build_kdtree.o math.o scene.o sample_shaders.o vm.o shader_print.o \
     lexer.cmi main.cmi ast.cmi shader.cmi shader_print_html.o shader_print_html.cmi rpuasm.cmi \
     parser.o ast.o rpuasm.o lexer.o
TARGETS=raytracer.opt raytracer build_kdtree.opt build_kdtree sample_shaders.opt sample_shaders\
        shader_print.opt shader_print rpuasm.opt rpuasm shader_print_html.opt shader_print_html

all: raytracer.opt build_kdtree.opt sample_shaders.opt shader_print.opt rpuasm.opt shader_print_html.opt

raytracer: math.cmi math.cmo scene.cmi scene.cmo vm.cmo raytracer.cmo
	ocamlc $(OCAMLC_FLAGS) -o $@ math.cmo scene.cmo shader_print.cmo vm.cmo raytracer.cmo

raytracer.opt: math.cmi math.cmx scene.cmi scene.cmx vm.cmx raytracer.cmx
	ocamlopt $(OCAMLOPT_FLAGS) -o $@ math.cmx scene.cmx shader_print.cmx vm.cmx raytracer.cmx

build_kdtree: math.cmi math.cmo scene.cmi scene.cmo build_kdtree.cmo
	ocamlc $(OCAMLC_FLAGS) -o $@ math.cmo scene.cmo build_kdtree.cmo

build_kdtree.opt: math.cmi math.cmx scene.cmi scene.cmx build_kdtree.cmx
	ocamlopt $(OCAMLOPT_FLAGS) -o $@ math.cmx scene.cmx build_kdtree.cmx

sample_shaders.opt: shader.cmi sample_shaders.cmx
	ocamlopt $(OCAMLOPT_FLAGS) -o $@ sample_shaders.cmx

shader_print.opt: shader.cmi shader_print.cmx
	ocamlopt $(OCAMLOPT_FLAGS) -o $@ shader_print.cmx

shader_print_html.opt: shader.cmi shader_print_html.cmx
	ocamlopt $(OCAMLOPT_FLAGS) -o $@ shader_print_html.cmx

rpuasm: parser.cmo lexer.cmo ast.cmo rpuasm.cmo
	ocamlc $(OCAMLCFLAGS) -o $@ $^

rpuasm.opt: parser.cmx lexer.cmx ast.cmx rpuasm.cmx
	ocamlopt $(OCAMLCFLAGS) -o $@ $^

%.cmo: %.ml
	ocamlc $(OCAMLC_FLAGS) -c $<
%.cmx: %.ml
	ocamlopt $(OCAMLOPT_FLAGS) -c $<
%.cmi: %.mli
	ocamlc $(OCAMLC_FLAGS) -c $<

%.ml: %.mll
	ocamllex $<
%.mli %.ml: %.mly
	ocamlyacc $<

clean:
	rm -vf $(OBJ) $(XOBJ) $(TARGETS)

ast.cmo: ast.cmi 
ast.cmx: ast.cmi 
build_kdtree.cmo: scene.cmi math.cmi 
build_kdtree.cmx: scene.cmx math.cmx 
lexer.cmo: parser.cmi ast.cmi 
lexer.cmx: parser.cmx ast.cmx 
math.cmo: math.cmi 
math.cmx: math.cmi 
parser.cmo: ast.cmi parser.cmi 
parser.cmx: ast.cmx parser.cmi 
parser.cmi: ast.cmi 
raytracer.cmo: vm.cmo scene.cmi math.cmi 
raytracer.cmx: vm.cmx scene.cmx math.cmx 
rpuasm.cmo: shader.cmi parser.cmi lexer.cmo ast.cmi 
rpuasm.cmx: shader.cmi parser.cmx lexer.cmx ast.cmx 
sample_shaders.cmo: shader.cmi 
sample_shaders.cmx: shader.cmi 
scene.cmo: math.cmi scene.cmi 
scene.cmx: math.cmx scene.cmi 
scene.cmi: math.cmi 
shader_print_html.cmo: shader.cmi 
shader_print_html.cmx: shader.cmi 
shader_print.cmo: shader.cmi 
shader_print.cmx: shader.cmi 
vm.cmo: shader_print.cmo shader.cmi 
vm.cmx: shader_print.cmx shader.cmi 
