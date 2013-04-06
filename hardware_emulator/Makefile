OCAMLMAKEFILE=OCamlMakefile

# Define all subprojects (actually the first few tests don't need them all)
TEST_COMMON=dynArray.ml math3.ml intersection_tests.ml memory.ml hardware.ml opcodes.ml vm.ml driver.ml

define PROJ_asm
  SOURCES=opcodes.ml asm_ast.ml asm_parser.mly asm_lexer.mll asm.ml
  RESULT=asm
endef

define PROJ_vm_function
  SOURCES=$(TEST_COMMON) vm_function.ml
  RESULT=vm_function
  LIBS=str
endef

define PROJ_vm_shader
  SOURCES=$(TEST_COMMON) vm_shader.ml
  RESULT=vm_shader
endef

define PROJ_build_kdtree
  SOURCES=dynArray.ml math3.ml build_kdtree.ml
  RESULT=build_kdtree
endef

define PROJ_build_kdtree_obj
  SOURCES=dynArray.ml math3.ml build_kdtree_obj.ml
  RESULT=build_kdtree_obj
endef

define PROJ_decompile_kdtree
  SOURCES=decompile_kdtree.ml
  RESULT=decompile_kdtree
endef

define PROJ_test1
  SOURCES=$(TEST_COMMON) test1.ml
  RESULT=test1
endef

define PROJ_test2
  SOURCES=$(TEST_COMMON) test2.ml
  RESULT=test2
endef

define PROJ_test3
  SOURCES=$(TEST_COMMON) test3.ml
  RESULT=test3
endef

define PROJ_test4
  SOURCES=$(TEST_COMMON) test4.ml
  RESULT=test4
endef

define PROJ_test5
  SOURCES=$(TEST_COMMON) test5.ml
  RESULT=test5
endef

define PROJ_test6
  SOURCES=$(TEST_COMMON) test6.ml
  RESULT=test6
endef

define PROJ_test7
  SOURCES=$(TEST_COMMON) test7.ml
  RESULT=test7
endef

define PROJ_test8
  SOURCES=$(TEST_COMMON) test8.ml
  RESULT=test8
endef

define PROJ_test9
  SOURCES=$(TEST_COMMON) test9.ml
  RESULT=test9
endef

export PROJ_asm PROJ_build_kdtree PROJ_build_kdtree_obj PROJ_decompile_kdtree
export PROJ_test1 PROJ_test2 PROJ_test3 PROJ_test4 PROJ_test5
export PROJ_test6 PROJ_test7 PROJ_test8 PROJ_test9 PROJ_vm_function PROJ_vm_shader

# If the environment does not define subprojects to handle,
# then use "ex1 ex2" as default
ifndef SUBPROJS
  export SUBPROJS=build_kdtree build_kdtree_obj decompile_kdtree asm test1 test2 test3 test4 test5 test6 test7 test8 test9 vm_function vm_shader
endif

all: native-code

# Tests and non-OCaml things

# Objects used in tests
edgar.raw: ./import_obj.pl ../scene/edgar_0.obj
	./import_obj.pl <../scene/edgar_0.obj >$@

edgar-compiled.dat: ./build_kdtree_obj edgar.raw
	./build_kdtree_obj -s edgar.raw -o $@

triangles.raw: ../scene/triangles.obj
	./import_obj.pl <../scene/triangles.obj >$@

triangles.dat: ./build_kdtree_obj triangles.raw
	./build_kdtree_obj -s triangles.raw -o $@

bunny-compiled.dat: ./build_kdtree ../scene/bunny2.dat
	./build_kdtree -s ../scene/bunny2.dat -o $@

depth.shr: ./asm depth.asm
	./asm -i depth.asm -o $@ 2>/dev/null

phong.shr:./asm phong.asm
	./asm -i phong.asm -o $@ 2>/dev/null

phong_trace.shr:./asm phong_trace.asm
	./asm -i phong_trace.asm -o $@ 2>/dev/null

phong_normal.shr:./asm phong_normal.asm
	./asm -i phong_normal.asm -o $@ 2>/dev/null

TEST_RESULTS= \
    test1.pnm \
    test2.pnm \
    test3_uv.pnm \
    test3_full.pnm \
    test3_closeup_bilinear.pnm \
    test3_closeup_ln.pnm \
    test3_closeup_nl.pnm \
    test3_closeup_nn.pnm \
    test3_closeup_bilinear_b.pnm \
    test3_closeup_ln_b.pnm \
    test3_closeup_nl_b.pnm \
    test3_closeup_nn_b.pnm \
    test3_closeup_bilinear_f4.pnm \
    test3_closeup_ln_f4.pnm \
    test3_closeup_nl_f4.pnm \
    test3_closeup_nn_f4.pnm \
    test4.pnm \
    test5.pnm \
    test6.pnm \
    test7.pnm \
    test8.pnm \
    test9.pnm

OTHER_OBJ=bunny-compiled.dat depth.shr phong.shr phong_trace.shr phong_normal.shr edgar.raw edgar-compiled.dat

cleantests:
	rm -f $(TEST_RESULTS)

cleanall: clean cleantests
	rm -f $(OTHER_OBJ)

# Catch-all target will be applied to all subprojects automatically
%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
