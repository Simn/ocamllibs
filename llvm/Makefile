OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC = llvm.mli llvm.ml llvm_analysis.mli llvm_analysis.ml llvm_ocaml.c analysis_ocaml.c

all: bytecode native

bytecode: llvm.cma

native: llvm.cmxa

llvm.cma: llvm_stubs.o llvm.ml llvm_analysis.ml
	$(OCAMLC) -a -o llvm.cma $(LIBS) llvm.ml llvm_analysis.ml

llvm.cmxa: llvm.mli llvm.ml llvm_analysis.mli llvm_analysis.ml llvm_stubs.o
	$(OCAMLC) llvm.mli llvm_analysis.mli
	$(OCAMLOPT) -a -o llvm.cmxa $(LIBS) llvm.ml llvm_analysis.ml

llvm_stubs.o: llvm_ocaml.c analysis_ocaml.c
	$(OCAMLC) -ccopt /arch:IA32 -I . $(CFLAGS) llvm_ocaml.c analysis_ocaml.c

clean:
	rm -f llvm.cma llvm.cmi llvm.cmx llvm.cmxa llvm.o llvm.obj llvm.lib llvm_stubs.obj llvm_stubs.o
	rm -f llvm.a libllvm.a libllvm.lib llvm.cmo
	rm -f llvm_analysis.cmi llvm_analysis.cmx llvm_analysis.obj llvm_ocaml.obj

.PHONY: all bytecode native clean
Makefile: ;
$(SRC): ;
