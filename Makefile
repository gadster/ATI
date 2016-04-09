OCB_FLAGS = -use-ocamlfind -j 8
OCB := ocamlbuild $(OCB_FLAGS)

all:
	$(OCB) all.otarget

%.native:
	$(OCB) "$@"

%.byte:
	$(OCB) "$@"

test:
	./position_test.native

clean:
	$(OCB) -clean
