.PHONY: test check

build:
	dune build

clean:
	dune clean	

code:
	-dune build
	code .
	! dune build --watch

game:
	OCAMLRUNPARAM=b dune exec finalproject/bin/main.exe	

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec finalproject/test/finalproject.exe

doc:
	dune build @doc