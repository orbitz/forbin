CAMLP4=
OCAMLC_OPTS=-package unix,re,losic
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=
NON_LIB_MODULES=forbin.ml

BYTE_TARGETS=forbin.byte
NATIVE_TARGETS=forbin.native

.PHONY: all test

all: native-code byte-code

test:

# Dependencies
include ../Ocamlrules.mk.in
