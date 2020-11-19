all: 
	dune b @doc 
	cp -R _build/default/_doc/_html/* docs/ 

.PHONY: all

