
.PHONY: all
all:
	cd src && ghc Main

.PHONY: clean
clean:
	rm -rf Main **/*.o **/*.hi *.class Prog.java src/Main
