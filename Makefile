PREFIX=/usr/local
DOCS=BlaiseParse.dvi Compile.dvi BlaiseAst.dvi Main.dvi CompileBloopC.dvi

%.dvi: %.lhs
	lhs2TeX --poly $*.lhs > $*.tex
	latex $*
	rm -f $*.aux $*.tex

all: bloop

doc: $(DOCS)

bloop: *.hs *.lhs
	ghc --make -fglasgow-exts -o bloop Main

install: bloop
	install -d $(PREFIX)/bin
	install -d $(PREFIX)/lib/bloop
	install -d $(PREFIX)/share/doc/bloop
	install bloop $(PREFIX)/bin/
	install prelude.scm $(PREFIX)/lib/bloop/
	install prelude.h $(PREFIX)/lib/bloop/
	install README COPYING $(PREFIX)/share/doc/bloop/

clean:
	rm -f *.o *.hi bloop *~ \#* test

dist: clean
	DIR=`pwd | xargs basename` ;\
	ZIP=$$DIR-`date +%F-%H%M`.tar.gz ;\
	 ( cd ..; tar czvf $$ZIP $$DIR ; du -hs $$ZIP)


