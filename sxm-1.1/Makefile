
#
# This is GNU Makefile for SXM (CXEMbI)
#
# Copyright (c) 2000, 2001  Andrew Pochinsky, Sergei Egorov
#

#
# C-only version collects function tables via MKTABLE; 
# it can be built with the following lines:
CC        = gcc
CPPTABLES =

#
# Alternatively, if your compiler can do C++, use -DCPPTABLES to collect tables
# via C++ static constructors (there are no differences in functionality).
# To build C++ version, uncomment lines below
#CC        = g++
#CPPTABLES = CPPTABLES

INSTALL = install
CDEFS  = CS=CS_ANSI $(CPPTABLES) SXMPATH=\"$(INSTALL_ROOT)$(bindir)/$(engine)\"
CFLAGS = -Wall -pedantic -O2 $(CDEFS:%=-D%)

# destinations for installer
prefix=/usr/local
bindir=$(prefix)/bin
mandir=$(prefix)/man
manext=1
# used by RPM
#INSTALL_ROOT=

# names for things
engine=sxm
interp=sxi
manfile=sxm.man
tarfile=sxm-1.1.tar.gz


# phony targets
.PHONY: all exe image test clean realclean install dist nocr


#
# all the relevant files are listed here
#

sources  =  booleans.c \
            equal.c \
            pairs.c \
            lists.c \
            symbols.c \
            numbers.c \
            chars.c \
            strings.c \
            vectors.c \
            control.c \
            ports.c \
            input.c \
            output.c \
            system.c \
            gcells.c \
            locales.c \
            hash.c \
            htables.c \
            boxes.c \
            bvectors.c \
            records.c \
            types.c \
            chqueue.c \
            sxvm.c \
            sxcom.c \
            sxdisasm.c \
            sximage.c \
            sxinit.c \
            sxmain.c \
            sxmem.c \
            sxmath.c \
            sxmathread.c \
            sxintern.c \
            sxmisc.c \
            sxlocale.c \
            sxhash.c \
            sxhtab.c \
            sxerror.c \
            sxwrite.c \
            sxread.c \
            sxio.c \
            iosys.c \
            iostr.c \
            iofilstd.c \
            iottystd.c \
            optional/iocomp.c \
            optional/iopipe.c \
            optional/ioprocess.c \
            optional/dirent.c \
            optional/direct.c \
            optional/more.c \
            osunix.c

headers  =  sxm.h \
            extern.h \
            define.h \
            chqueue.h \
            handles.h \
            io.h \
            mem.h \
            os.h \
            sxdisasm.h \
            sxhash.h \
            sxhtab.h \
            sximage.h \
            sxintern.h \
            sxio.h \
            sxlocale.h \
            sxmath.h \
            sxmathread.h \
            sxread.h \
            sxwrite.h \
            unichar.h \
            vmop.h

tables  =   ntag.t \
            vmop.t

packages =  scheme/complib.ss \
            scheme/locales.ss \
            scheme/pretty.ss \
            scheme/engines.ss \
            scheme/cafe.ss \
            scheme/trace.ss \
            scheme/debug.ss \
            scheme/srfi.ss

documents = Readme \
            Copying \
            Install \
            History \
            Todo

#
# dependent definitions
#

objects = $(sources:.c=.o)


#
# executables
#

sxmexe = ./$(engine)
mkdocexe = ./mkdoc
mktableexe = ./mktable


#
# phony targets
#

exe: $(sxmexe)

all: exe image Formlist


#
# real targets
#

$(engine): $(objects)
	$(CC) $(CFLAGS) -o $@ $^ -lm

$(objects): %.o: %.c 
	$(CC) $(CFLAGS) -c -o $@ $<

sxinit.o: all.pt all.kt all.dt all.ibt all.vt

sxio.o: all.pct

$(objects): $(headers) $(tables)


#
# symbol tables
#

mktable: mktable.c
	$(CC) $(CFLAGS) -o $@ $^ -lm

all.pt: $(sources) $(mktableexe)
	cat $(sources) | $(mktableexe) PROCEDURE >$@

all.kt: $(sources) $(mktableexe)
	cat $(sources) | $(mktableexe) CONTINUATION >$@

all.dt: $(sources) $(mktableexe)
	cat $(sources) | $(mktableexe) DATUM >$@

all.ibt: $(sources) $(mktableexe)
	cat $(sources) | $(mktableexe) INITIAL_BINDING >$@

all.vt: $(sources) $(mktableexe)
	cat $(sources) | $(mktableexe) VARIABLE >$@

all.pct: $(sources) $(mktableexe)
	cat $(sources) | $(mktableexe) PORT_CLASS >$@


#
# heap image
#

image: $(interp)

sxm.cs: $(sxmexe) scheme/core.cs scheme/src2cs.ss scheme/sxm.src
	$(sxmexe) -e scheme/core.cs scheme/src2cs.ss <scheme/sxm.src >$@

$(interp): $(sxmexe) sxm.cs $(packages)
	$(sxmexe) -t -s $(interp) -e sxm.cs $(packages)


#
# test
#

test: $(interp) tests/r5rstest.ss tests/exit.ss
	$(sxmexe) -h $(interp) tests/r5rstest.ss tests/exit.ss


#
# installation
#

install: exe image $(engine).$(manext) $(interp).$(manext)
	$(INSTALL) -s -m 555 $(sxmexe) $(INSTALL_ROOT)$(bindir)
	$(INSTALL) -m 555 $(interp) $(INSTALL_ROOT)$(bindir)
	$(INSTALL) $(engine).$(manext) $(INSTALL_ROOT)$(mandir)/man$(manext)
	$(INSTALL) $(interp).$(manext) $(INSTALL_ROOT)$(mandir)/man$(manext)

$(engine).$(manext): $(manfile)
	sed 's=BINDIR=$(bindir)=g' $^ \
	 | sed 's=ENGINE=$(engine)=g' \
	 | sed 's=INTERP=$(interp)=g' >$@
	chmod +r $@

$(interp).$(manext): $(manfile) $(engine).$(manext)
	echo '.so man$(manext)/$(engine).$(manext)' > $@
	chmod +r $@


#
# list of top-level forms
#

mkdoc: mkdoc.c
	$(CC) $(CFLAGS) -o $@ $^ -lm

Formlist: $(mkdocexe) $(sources) scheme/sxm.src $(packages)
	cat $(sources) scheme/sxm.src $(packages) | $(mkdocexe) >$@


#
# cleanup
#

clean:
	$(RM) $(objects)
	$(RM) all.*
	$(RM) core *~ 
	$(RM) $(mktableexe) $(mkdocexe)
	$(RM) tmp?
	$(RM) $(engine).$(manext) $(interp).$(manext)

realclean: clean
	$(RM) $(interp) $(engine) sxm.cs Formlist


#
# remove CRs from source files (Win->Unix)
#

nocr: 
	newline -1sq "*.c" "*.h" "*.t" "*.ss" "*.cs" "*.src"
	newline -1sq Makefile mkfile $(documents) $(manfile) sxm.spec

#
# distributive
#

dist: realclean
	$(RM) $(tarfile)
	(dname=`pwd`; dname=`basename $$dname`; cd .. ; \
	 tar -cvf - $$dname | gzip -9 > $(tarfile))
	mv ../$(tarfile) .

