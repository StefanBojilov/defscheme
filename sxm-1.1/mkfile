< /$objtype/mkfile

PCC		= pcc
CFLAGS	= -D'CS=CS_ANSI' -D'SXMPATH="/bin/sxm"' -D'_POSIX_SOURCE' -D_Plan9r3

engine=sxm
interp=sxi

objects = booleans.$O \
            equal.$O \
            pairs.$O \
            lists.$O \
            symbols.$O \
            numbers.$O \
            chars.$O \
            strings.$O \
            vectors.$O \
            control.$O \
            ports.$O \
            input.$O \
            output.$O \
            system.$O \
            gcells.$O \
            locales.$O \
            hash.$O \
            htables.$O \
            boxes.$O \
            bvectors.$O \
            records.$0 \
            types.$O \
            chqueue.$O \
            sxvm.$O \
            sxcom.$O \
            sxdisasm.$O \
            sximage.$O \
            sxinit.$O \
            sxmain.$O \
            sxmem.$O \
            sxmath.$O \
            sxmathread.$O \
            sxintern.$O \
            sxmisc.$O \
            sxlocale.$O \
            sxhash.$O \
            sxhtab.$O \
            sxerror.$O \
            sxwrite.$O \
            sxread.$O \
            sxio.$O \
            iosys.$O \
            iostr.$O \
            iofilstd.$O \
            iottystd.$O \
            optional/iocomp.$O \
            optional/iopipe.$O \
            optional/ioprocess.$O \
            optional/dirent.$O \
            optional/direct.$O \
            optional/more.$O \
            osplan9.$O

#sysobj =  os2plan9.$O


sources=`{echo $objects | sed 's/\.'$O'/.c/g'}


headers =   sxm.h \
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

#
# Dependent definitions
#
sxmexe=./$engine
mktableexe=./mktable

#################################################################################

all:V: exe image

clean:V:
	rm -f $objects
	rm -f all.*
	rm -f $mktableexe

nuke:V: clean
	rm -f $sxmexe $interp

install:V: exe image
	cp $sxmexe /$objtype/bin
	cp $interp /rc/bin
	sed < sxm.man 's.BINDIR./bin.g' | \
        sed 's.ENGINE.'^$engine'.g' | \
        sed 's.INTERP.'^$interp'.g' > /sys/man/1/$engine
	echo '.so /sys/man/1/'^$engine > /sys/man/1/$interp

est:V: $sxmexe $interp tests/r5rstest.ss tests/exit.ss
	$sxmexe -h $interp tests/r5rstest.ss tests/exit.ss


#################################################################################

exe:V: $sxmexe

$sxmexe: $objects
	$PCC $CFLAGS -o $target $prereq
	strip $target

#################################################################################
#$sysobj: `{echo $sysobj | sed 's/\.'$O'/.c/'}
#	$CC $CFLAGS -o $target $prereq

%.$O: %.c
	$PCC $CFLAGS -c -o $target $stem.c

sxinit.$O: all.pt all.kt all.dt all.ibt all.vt

sxio.$O: all.pct

$objects: $headers $tables

#
# symbol tables
#

$mktableexe: mktable.c
	$PCC $CFLAGS -o $target $prereq


all.pt: $sources $mktableexe
	cat $sources | $mktableexe PROCEDURE >$target

all.kt: $sources $mktableexe
	cat $sources | $mktableexe CONTINUATION >$target

all.dt: $sources $mktableexe
	cat $sources | $mktableexe DATUM >$target

all.ibt: $sources $mktableexe
	cat $sources | $mktableexe INITIAL_BINDING >$target

all.vt: $sources $mktableexe
	cat $sources | $mktableexe VARIABLE >$target

all.pct: $sources $mktableexe
	cat $sources | $mktableexe PORT_CLASS >$target

#
# heap image
#

image:V: $interp

$interp: $sxmexe sxm.cs $packages
	$sxmexe -t -s $interp -e sxm.cs $packages

sxm.cs: $sxmexe scheme/core.cs scheme/src2cs.ss scheme/sxm.src
	$sxmexe -e scheme/core.cs scheme/src2cs.ss <scheme/sxm.src >$target
