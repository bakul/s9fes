# mkfile for Plan 9
# By Nils M Holm, 2008,2015; Ray Lai, 2014

</$objtype/mkfile

TARG=		s9
OFILES=		s9.$O s9core.$O plan9.$O s9-ffi.$O
CLEANFILES=	s9.image test.image
CFLAGS=		-FVw -Dplan9 '-DEXTENSIONS=sys_init();'
EXTRASCM=	-l ext/sys-plan9/plan9.scm -l ext/sys-plan9/plan9-tools.scm

s9dir=		/lib/s9fes

all:V: s9 s9.image help/apropos

tests:V: test realtest srtest libtest

s9:	$O.out
	cp $prereq $target

s9.image:	s9 s9.scm config.scm
	S9FES_LIBRARY_PATH=lib:contrib \
		./s9 -i - -l config.scm $EXTRASCM -d $target

plan9.$O: ext/sys-plan9/plan9.c
	$CC $CFLAGS -p -I `{pwd} ext/sys-plan9/plan9.c

s9-ffi.$O:	ext/sys-plan9/s9-ffi.c
	$CC $CFLAGS ext/sys-plan9/s9-ffi.c

help/apropos:
	rc util/fix-links.rc

libtest:V: s9 test.image
	ape/psh util/$target.sh

%test:V: s9 test.image util/%test.scm
	./s9 -i test -f util/$target.scm

test.image: s9 s9.scm
	./s9 -i - -d $target

inst: s9 s9.image
	mkdir -p $s9dir
	mkdir -p $s9dir/^($objtype lib ext contrib help)
	cp s9 /$objtype/bin/s9fes
	cp s9.image $s9dir/$objtype/s9.image
	cp s9.scm $s9dir/s9fes.scm
	cp lib/* $s9dir/lib
	cp ext/sys-plan9/*.scm $s9dir/ext
	cp contrib/* $s9dir/contrib
	{ x = `{pwd}; tar c help | { cd $s9dir; tar x }; cd $x }
	sed -e 's|^s9dir=.*|s9dir='$s9dir'|' <util/s9.rc >/rc/bin/s9
	chmod 755 /rc/bin/s9

deinst:
	rm -rf /lib/s9fes
	rm -f /rc/bin/s9 /$objtype/bin/s9fes

</sys/src/cmd/mkone
