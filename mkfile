# mkfile for Plan 9
# By Nils M Holm and contributors, 2008-2018

</$objtype/mkfile

TARG=		s9
OFILES=		s9.$O s9core.$O
CLEANFILES=	s9.image test.image
CFLAGS=		-FVw -Dplan9
EXTRASCM=

s9dir=		/lib/s9fes

all:V:	s9 s9.image

tests:V:	coretest test realtest srtest libtest

s9:	$O.out
	cp $prereq $target
	chmod +x $target

s9.image:	s9 s9.scm config.scm
	S9_IMAGE_DIR=. S9FES_LIBRARY_PATH=lib:contrib \
		./s9 -i - -l config.scm $EXTRASCM -d $target

coretest:V:	s9core.c s9core.h s9import.h
	$CC -Dplan9 -DTEST -o s9test.$O s9core.c
	$LD -o s9test s9test.$O
	./s9test
	rm -f s9test.$O s9test

libtest:V: s9 test.image
	ape/psh util/$target.sh

%test:V: s9 test.image util/%test.scm
	./s9 -i ./test -f util/$target.scm

test.image: s9 s9.scm
	./s9 -i - -d $target

inst: s9 s9.image
	mkdir -p $s9dir
	mkdir -p $s9dir/^($objtype lib ext contrib help)
	cp s9 /$objtype/bin/s9fes
	cp s9.image $s9dir/$objtype/s9.image
	cp s9.scm $s9dir/s9fes.scm
	cp lib/* $s9dir/lib
	cp contrib/* $s9dir/contrib
	{ x = `{pwd}; tar c help | { cd $s9dir; tar x }; cd $x }
	sed -e 's|^s9dir=.*|s9dir='$s9dir'|' <util/s9.rc >/rc/bin/s9
	chmod 755 /rc/bin/s9

deinst:
	rm -rf /lib/s9fes
	rm -f /rc/bin/s9 /$objtype/bin/s9fes

</sys/src/cmd/mkone
