# Scheme 9 from Empty Space
# Makefile (obviously)
# By Nils M Holm, 2007-2018
# In the public domain

# Change at least this line:
PREFIX= /u

# Base version and Release
BASE=		20181002
RELEASE=	20181028

# Override default compiler and flags
CC=	cc
CFLAGS=	-g -Wall -std=c99 -pedantic -O2

# Which OS are we using (unix or plan9)?
OSDEF=	-Dunix

# Uncomment these to include the Unix extensions
EXTRA_SCM+=	-l ext/sys-unix/unix.scm -l ext/sys-unix/unix-tools.scm
EXTRA_OBJS+=	unix.o
EXTRA_INIT+=	sys_init();
EXTRA_LIBS+=

# Uncomment these to include the Curses extensions
EXTRA_SCM+=	-l ext/curses/curses.scm
EXTRA_OBJS+=	curses.o
EXTRA_INIT+=	curs_init();
EXTRA_LIBS+=	-lncurses

# Uncomment these to include the CSV extensions
EXTRA_SCM+=	-l ext/csv/csv.scm
EXTRA_OBJS+=	csv.o
EXTRA_INIT+=	csv_init();
EXTRA_LIBS+=

# Options to be added to $(DEFS)
#	-DS9_BITS_PER_WORD_64	# 64-bit build (don't do this!)
#	-DLIBRARY_PATH="\"dir:...\""
#				# search path for LOCATE-FILE, etc
#	-DIMAGE_DIR="\"dir\""	# location of image file
#	-DNETWORK		# include socket code in the Unix extension
#	-DCURSES_COLOR		# enable the CURS:SET-COLOR primitive
#	-DCURSES_RESET		# automatically run CURS:ENDWIN on the REPL
#				# (requires the Curses extension)

DEFS=	$(OSDEF) \
	-DLIBRARY_PATH="\".:~/s9fes:$(S9DIR)\"" \
	-DIMAGE_DIR="\"$(S9DIR)\"" \
	-DEXTENSIONS="$(EXTRA_INIT)" \
	-DNETWORK \
	-DCURSES_COLOR \
	-DCURSES_RESET

# Where to install the stuff
S9DIR=	$(PREFIX)/share/s9fes
BINDIR=	$(PREFIX)/bin
INCDIR=	$(PREFIX)/include
LIBDIR=	$(PREFIX)/lib
MANDIR=	$(PREFIX)/man/man1

# Set up environment to be used during the build process
BUILD_ENV=	env S9FES_LIBRARY_PATH=.:lib:ext/sys-unix:ext/curses:ext/csv:contrib S9FES_IMAGE_DIR=.

SETPREFIX=	sed -e "s|^\#! /usr/local|\#! $(PREFIX)|"

default:	s9 s9.image s9.1.gz s9.1.txt libs9core.a help/apropos

all:	default

s9:	s9.o s9core.o $(EXTRA_OBJS)
	$(CC) -o s9 $(LDFLAGS) s9.o s9core.o $(EXTRA_OBJS) $(EXTRA_LIBS)

s9.o:	s9.c s9core.h s9import.h s9ext.h
	$(CC) -o s9.o $(CFLAGS) $(DEFS) -c s9.c

s9core.o:	s9core.c s9core.h
	$(CC) -o s9core.o $(CFLAGS) $(DEFS) -c s9core.c

s9.image:	s9 s9.scm ext/sys-unix/unix.scm ext/curses/curses.scm \
		ext/csv/csv.scm config.scm
	$(BUILD_ENV) ./s9 -i - $(EXTRA_SCM) -l config.scm -d s9.image

libs9core.a: s9core.o
	ar q libs9core.a s9core.o

help/apropos:
	sh util/fix-links.sh

s9.1.gz:	s9.1
	sed -e "s,@S9DIR@,$(S9DIR)," <s9.1 |gzip -9 >s9.1.gz

unix.o:	ext/sys-unix/unix.c s9core.h s9import.h s9ext.h
	$(CC) $(CFLAGS) $(DEFS) -I . -o unix.o -c ext/sys-unix/unix.c

curses.o:	ext/curses/curses.c s9core.h s9import.h s9ext.h
	$(CC) $(CFLAGS) $(DEFS) -I . -o curses.o -c ext/curses/curses.c

csv.o:	ext/csv/csv.c s9core.h s9import.h s9ext.h
	$(CC) $(CFLAGS) $(DEFS) -I . -o csv.o -c ext/csv/csv.c

lint:
	cc -g -Wall -ansi -pedantic -O3 s9.c s9core.c && rm a.out

test:	s9 test.image
	$(BUILD_ENV) ./s9 -i test.image util/test.scm

libtest:	s9 test.image
	$(BUILD_ENV) sh util/libtest.sh

systest:	s9 test.image s9.image
	$(BUILD_ENV) ./s9 -i test.image util/systest.scm

srtest:	s9 test.image
	$(BUILD_ENV) ./s9 -i test.image util/srtest.scm

realtest:	s9 test.image
	$(BUILD_ENV) ./s9 -i test.image util/realtest.scm

test.image:	s9 s9.scm
	$(BUILD_ENV) ./s9 -i - $(EXTRA_SCM) -d test.image

tests: test realtest srtest libtest systest

install:	install-s9 install-util

install-all:	install-s9 install-util install-progs

# old version of install(1) may need -c
#C=-c
install-s9:	s9 s9.scm s9.image s9.1.gz libs9core.a
	install -d -m 0755 $(S9DIR)
	install -d -m 0755 $(S9DIR)/help
	install -d -m 0755 $(S9DIR)/help/sys-unix
	install -d -m 0755 $(S9DIR)/help/curses
	install -d -m 0755 $(S9DIR)/help/csv
	install -d -m 0755 $(BINDIR)
	install -d -m 0755 $(LIBDIR)
	install -d -m 0755 $(INCDIR)
	install -d -m 0755 $(MANDIR)
	install $C -m 0755 s9 $(BINDIR)
	strip $(BINDIR)/s9
	install $C -m 0644 s9.scm $(S9DIR)
	install $C -m 0644 s9.image $(S9DIR)
	install $C -m 0644 lib/* $(S9DIR)
	install $C -m 0644 ext/sys-unix/*.scm $(S9DIR)
	install $C -m 0644 ext/curses/*.scm $(S9DIR)
	install $C -m 0644 ext/csv/*.scm $(S9DIR)
	install $C -m 0644 contrib/* $(S9DIR)
	install $C -m 0644 s9.1.gz $(MANDIR)
	(tar cf - help | tar xfC - $(S9DIR))
	install $C -m 0644 libs9core.a $(LIBDIR)
	install $C -m 0644 s9core.h $(INCDIR)
	install $C -m 0644 s9import.h $(INCDIR)

install-util:
	$(SETPREFIX) <prog/s9help.scm >$(BINDIR)/s9help
	$(SETPREFIX) <prog/s9resolve.scm >$(BINDIR)/s9resolve
	$(SETPREFIX) <prog/scm2html1.scm >$(BINDIR)/scm2html
	$(SETPREFIX) <prog/scmpp.scm >$(BINDIR)/scmpp
	-chmod +x $(BINDIR)/s9help	\
		  $(BINDIR)/s9resolve	\
		  $(BINDIR)/scm2html	\
		  $(BINDIR)/scmpp

install-progs:
	$(SETPREFIX) <prog/advgen.scm >$(BINDIR)/advgen
	$(SETPREFIX) <prog/c2html1.scm >$(BINDIR)/c2html
	$(SETPREFIX) <prog/cols.scm >$(BINDIR)/cols
	$(SETPREFIX) <prog/dupes.scm >$(BINDIR)/dupes
	$(SETPREFIX) <prog/edoc.scm.edoc >$(BINDIR)/edoc
	$(SETPREFIX) <prog/htmlify.scm >$(BINDIR)/htmlify
	$(SETPREFIX) <prog/s9hts.scm >$(BINDIR)/s9hts
	$(SETPREFIX) <prog/soccat.scm >$(BINDIR)/soccat
	-chmod +x $(BINDIR)/advgen	\
		  $(BINDIR)/c2html	\
		  $(BINDIR)/cols	\
		  $(BINDIR)/dupes	\
		  $(BINDIR)/edoc	\
		  $(BINDIR)/htmlify	\
		  $(BINDIR)/s9hts	\
		  $(BINDIR)/soccat

deinstall:	deinstall-s9 deinstall-util deinstall-progs

deinstall-s9:
	rm -f $(S9DIR)/help/* && rmdir $(S9DIR)/help
	rm -f $(S9DIR)/* && rmdir $(S9DIR)
	rm -f $(BINDIR)/s9
	-rmdir $(BINDIR)
	-rmdir $(MANDIR)

deinstall-util:
	rm -f $(BINDIR)/s9help		\
	      $(BINDIR)/s9resolve	\
	      $(BINDIR)/scm2html	\
	      $(BINDIR)/scmpp

deinstall-progs:
	rm -f $(BINDIR)/advgen		\
	      $(BINDIR)/c2html		\
	      $(BINDIR)/cols		\
	      $(BINDIR)/dupes		\
	      $(BINDIR)/edoc		\
	      $(BINDIR)/htmlify		\
	      $(BINDIR)/s9hts		\
	      $(BINDIR)/soccat

tabs:
	@find . -name \*.scm -exec grep -l "	" {} \;

cd:
	./s9 -i s9.image -f util/check-descr.scm

clean:
	rm -f s9 s9.image libs9core.a test.image s9.1.gz *.o *.core \
		CATEGORIES.html HACKING.html core s9fes-$(RELEASE).tgz \
		s9fes-$(BASE).tgz s9core-$(RELEASE).tgz __testfile__ \
		_meta _toc.tr _xref.tr _ndx.tr

new-version:
	vi Makefile s9.c HISTORY
	make s9.c

update-library:
	vi util/make-docs
	util/make-docs
	vi util/make-help-links \
		util/descriptions \
		util/categories.html
	cd help && s9 -f ../util/procedures.scm >INDEX
	@echo
	@echo "Now copy the new help pages from help-new to help"
	@echo "and run util/fix-links.sh"

s9.1.txt:	s9.1
	$(CC) -o rpp util/rpp.c
	nroff -c -mdoc s9.1 | ./rpp -a >s9.1.txt
	rm -f rpp

docs:	lib ext/sys-unix ext/sys-plan9 ext/curses ext/csv contrib
	util/make-docs
	mv -f help-new/sys-unix/* help/sys-unix
#	mv -f help-new/sys-plan9/* help/sys-plan9
	mv -f help-new/curses/* help/curses
#	mv -f help-new/csv/* help/csv
	rm help-new/sys-plan9/*
	rmdir help-new/sys-unix help-new/sys-plan9 help-new/curses help-new/csv
	mv -f help-new/* help
	rmdir help-new

webdump:
	util/make-html -r $(RELEASE)

advdump:	prog/advgen.scm prog/adventure.adv prog/adventure.intro
	sed -e 's/@dir/quest/' -e 's/@file/index/g' <util/pagehead >pagehead
	prog/advgen.scm -rv \
		-P terminal:session \
		-p pagehead \
		-e util/pagetail \
		-i prog/adventure.intro \
		-t "The Quest for S9fES" \
		-y ../t3x.css \
		prog/adventure.adv
	rm -f pagehead
	cp MASCOT.png advdump

csums:
	csum -u <_csums >_csums.new
	mv _csums.new _csums

mksums:	clean
	find . -type f | grep -v _csums | csum >_csums

dist:	clean s9.1.txt
	make clean
	cd .. && find s9 -type f | sort >s9/MANIFEST
	cd .. && \
		tar cfT - s9/MANIFEST | gzip -9 > s9fes-$(RELEASE).tgz && \
		mv s9fes-$(RELEASE).tgz s9
	ls -l s9fes-$(RELEASE).tgz | awk '{print int($$5/1024+.5)}'

cdist:
	tar cf - s9core.[ch] s9import.h s9core.txt README.s9core \
		| gzip -9 > s9core-$(RELEASE).tgz 

arc:	clean s9.1.txt
	cd .. && find s9 -type f | sort >s9/MANIFEST
	cd .. && tar cfT - s9/MANIFEST | gzip -9 > s9fes-$(BASE).tgz && \
		mv s9fes-$(BASE).tgz s9
	rm MANIFEST
	ls -l s9fes-$(BASE).tgz | awk '{print int($$5/1024+.5)}'
