# description: libsms overall project makefile
# author: Jim Raines (jraines)
# created: 11Feb00 (split from libsms.mak)
#
# Note: this makefile calls other makefiles to do the actual compiling
# Note2: $(MAKE) is used for recursive make to allow the use of non-executing
# options to make and to cause those invocations to still do recursive makes
#
# See libsmsInit for further documentation.
#
# This file is controlled by the Concurent Version System (CVS):
# $Id: Makefile,v 1.11 2008/04/08 19:16:08 jonthom Exp $
#
# Modification history
#   automated incrementing of build number, version numbers now passed down
#   to subdir. makes, removes old ver. on install.  J. Raines, 23Feb00


##################################################################
# local variables -- note: shell and basename are make functions #
##################################################################
VERSIONFILE = ./libsms-version
# Set version from file '$(VERSIONFILE)', e.g. major.minor.build
FVERSION = $(shell cat $(VERSIONFILE))
# Major version only
MVERSION = $(basename $(basename $(FVERSION)))

PERL5LIB = /usr/lib/perl5/5.6.0/i386-linux/CORE
##PERL5LIB = /usr/lib/perl5/i386-linux/5.00405/CORE
###########################
# make internal variables #
###########################
MAKEFLAGS = -e FVERSION=$(FVERSION) MVERSION=$(MVERSION)

CFLAGS = -I$(PERL5LIB)

###########
# targets #
###########
.PHONY : libsms all clean dist floppy install pooh install-pooh

all:
	cd swig; $(MAKE)
	cd src; $(MAKE) all

pooh:
	cd src; $(MAKE) pooh \
	PERL5LIB=/usr/lib/perl5/i386-linux/5.00405/CORE

# remove .o files and rebuild from scratch
clean:
	cd swig; make clean
	cd src; make clean

# make source only distribution
DIST = dist/libsms
dist:
	# main stuff
	cp libsms.pm $(DIST)/
	cp libsms.html $(DIST)/
	# source
	cp src/*.c src/Makefile src/*.mak $(DIST)/src
	cp include/libsms.h $(DIST)/include
	ln -fs $(DIST)/include/libsms.h $(DIST)/libsms.h
	cd swig; \
	cp libsms.i libsms_wrap.c libsms-h2i.pl Makefile swig.mak $(DIST)/swig
	# examples -- copied by hand to keep them working
	# make tar ball
	cd $(DIST)/..; tar cvf libsms.tar libsms; gzip libsms.tar
# make a floppy 
floppy:
	make dist
	cp $(DIST)/../libsms.tar.gz /mnt/floppy

# install libsms files in /arch/wind/winddata (mounted as /home/winddata)
# and on website
INSTDIR = /home/winddata/sw/wind/libsms
WEBDIR  = 'pooh:/home/httpd/html/solar-heliospheric/intra/fiskgroup/'
install:
	# remove old version
	#$(RM) $(INSTDIR)/libsms.so*
	#$(RM) $(INSTDIR)/libsmsc.so*
	# perl library
	#cp libsms.so.$(FVERSION) $(INSTDIR)
	cd $(INSTDIR); ln -fs libsms.so.$(FVERSION) libsms.so
	# C library
	#cp libsmsc.so.$(FVERSION) $(INSTDIR)
	cd $(INSTDIR); ln -fs libsmsc.so.$(FVERSION) libsmsc.so
	#cp libsms.h $(INSTDIR)
	# documentation 
	#cp libsms.html $(INSTDIR)
	scp libsms.html $(WEBDIR)
	#cp libsms.pm $(INSTDIR)
	#cp libsms-version $(INSTDIR)
	cd $(INSTDIR); chmod g+rx *
	# examples (Perl)
	cd dist/examples/Perl; cp *.pl $(INSTDIR)/examples/Perl
	cd $(INSTDIR)/examples/Perl; chmod ug+rx *.pl
	# examples (C)
	cd dist/examples/C; cp *.c $(INSTDIR)/examples/C
	cd $(INSTDIR)/examples/C; chmod ug+rx *.c
	# increment build number in development directory
	perl tools/uv.pl $(FVERSION) > $(VERSIONFILE)



# ------ INSTALLATION ON POOH ONLY -----
# install libsms files in /arch/wind/winddata (mounted as /home/winddata)
# and on website
INSTDIRPOOH = /home/winddata/sw/wind/libsms/pooh
WEBDIR  = 'pooh:/home/httpd/html/solar-heliospheric/intra/fiskgroup/'
install-pooh:
	# remove old version
	$(RM) $(INSTDIRPOOH)/libsms.so*
	$(RM) $(INSTDIRPOOH)/libsmsc.so*
	# perl library
	cp libsms.so.$(FVERSION) $(INSTDIRPOOH)
	cd $(INSTDIRPOOH); ln -fs libsms.so.$(FVERSION) libsms.so
	# C library
	cp libsmsc.so.$(FVERSION) $(INSTDIRPOOH)
	cd $(INSTDIRPOOH); ln -fs libsmsc.so.$(FVERSION) libsmsc.so
	cp libsms.h $(INSTDIRPOOH)
	# documentation 
	cp libsms.html $(INSTDIRPOOH)
	scp libsms.html $(WEBDIR)
	cp libsms.pm $(INSTDIRPOOH)
	cp libsms-version $(INSTDIRPOOH)
	cd $(INSTDIRPOOH); chmod g+rx *
	# increment build number in development directory
	perl tools/uv.pl $(FVERSION) > $(VERSIONFILE)

# make symbolic links for ease of developing only (no other function)
links:
	ln -fs Makefile libsms.mak
	cd src; ln -fs Makefile libsms-src.mak
	cd swig; ln -fs Makefile swig.mak
