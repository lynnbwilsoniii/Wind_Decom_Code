# description: libsms/src  makefile
# author: Jim Raines (jraines)
# created: 7Oct99
#
# Note: $(MAKE) is used for recursive make to allow the use of non-executing
# options to make and to cause those invocations to still do recursive makes
#
# See libsmsInit for further documentation.
#
# This file is controlled by the Concurent Version System (CVS):
# $Id: Makefile,v 1.9 2005/11/29 19:22:11 jfeeman Exp $
#
# Modification history
#   automated incrementing of build number, version numbers now passed in
#   from top level make.  J. Raines, 23Feb00
#
#   Made include and library directories relative so old version could be
#   retrieved from CVS and built in place.  J. Raines, 5Apr00

###############################################
# variables to pass options to implicit rules #
###############################################
CC = gcc
CFLAGS = -I.. -g -fpic -DLIBSMSVERSION='"$(FVERSION)"'
LDFLAGS = -L..
LOADLIBES = -lsms -lm -lc

# should work to add this as a dep. for all .o but doesn't
# %.o : %.c libsms.h

#############
# variables #
#############
OBJECTS = smsOpenFile.o  smsReadCycle.o getFLR.o \
          lBuildWord.o dBuildWord.o getXDB.o fillStructEDB.o fillStructEDB_R_H.o \
	  smsDecompress.o smsTools.o getBits.o decodeEDBheader.o getDRH.o \
	  fixTcore.o decodeHDB.o decodeTPHA.o readRecord.o smsDecommHDB.o \
	  decodeXPHA.o smsCloseFile.o fixReadoutDelay.o libsmsInit.o \
	  checkEDBseq.o fTemperature.o decodeMPHA.o

# Version number defaults -- current values passed in from top level make
# Full version -- major.minor.build_number 
FVERSION = 1.0.0
# Major version only
MVERSION = 1

# SWIG stuff 
SWIG = ../swig

#########################################
# targets needing explicit dependencies #
#########################################
all:
	$(MAKE) libsms
	mv libsms.so.$(FVERSION) ..
	cd ..; ln -fs libsms.so.$(FVERSION) libsms.so
	mv libsmsc.so.$(FVERSION) ..
	cd ..; ln -fs libsmsc.so.$(FVERSION) libsmsc.so.$(MVERSION)
	cd ..; ln -fs libsmsc.so.$(FVERSION) libsmsc.so

pooh:
	$(MAKE) libsms
	mv libsms.so.$(FVERSION) ../pooh
	cd ../pooh; ln -fs libsms.so.$(FVERSION) libsms.so
	mv libsmsc.so.$(FVERSION) ../pooh
	cd ../pooh; ln -fs libsmsc.so.$(FVERSION) libsmsc.so.$(MVERSION)
	cd ../pooh; ln -fs libsmsc.so.$(FVERSION) libsmsc.so

libsms:
	$(MAKE) libsms.so
	$(MAKE) libsmsc.so

# build shared library
libsms.so: $(OBJECTS) $(SWIG)/libsms_wrap.o
	cd $(SWIG); $(MAKE) libsms_wrap.o # libsms_wrap.o dep. not quite right; # it won't rebuild libsms_wrap.c if  libsms.h changes 
	gcc -g -shared -Wl,-soname,$@ -o $@.$(FVERSION) $^ -lm -lc

# build shared library without swig/perl stuff for C usage
libsmsc.so: $(OBJECTS)
	gcc -g -shared -Wl,-soname,$@ -o $@.$(FVERSION) $^ -lm -lc

# build a static library (unused; could use for static-linked C programs)
libsms.a: $(OBJECTS) Makefile
	$(AR) crsv  $@ $(OBJECTS)

# build perl wrapper object
$(SWIG)/libsms_wrap.o: 
	cd $(SWIG); $(MAKE) libsms_wrap.o

# all objects depend on this header file (this works; 15Nov99)
$(OBJECTS): ../libsms.h

# rebuild when anything is changed because this contains the build date/time
libsmsInit.o: $(OBJECTS) libsms-src.mak

# this link is just to make id'ing buffers in emacs easier w/multiple Makefiles
libsms-src.mak: Makefile
	ln -fs Makefile libsms-src.mak

# phony targets
.PHONY : clean
# remove all object files, libraries (moved to ..) and their links then
# remake everything
clean:
	$(RM) $(OBJECTS)
	echo not removing libraries -- remove manually if necessary
