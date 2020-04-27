# libsms/swig/Makefile -- makefile for SWIG/Perl interface to libsms
# Jim Raines, 21Dec99
#
# See libsmsInit for further documentation.
#
# This file is controlled by the Concurent Version System (CVS):
# $Id: libsms_swig.mk,v 1.4 2012/05/16 17:31:19 jonthom Exp $
#
# Modification history


ifeq ($(BIT),32) 
	PERL5LIB = /usr/lib/perl5/5.6.0/i386-linux/CORE
	PERL5LIB_FC3 = /usr/lib/perl5/5.8.3/i386-linux-thread-multi/CORE
else
	PERL5LIB = /usr/lib64/perl5/5.8.8/i386-linux/CORE
	PERL5LIB_FC3 = /usr/lib64/perl5/5.8.8/x86_64-linux-thread-multi/CORE
endif

# define flags for implicit rules
CC = gcc 
CPP = g++
CFLAGS = -I$(PERL5LIB) -Dbool=char
CFLAGS_FC3 = -I$(PERL5LIB_FC3) -Dbool=char -D_GNU_SOURCE -fpic
LDFLAGS = 
LOADLIBES =

SWIG = swig
SWIG_1_1_883 = /shrg1/local/SWIG1.1-883/swig
SWIG_1_3_25 = /shrg1/local/SWIG-1.3.25/swig
SWIG_FC3 = $(SWIG_1_3_25)

# targets (default first)
libsms_wrap.o: libsms_wrap.c

FC3: libsms_wrap.c_fc3
	$(CC) $(CFLAGS_FC3) -c -o libsms_wrap.o libsms_wrap.c

# add ascii documentation when less builds are required
libsms_wrap.c:	libsms.i
	$(SWIG) -perl5 -dhtml libsms.i
	mv libsms.pm ..
	mv libsms_wrap.html ../libsms.html

libsms_wrap.c_fc3: libsms.i
	$(SWIG_FC3) -perl5 libsms.i
	$(CPP) swig_fix.cc -g -o swig_fix
	mv libsms.pm ..
	./swig_fix

libsms.i: ../libsms.h
	./libsms-h2i.pl ../libsms.h


# phony target to remove object files for clean make
.PHONY: clean
clean:
	$(RM) libsms_wrap.o libsms_wrap.c swig_fix

