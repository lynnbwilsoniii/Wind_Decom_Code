# description: libsms/tests makefile
# author: Jim Raines (jraines)
# created: 17Nov99 (under development)

###############################################
# variables to pass options to implicit rules #
###############################################
#SHELL = /bin/tcsh -f
CC = gcc
CFLAGS = -g -I/home/jraines/wind/libsms -DLIBSMSVERSION='"$(FVERSION)"'
LDFLAGS = -L/home/jraines/wind/libsms
LOADLIBES = -lsmsc -lm

MAKEFLAGS = -e FVERSION=$(FVERSION) MVERSION=$(MVERSION)

# set version
FVERSION = $(shell cat version)
MVERSION = $(basename $(basename $(FVERSION)))

###########################
# targets (default first) #
###########################
uv2:
	echo $(FVERSION)
	echo $(MVERSION)
uv:
	cd src; $(MAKE)
	perl uv.pl `cat version` > version

sctest: sctest.o # ../libsms.a

getbits: getbits.o ../getBits.o

