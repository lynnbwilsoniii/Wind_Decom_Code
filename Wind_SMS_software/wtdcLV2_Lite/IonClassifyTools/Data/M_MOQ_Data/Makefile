#############################################################################
# Makefile for building: wtdcLV2_Lite_DF_StaticT
#############################################################################

####### Compiler, tools and options
CC       = gcc
CXX      = g++
CFLAGS   = -c -Wall -W -O2 -g -fexceptions -fstack-protector --param=ssp-buffer-size=4
CXXFLAGS = -c -Wall -W -O2 -g -fexceptions -fstack-protector --param=ssp-buffer-size=4
INCPATH  = -I. -I/shrg1/wind/sw/wind/libsms
LINK     = g++
LIBS     = -lsmsc
DEL_FILE = rm -f

####### Files

SOURCES = wtLV2_mass_moq_yearly.cpp
OBJECTS = wtLV2_mass_moq_yearly.o
TARGET   = wtLV2_mass_moq_yearly

####### Build Rules

all: wtLV2_mass_moq_yearly

$(TARGET): $(OBJECTS)
	$(LINK) -o $(TARGET) $(OBJECTS) $(LIBS)
	
wtLV2_mass_moq_yearly.o: wtLV2_mass_moq_yearly.cpp
	$(CXX) $(CXXFLAGS) $(INCPATH) wtLV2_mass_moq_yearly.cpp
	
clean:
	$(DEL_FILE) $(OBJECTS)
	$(DEL_FILE) $(TARGET)	
