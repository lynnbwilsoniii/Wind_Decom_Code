#############################################################################
# Makefile for building: wtdcLV2_CSD_Moments
#############################################################################

####### Compiler, tools and options
CC       = gcc
CXX      = g++
CFLAGS   = -c -Wall -W -O2 -g -fexceptions -fstack-protector --param=ssp-buffer-size=4
CXXFLAGS = -c -Wall -W -O2 -g -fexceptions -fstack-protector --param=ssp-buffer-size=4
INCPATH  = -I.
LINK     = g++
LIBS     = 
DEL_FILE = rm -f

####### Files

HEADERS = wtdcLV2_CSD.h
SOURCES = wtdcLV2_CSD_Tools.cpp \
		wtdcLV2_CSD_Moments.cpp
OBJECTS = wtdcLV2_CSD_Tools.o \
		wtdcLV2_CSD_Moments.o
TARGET   = wtdcLV2_CSD_Moments

####### Build Rules

all: wtdcLV2_CSD_Moments

$(TARGET): $(OBJECTS)
	$(LINK) -o $(TARGET) $(OBJECTS) $(LIBS)
	
wtdcLV2_CSD_Tools.o: wtdcLV2_CSD_Tools.cpp wtdcLV2_CSD.h
	$(CXX) $(CXXFLAGS) $(INCPATH) wtdcLV2_CSD_Tools.cpp

wtdcLV2_CSD_Moments.o: wtdcLV2_CSD_Moments.cpp wtdcLV2_CSD.h
	$(CXX) $(CXXFLAGS) $(INCPATH) wtdcLV2_CSD_Moments.cpp
	
clean:
	$(DEL_FILE) $(OBJECTS)
	$(DEL_FILE) $(TARGET)	