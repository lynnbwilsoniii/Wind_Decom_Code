#############################################################################
# Makefile for building: wtdcLV2_CSD_Supra
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

HEADERS = wtdcLV2_CSD_Supra.h
SOURCES = wtdcLV2_CSD_Supra_Tools.cpp \
		wtdcLV2_CSD_Supra.cpp
OBJECTS = wtdcLV2_CSD_Supra_Tools.o \
		wtdcLV2_CSD_Supra.o
TARGET   = wtdcLV2_CSD_Supra

####### Build Rules

all: wtdcLV2_CSD_Supra

$(TARGET): $(OBJECTS)
	$(LINK) -o $(TARGET) $(OBJECTS) $(LIBS)
	
wtdcLV2_CSD_Supra_Tools.o: wtdcLV2_CSD_Supra_Tools.cpp wtdcLV2_CSD_Supra.h
	$(CXX) $(CXXFLAGS) $(INCPATH) wtdcLV2_CSD_Supra_Tools.cpp

wtdcLV2_CSD_Supra.o: wtdcLV2_CSD_Supra.cpp wtdcLV2_CSD_Supra.h
	$(CXX) $(CXXFLAGS) $(INCPATH) wtdcLV2_CSD_Supra.cpp
	
clean:
	$(DEL_FILE) $(OBJECTS)
	$(DEL_FILE) $(TARGET)	