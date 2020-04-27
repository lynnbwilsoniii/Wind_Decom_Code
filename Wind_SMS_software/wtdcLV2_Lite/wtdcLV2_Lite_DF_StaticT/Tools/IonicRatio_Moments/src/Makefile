#############################################################################
# Makefile for building: wtdcLV2_IonicRatio
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

HEADERS = wtdcLV2_Ion.h
SOURCES = wtdcLV2_IonTools.cpp \
		wtdcLV2_IonicRatio.cpp
OBJECTS = wtdcLV2_IonTools.o \
		wtdcLV2_IonicRatio.o
TARGET   = wtdcLV2_IonicRatio

####### Build Rules

all: wtdcLV2_IonicRatio

$(TARGET): $(OBJECTS)
	$(LINK) -o $(TARGET) $(OBJECTS) $(LIBS)
	
wtdcLV2_IonTools.o: wtdcLV2_IonTools.cpp wtdcLV2_Ion.h
	$(CXX) $(CXXFLAGS) $(INCPATH) wtdcLV2_IonTools.cpp

wtdcLV2_IonicRatio.o: wtdcLV2_IonicRatio.cpp wtdcLV2_Ion.h
	$(CXX) $(CXXFLAGS) $(INCPATH) wtdcLV2_IonicRatio.cpp
	
clean:
	$(DEL_FILE) $(OBJECTS)
	$(DEL_FILE) $(TARGET)	