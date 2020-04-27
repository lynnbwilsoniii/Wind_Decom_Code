#############################################################################
# Makefile for building: wtdcLV2_IonicRatio_Supra
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

HEADERS = wtdcLV2_Ion_Supra.h
SOURCES = wtdcLV2_IonicRatio_SupraTools.cpp \
		wtdcLV2_IonicRatio_Suprathermal.cpp
OBJECTS = wtdcLV2_IonicRatio_SupraTools.o \
		wtdcLV2_IonicRatio_Suprathermal.o
TARGET   = wtdcLV2_IonicRatio_Supra

####### Build Rules

all: wtdcLV2_IonicRatio_Supra

$(TARGET): $(OBJECTS)
	$(LINK) -o $(TARGET) $(OBJECTS) $(LIBS)
	
wtdcLV2_IonicRatio_SupraTools.o: wtdcLV2_IonicRatio_SupraTools.cpp wtdcLV2_Ion_Supra.h
	$(CXX) $(CXXFLAGS) $(INCPATH) wtdcLV2_IonicRatio_SupraTools.cpp

wtdcLV2_IonicRatio_Suprathermal.o: wtdcLV2_IonicRatio_Suprathermal.cpp wtdcLV2_Ion_Supra.h
	$(CXX) $(CXXFLAGS) $(INCPATH) wtdcLV2_IonicRatio_Suprathermal.cpp
	
clean:
	$(DEL_FILE) $(OBJECTS)
	$(DEL_FILE) $(TARGET)	
