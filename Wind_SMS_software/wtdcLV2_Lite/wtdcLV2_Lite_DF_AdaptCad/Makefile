#############################################################################
# Makefile for building: wtdcLV2_Lite_DF_AdaptCad
#############################################################################

####### Compiler, tools and options
CC       = gcc
CXX      = g++
CFLAGS   = -c -Wall -W -O2 -g -fexceptions -fstack-protector --param=ssp-buffer-size=4
CXXFLAGS = -c -Wall -W -O2 -g -fexceptions -fstack-protector --param=ssp-buffer-size=4
INCPATH  = -I. -I/shrg1/wind/sw/wind/libsms -Isrc
LIB_PATH = -L/shrg1/local/lib/64b
LINK     = g++
LIBS     = -lsmsc
DEL_FILE = rm -f

####### Files

HEADERS = src/wtdcLV2_Lite.h
SOURCES = src/calcMoments.cpp \
		src/calcMomentsBySector.cpp \
		src/wtdcLV2_Lite_DF_adaptCad.cpp
OBJECTS = src/calcMoments.o \
		src/calcMomentsBySector.o \
		src/wtdcLV2_Lite_DF_adaptCad.o
TARGET   = wtdcLV2_Lite_DF_AdaptCad

####### Build Rules

all: wtdcLV2_Lite_DF_AdaptCad

$(TARGET): $(OBJECTS)
	$(LINK) $(LIB_PATH) -o $(TARGET) $(OBJECTS) $(LIBS)
	
src/calcMoments.o: src/calcMoments.cpp src/wtdcLV2_Lite.h
	$(CXX) $(CXXFLAGS) $(INCPATH) -o src/calcMoments.o src/calcMoments.cpp
	
src/calcMomentsBySector.o: src/calcMomentsBySector.cpp src/wtdcLV2_Lite.h
	$(CXX) $(CXXFLAGS) $(INCPATH) -o src/calcMomentsBySector.o src/calcMomentsBySector.cpp

src/wtdcLV2_Lite_DF_adaptCad.o: src/wtdcLV2_Lite_DF_adaptCad.cpp src/wtdcLV2_Lite.h
	$(CXX) $(CXXFLAGS) $(INCPATH) -o src/wtdcLV2_Lite_DF_adaptCad.o src/wtdcLV2_Lite_DF_adaptCad.cpp
	
clean:
	$(DEL_FILE) $(OBJECTS)
	$(DEL_FILE) $(TARGET)	
