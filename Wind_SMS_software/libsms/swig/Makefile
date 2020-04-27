CPP = g++

all:
	$(MAKE) libsms
	
libsms: 
	$(CPP) check_processor.cc -g -o check_processor
	./check_processor
	
clean:
	$(MAKE) -f libsms_swig.mk clean