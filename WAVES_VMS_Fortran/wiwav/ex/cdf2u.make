# cdf2u.make - for cdf2u.for
# execute this file by saying "make -f cdf2.make"
# Note that libcdf.so must be in LD_LIBRARY_PATH

ff = f77 -C -e -xl -PIC -lV77 -w 
#mylib = -L/home/cdf25-dist/lib -lcdf
#mylib = -lcdf
#mylib = -L/home/wind/lib -lcdf

cdf2u: cdf2u.o 
	$(ff) cdf2u.o -o cdf2u
#	$(ff) cdf2u.o $(mylib) -o cdf2u

cdf2u.o: cdf2u.for
	$(ff) -c cdf2u.for
