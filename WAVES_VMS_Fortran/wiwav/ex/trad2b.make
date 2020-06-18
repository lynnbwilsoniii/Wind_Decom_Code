# trad2b.make - for trad2b.for
# execute this file by saying "make -f trad2b.make"

ff = f77 -C -e -xl -PIC -lV77 -w 
wind_lib = -L/home/wind/lib_new -lWAVES
#wind_lib = -L/home/wind/lib -lWAVES
#wind_lib = /home/wind/src/*.o    

trad2b: trad2b.o 
	$(ff) trad2b.o $(wind_lib) -o trad2b

trad2b.o: trad2b.for
	$(ff) -c trad2b.for
