# trad2a.make - for trad2a.for
# execute this file by saying "make -f trad2a.make"

ff = f77 -C -e -xl -PIC -w -dalign -cg92 -Xlist
#libs = -lV77 -lsunmath 
libs = -L/usr/lang/SC3.0.1/lib -lV77 -lcx -lsunmath -lm -lansi 
wind_lib = -L/home/wind/lib_new -lWAVES

trad2a: trad2a.o 
	$(ff) trad2a.o $(wind_lib) -o trad2a $(libs)

trad2a.o: trad2a.for
	$(ff) -c trad2a.for
