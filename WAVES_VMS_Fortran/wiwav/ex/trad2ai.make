# trad2ai.make - for trad2ai.for
# execute this file by saying "make -f trad2ai.make"

ff = f77 -C -e -xl -PIC -w -dalign -cg92 -Xlist
#libs = -lV77 -lsunmath 
libs = -L/usr/lang/SC3.0.1/lib -lV77 -lcx -lsunmath -lm -lansi 
wind_lib = -L/home/wind/lib_new -lWAVES

trad2ai: trad2ai.o 
	$(ff) trad2ai.o $(wind_lib) -o trad2ai $(libs)

trad2ai.o: trad2ai.for
	$(ff) -c trad2ai.for
