# t.make - for t.for
# execute this file by saying "make -f t.make"

ff = f77 -e 

t: t.o 
	$(ff) t.o -o t

t.o: t.for
	$(ff) -c t.for 
